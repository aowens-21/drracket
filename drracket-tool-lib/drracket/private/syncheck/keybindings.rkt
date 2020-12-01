#lang racket/base

(require racket/match
         racket/class)

(provide (all-defined-out))

;; Keybinding language code, will be moved eventually
(struct el-seq (e1 e2) #:prefab) ;; a sequence of el instructions
(struct el-while-step (condition body step-size step-type) #:prefab) ;; evaluates body until condition no longer holds or steps stop making progress, steps by step-size for each iteration
(struct el-insert (text) #:prefab) ;; insert some text at the current position (or replace current selection)
(struct el-insert-return (posn) #:prefab) ;; inserts a newline at the given position
(struct el-set-position (posn) #:prefab) ;; moves the cursor to the given position
(struct el-delete (p1 p2) #:prefab) ;; delete everything between two points
(struct el-seek (dist) #:prefab) ;; move some distance from the current position (or end position of selection)
(struct el-seek-while (condition step-size step-type) #:prefab) ;; seeks by step-size and type until condition no longer holds or we no longer make progress
(struct el-let (sym posn body) #:prefab) ;; binds the given symbol to a position in body
(struct el-set! (sym posn) #:prefab) ;; overwrites a binding inside a let body
(struct el-down-sexp (posn) #:prefab) ;; move into the next forward sexp from the given position
(struct el-up-sexp (posn) #:prefab) ;; exit the sexp at the given position
(struct el-forward-sexp (posn) #:prefab) ;; skip over the next forward sexp

(struct el-equal (lhs rhs) #:prefab) ;; check if two terms are equal (right now only supports reserved symbols or literal chars)
(struct el-or (b1 b2) #:prefab) ;; ORs together two boolean expressions
(struct el-and (b1 b2) #:prefab) ;; ANDs together two boolean expressions
(struct el-not (b) #:prefab) ;; negates a boolean (only equal for now)
(struct el-has-forward-sexp (posn) #:prefab) ;; returns true there is an sexp following the given position (relative to the sexp we are in)

(struct el-add (n1 n2) #:prefab) ;; adds two ints together
(struct el-sub (n1 n2) #:prefab) ;; subtracts one int from another
(struct el-int (n) #:prefab) ;; stores a literal int
(struct el-posn-lookup (sym) #:prefab) ;; looks up a reserved symbol that will be an editor position

(struct el-char (c) #:prefab) ;; stores a literal char, useful for checking what char we're on
(struct el-string (s) #:prefab) ;; stores a racket string
(struct el-text-lookup (sym) #:prefab) ;; looks up a reserved symbol that will be text
(struct el-get-forward-word (posn) #:prefab) ;; get the word starting at the given position

(define (el-stmt? e)
  (match e
    [(? el-while-step?) #t]
    [(? el-seek-while?) #t]
    [(? el-insert?) #t]
    [(? el-delete?) #t]
    [(? el-seek?) #t]
    [(? el-seq?) #t]
    [(? el-insert-return?) #t]
    [(? el-set-position?) #t]
    [(? el-let?) #t]
    [(? el-set!?) #t]
    [(? el-down-sexp?) #t]
    [(? el-up-sexp?) #t]
    [(? el-forward-sexp?) #t]
    [_ #f]))

(define (el-bool-expr? e)
  (match e
    [(? el-equal?) #t]
    [(? el-or?) #t]
    [(? el-and?) #t]
    [(? el-not?) #t]
    [(? el-has-forward-sexp?) #t]
    [_ #f]))

(define (el-num-expr? e)
  (match e
    [(? el-add?) #t]
    [(? el-sub?) #t]
    [(? el-int?) #t]
    [(? el-posn-lookup?) #t]
    [_ #f]))


(define (el-text-expr? e)
  (match e
    [(? el-text-lookup?) #t]
    [(? el-char?) #t]
    [(? el-string?) #t]
    [(? el-get-forward-word?) #t]
    [_ #f]))

;; Helper to make a seq out of a list of terms
(define (make-el-seq el-terms)
  (cond [(= (length el-terms) 1)
         (car el-terms)]
        [else `#s(el-seq ,(car el-terms)
                         ,(make-el-seq (cdr el-terms)))]))

(define (interp-stmt editor bindings stmt)
  (unless (el-stmt? stmt)
    (error "Expected el-stmt, got: " stmt))
  (match stmt
    [(el-seq e1 e2)
     (interp-stmt editor bindings e1)
     (interp-stmt editor bindings e2)]
    [(el-insert text) 
     (define insert-val (if (string? text)
                            text
                            (interp-text-expr editor bindings text)))
     (send editor insert insert-val)]
    [(el-delete p1 p2)
     (send editor delete (num-or-num-expr editor bindings p1) (num-or-num-expr editor bindings p2))]
    [(el-while-step condition body step-size step-type)
     ;; While will precompute iterations based on state of the editor before body execution
     ;; so body cannot change (and potentially make infinite) the number of iterations
     (define initial-pos (send editor get-end-position))
     (define num-of-iterations (let do-count ([count 0])
                                 (cond
                                   [(interp-bool-expr editor bindings condition)
                                    (process-step editor step-size step-type)
                                    (do-count (add1 count))]
                                   [else count])))
     (send editor set-position initial-pos)
     (for ([i (in-range num-of-iterations)])
       (define old-pos (send editor get-end-position))
       (interp-stmt editor bindings body)
       (send editor set-position old-pos)
       (process-step editor step-size step-type))]
    [(el-seek-while condition step-size step-type)
     (when (interp-bool-expr editor bindings condition)
       (define old-pos (send editor get-end-position))
       (process-step editor step-size step-type)
       (when (not (= old-pos (send editor get-end-position)))
         (interp-stmt editor bindings stmt)))]
    [(el-seek dist)
     (define dist-num (num-or-num-expr editor bindings dist))
     (define abs-dist (abs dist-num))
     (define direction (if (< dist-num 0) 'left 'right))
     (for ([i (in-range abs-dist)])
       (send editor move-position direction))]
    [(el-insert-return posn)
     (define old-start (send editor get-start-position))
     (define old-end (send editor get-end-position))
     (send editor set-position (num-or-num-expr editor bindings posn))
     (send editor insert-return)]
    [(el-set-position posn)
     (send editor set-position (num-or-num-expr editor bindings posn))]
    [(el-let sym posn body)
     (define position (interp-num-expr editor bindings posn))
     ;; Keep the old value and replace it after the let body has run so we have shadowing and we don't need
     ;; to copy the old hash table
     (cond [(hash-has-key? bindings sym)
            (define old-val (hash-ref bindings sym))
            (hash-set! bindings sym position)
            (interp-stmt editor bindings body)
            (hash-set! bindings sym old-val)]
           [else (hash-set! bindings sym position)
                 (interp-stmt editor bindings body)])]
    [(el-set! sym posn)
     (unless (hash-has-key? bindings sym)
       (error "Cannot assign variable before definition: " sym))
     (hash-set! bindings sym (interp-num-expr editor bindings posn))]
    [(el-down-sexp posn)
     (send editor down-sexp (interp-num-expr editor bindings posn))]
    [(el-up-sexp posn)
     (send editor up-sexp (interp-num-expr editor bindings posn))]
    [(el-forward-sexp posn)
     (send editor forward-sexp (interp-num-expr editor bindings posn))]))

;; Steps according to the text% move-position method, but also steps over sexps
(define (process-step editor size type)
  (unless (not (= size 0))
    (error 'interp "Step size for iteration must be non-zero"))
  (define direction (if (< size 0) 'left 'right))
  (define abs-size (abs size))
  (for ([i (in-range abs-size)])
    (case type
      ['sexp (if (equal? direction 'left)
                 (send editor backward-sexp (send editor get-end-position))
                 (send editor forward-sexp (send editor get-end-position)))]
      [else (send editor move-position direction #f type)])))

;; Helper to let various instructions take literal numbers or num-exprs
(define (num-or-num-expr editor bindings e)
  (if (number? e)
      e
      (interp-num-expr editor bindings e)))

(define (interp-bool-expr editor bindings expr)
  (unless (el-bool-expr? expr)
    (error "Expected el-bool-expr, got " expr))
  (match expr
    [(el-equal lhs rhs)
     (define (interp-side e)
       (cond [(el-num-expr? e)
              (interp-num-expr editor bindings e)]
             [(el-text-expr? e)
              (interp-text-expr editor bindings e)]
             [else (error "Expected el-num-expr or el-text-expr, got " e)]))
     (define l (interp-side lhs))
     (define r (interp-side rhs))
     (equal? l r)]
    [(el-and b1 b2) (and (interp-bool-expr editor bindings b1) (interp-bool-expr editor bindings b2))]
    [(el-or b1 b2) (or (interp-bool-expr editor bindings b1) (interp-bool-expr editor bindings b2))]
    [(el-not b) (not (interp-bool-expr editor bindings b))]
    [(el-has-forward-sexp posn)
     (if (send editor get-forward-sexp (interp-num-expr editor bindings posn)) #t #f)]))

(define (interp-num-expr editor bindings expr)
  (unless (el-num-expr? expr)
    (error "Expected el-num-expr, got " expr))
  (match expr
    [(el-add n1 n2) (+ (interp-num-expr editor bindings n1) (interp-num-expr editor bindings n2))]
    [(el-sub n1 n2) (- (interp-num-expr editor bindings n1) (interp-num-expr editor bindings n2))]
    [(el-int n) n]
    [(el-posn-lookup sym)
     (case sym
       ['sp (send editor get-start-position)]
       ['ep (send editor get-end-position)]
       ['last-posn (send editor last-position)]
       [else (if (hash-has-key? bindings sym)
                 (hash-ref bindings sym)
                 (error 'el-posn-lookup "Free identifier " sym))])]))

(define (interp-text-expr editor bindings expr)
  (unless (el-text-expr? expr)
    (error "Expected el-text-expr, got " expr))
  (match expr
    [(el-string s) s]
    [(el-char c) c]
    [(el-text-lookup sym)
     (case sym
       ['current-text
        (define sp (send editor get-start-position))
        (define ep (send editor get-end-position))
        (if (= sp ep)
            (send editor get-character sp)
            (send editor get-text sp ep))]
       [else (if (hash-has-key? bindings sym)
                 (hash-ref bindings sym)
                 (error 'el-text-lookup "Free identifier " sym))])]
    [(el-get-forward-word posn)
     (define old-start (send editor get-start-position))
     (define old-end (send editor get-end-position))
     (send editor set-position (interp-num-expr editor bindings posn))
     (let move-forward ()
       (send editor move-position 'right #f 'word)
       (if (or (= (send editor last-position) (send editor get-end-position))
               (not (char=? (send editor get-character (send editor get-end-position)) #\-)))
           (begin0 (send editor get-word-at (send editor get-end-position))
                   (send editor set-position old-start old-end))
           (move-forward)))]))
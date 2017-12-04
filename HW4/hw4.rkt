
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define ones (lambda () (cons 1 ones)))

;; put your code below

(define (sequence-wrong low high stride)
  (letrec ([f (lambda (l h s a)
          (if (> l h)
            a
            (f (+ l s) h s (cons l a))))
          ])
    (f low high stride null)))

; p1
; similar to python range function
(define (sequence l h s)
  (if (> l h)
  null
  (cons l (sequence (+ l s) h s))))

; p2
; apply a function to each of a list
(define (string-append-map xs s)
  (map (lambda (s1)
         (string-append s1 s)) xs))

; p3
; list-nth-mod
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [(= (remainder n (length xs)) 0) (car xs)]
        [(< n (length xs)) (list-nth-mod (cdr xs) (- n 1))]
        [#t (list-nth-mod xs (- n (length xs)))]))

; p4
; return a list containing first n elements of stream
(define (stream-for-n-steps s n)
  (if (= n 0)
  null
  (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; p5
; return a stream containing natural numbers with those 
; divisable by 5 negated
(define funny-number-stream 
  (letrec ([f (lambda (x) (cons 
              (if (= (remainder x 5) 0) 
                (- x) 
                x) 
              (lambda () (f (+ 1 x)))))])
  (lambda () (f 1))))

; p6
; generate dan and dog
(define dan-then-dog
  (letrec ([then-dog (lambda () (cons "dog.jpg" then-dan))]
           [then-dan (lambda () (cons "dan.jpg" then-dog))])
  then-dan))

; p7
; make a new stream, cons 0 to original stream element
(define (stream-add-zero s) 
  (letrec ([f (lambda (st) (cons 
              (cons 0 (car (st)))
              (lambda () (f (cdr (st))))))])
  (lambda () (f s))))
(define even-number-stream 
  (letrec ([f (lambda (x) (cons 
              x
              (lambda () (f (+ 2 x)))))])
  (lambda () (f 2))))
; (print (stream-for-n-steps (stream-add-zero even-number-stream) 12))

; p8
; cycle-lists

; (define (cycle-lists xs ys)
;   (letrec ([f (lambda (xss yss) (cons (cons (car xs) (car ys))
;                               (lambda ()  ) )
;               )])
;   (f xs ys))
; )

; p9
; assoc library function for vector
(define (vector-assoc v vec)
  (let ([length (if (vector? vec) (vector-length vec) 0)])
    (letrec ([va-sub (lambda (sub) 
      (if 
        (= sub length)
        #f
        (if (pair? (vector-ref vec sub))
          (if (equal? v (car (vector-ref vec sub)))
            (vector-ref vec sub)
            (va-sub (+ sub 1)))
          (va-sub (+ sub 1)))))])
    (va-sub 0))))

; p10
(define (cached-assoc xs n)
  (letrec (
    [memo (make-vector n)]
    [slot 0]
    [f (lambda (v) 
      (let ([ans (vector-assoc v memo)])
        (if ans
          ans
          (let ([new-ans (vector-assoc v xs)])
            (if new-ans
                (begin
                  (vector-set! memo slot new-ans)
                  (set! slot (remainder (+ 1 slot) n))
                  new-ans
                )
                new-ans
              )
          )
        )
      )
    )])
  f)
)

; p11
; define a while-less syntax
(define-syntax while-less
  (syntax-rules (do)
    [ 
      (while-less e1 do e2)
      (let ([t1 e1])
        (letrec (
          [loop (lambda (exp)
            (if (<= t1 exp)
              #t
              (loop e2)
            )
          )])
        (loop e2))
      )
    ]
  )
)


; grader output for 1st submission
; 
; while-less: Macro terminates with #t as result (do: bad syntax   in: do) [error]
; while-less: Evaluates e2 the correct number of times (do: bad syntax   in: do) [error]
; while-less: Evaluates e1 only once (do: bad syntax   in: do) [error]
; stream-add-zero: cons zero to the entire stream's elements (Result of (stream-for-n-steps-soln (stream-add-zero test-stream-of-evens) 12) was expected to equal '((0 . 2) (0 . 4) (0 . 6) (0 . 8) (0 . 10) (0 . 12) (0 . 14) (0 . 16) (0 . 18) (0 . 20) (0 . 22) (0 . 24))) [incorrect answer]
; stream-add-zero: cons zero to the stream's second element [incorrect answer]
; cycle-lists: Lists of uneven lengths (cycle-lists: undefined;  cannot reference undefined identifier) [error]
; cycle-lists: Multiple cycles through the lists (cycle-lists: undefined;  cannot reference undefined identifier) [error]
; cycle-lists: One cycle through the lists (cycle-lists: undefined;  cannot reference undefined identifier) [error]
; cycle-lists: Returns a stream (cycle-lists: undefined;  cannot reference undefined identifier) [error]
; vector-assoc: Lookup skips non-pairs (Result of (vector-assoc 5 (vector (quote blah) something (lambda () (quote blah)) (cons -5 3) (cons 5 2))) was expected to equal '(5 . 2)) [incorrect answer]
; cached-assoc: Full caching implementation (cached-assoc: undefined;  cannot reference undefined identifier) [error]
; cached-assoc: Uses vector-set! to add a new pair to the cache (cached-assoc: undefined;  cannot reference undefined identifier) [error]
; cached-assoc: Checks cache for answer first before using assoc (cached-assoc: undefined;  cannot reference undefined identifier) [error]
; cached-assoc: Creates a new vector of correct size filled with #f (cached-assoc: undefined;  cannot reference undefined identifier) [error]
; cached-assoc: Returns the same thing as assoc (cached-assoc: undefined;  cannot reference undefined identifier) [error]
; cached-assoc: Returns a procedure (cached-assoc: undefined;  cannot reference undefined identifier) [error]
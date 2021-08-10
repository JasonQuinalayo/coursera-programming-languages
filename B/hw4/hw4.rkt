
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high) 
      null 
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix) 
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n) 
  (let ([len (length xs)])
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(equal? len 0) (error "list-nth-mod: empty list")] 
          [(let ([remdr (remainder n len)]) 
             (car (list-tail xs remdr)))])))

(define (stream-for-n-steps s n)
  (if (equal? n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream 
  (letrec ([f (lambda(x) (if (equal? (remainder x 5) 0)
                             (cons (- x) (lambda () (f (+ x 1))))
                             (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog 
  (letrec ([f (lambda (x) (if (string=? x "dog.jpg")
                              (cons "dan.jpg" (lambda () (f "dan.jpg")))
                              (cons "dog.jpg" (lambda () (f "dog.jpg")))))]) 
    (lambda () (f "dog.jpg"))))

(define (stream-add-zero s) 
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys) 
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) 
                               (lambda () (f (+ n 1)))))]) 
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n) (if (equal? n (vector-length vec)) 
                              #f 
                              (let ([vref (vector-ref vec n)]) 
                                    (if (pair? vref)
                                        (if (equal? (car vref) v)
                                            vref
                                            (f (+ n 1)))
                                        (f (+ n 1))))))]) 
    (f 0)))

(define (cached-assoc xs n) 
  (letrec ([memo (make-vector n #f)]
           [cursor 0]
           [f (lambda (v) (let ([ans (vector-assoc v memo)]) 
                            (if ans 
                                ans 
                                (let ([ans2 (assoc v xs)]) 
                                  (if ans2
                                      (begin
                                        (vector-set! memo cursor ans2)
                                        (if (equal? cursor (- n 1))
                                            (set! cursor 0)
                                            (set! cursor (+ cursor 1)))
                                        ans2)
                                      #f)))))]) 
    f))

(define-syntax while-less 
  (syntax-rules (do) 
    [(while-less e1 do e2) 
     (letrec ([x e1]
              [f (lambda () (if (>= e2 x) #t (f)))]) 
       (f))]))

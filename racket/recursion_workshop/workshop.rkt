#lang racket
(require racket/trace)

(trace-define factorial
  (lambda (n)
    (if (= n 0) 1
        (* n (factorial (- n 1))))))

;; tail-recursive version
(trace-define fact-iter
  (lambda (n r)
    (if (= n 0) r
        (fact-iter (- n 1) (* n r)))))

(trace-define fact
              (lambda (n) (fact-iter n 1)))

;; continuation-passing style
(define factk
  (lambda (n k)
    (if (= n 0) (k 1)
        (factk (- n 1) (lambda (r) (k (* n r)))))))

;; Fibonacci
;; fib(0) = 0
;; fib(1) = 1
;; fib(n) = fib(n-1) + fib(n-2) otherwise


;; direct-style (non tail-recursive)
(trace-define fib
  (lambda (n)
    (if (= n 0) 0
        (if (= n 1) 1 (+ (fib(- n 1)) (fib(- n 2)))))))

(define fib-acc
 (lambda (n a b)
   (if (= n 0) a
       (if (= n 1) b
           (fib-acc (- n 1) b (+ a b))))))

(define fib2
  (lambda (n)
    (fib-acc n 0 1)))

(define fibk
  (lambda (n k)
    (if (= n 0) (k 0)
        (if (= n 1) (k 1)
            (fibk (- n 2)
                  (lambda (r2)
                    (fibk (- n 1)
                          (lambda (r1)
                            (k (+ r1 r2))))))))))

(define length
  (lambda (xs)
    (if (null? xs) 0 (+ 1 (length (rest xs))))))

(define sum
  (lambda (xs)
    (if (null? xs) 0 (+ (first xs) (sum (rest xs))))))

(define sum-tree
  (lambda (xs)
    (if (null? xs) 0
        (if (list? (first xs))
            (+ (sum-tree (first xs)) (sum-tree (rest xs)))
            (+ (first xs) (sum-tree (rest xs)))))))

;; write a recursive function that can work over lists or trees
;; and squares all the numbers in nested lists
;; (square-list '(3 4 5)) => '(9 16 25)
;; (square-tree '((3) (4 5)))) => '((9) (16 25))

(define square-list
  (lambda (xs)
    (if (null? xs) '() (cons (* (first xs) (first xs)) (square-list (rest xs))))))

(define square-tree
  (lambda (xs)
    (if (null? xs) '()
        (if (list? (first xs))
            (cons (square-tree (first xs)) (square-tree (rest xs)))
            (cons (* (first xs) (first xs)) (square-tree (rest xs)))))))

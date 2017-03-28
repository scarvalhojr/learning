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
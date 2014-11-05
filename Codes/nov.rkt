#lang racket

(define is-odd?
  (lambda (X)
    (cond
     (= (quotient x 2) 0)
     else
     (#f))))




(define (is-odd? x)
  (cond
   ((= (quotient x 2) 0) #t)
   (else #f)))
; test
(is-odd? 999)

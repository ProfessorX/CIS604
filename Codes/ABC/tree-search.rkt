;;; Tree search in racket
;; 20141025 Lab 15:05

; Imperative
; <imperative>
(define (bfs ? root)
  (let/ec return
    (define q (list root))
    (while (not (empty? q))
      (define cur (first q))
      (set! q (rest q))
      (cond
       [(? (node-data cur))
        (return cur)]
       [else
        (set! q (append q (node-children cur)))]))
    #f))


; node definitio
; <cons-node>
(define node-data car)
(define node-children cdr)
(define example-tree
  '(0
    (1
     (3
      (7)
      (8))
     (4
      (9)
      (10)))
    (2
     (5
      (11)
      (12))
     (6
      (13)
      (14)))))

; <example-tests>
(check-false
 (bfs (curry = 15) example-tree))
(check-equal?
 (bfs (curry = 11) example-tree)
 '(11))


<functional>
(define (bfs ? root)
  (define (bfs/queue q)
    (cond
     [(empty? q) #f]
     [else (match-define (cons cur next-q) q)
           (cond
            [(? (node-data cur))
             cur]
            [else
             (bfs/queue
              (append next-q (node-children cur)))])]))
  (bfs/queue (list root)))

<simple-imperative>
(define (bfs ? root)
  (define q (make-queue))
  (define (bfs/queue)
    (cond
     [(queue-empty? q)
      #f]
     [else
      (define cur (dequeue! q))
      (cond
       [(? (node-data cur))
        cur]
       [else
        (for-each (curry enqueue! q)
                  (node-children cur))
        (bfs/queue)])]))
  (enqueue! q root)
  (bfs/queue))

; simpler imperative
(define (bfs ? root)
  (define q (make-queue))
  (enqueue! q root)
  (for/or ([cur in-queue q])
    (cond
     [(? (node-data cur))
      cur]
     [else
      (for-each (curry enqueue! q)
                (node-children cur))
      #f])))

; binary code
(define (node-data x) x)
(define (node-children x)
  (list (+ (* 2 x) 0)
        (+ (* 2 x) 1)))

; binary tests
(check-equal?
 (bfs (curry = 11) 1)
 '11)

; binary test infinite
; Do NOT try this at home
(check-false
 (bfs (curry = 0) 1))  






<*>::=
(require racket/list
         rackunit
         racket/function
         racket/match
         data/queue)

(define-syntax-rule (while cond body ...)
  (let loop ()
    (when cond
      body ...
      (loop))))


; It seems that <> is not a valid syntax in "pure" rkt codes.

(let ()
  <cons-node>
  (let ()
    <imperative>
    <example-tests>))

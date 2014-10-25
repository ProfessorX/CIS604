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

(let ()
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
  (let ()
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
    (check-false
     (bfs (curry = 15) example-tree))
    (check-equal?
     (bfs (curry = 11) example-tree)
     '(11))))


; Okay you newbie noob, something about rackunit. 
(check-true 1)
(check-not-false #f)

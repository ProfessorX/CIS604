(let ((x (amb 3 4 5))
      (y (amb 6 7 8 )))
  (assert (= (+ x y) 12))
  (display x)
  (display y))  
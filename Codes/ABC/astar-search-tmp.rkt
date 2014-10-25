; map display
(define map-scale 15)
(define (type-color ty)
  (match type-color
    [0 "yellow"]
    [1 "green"]
    [2 "red"]))

(define (cell-square ty)
  (square (map-scale "solid" (type-color ty))))
(define (row-image M row)
  (apply beside
         (for/list ([col-matrix (in-range (matrix-num-cols M))])
           (cell-square (matrix-ref M row col)))))
(define (map-image M)
  (apply above
         (for/list ([row-matrix (in-range (matrix-num-rows M))])
           (row-image M row))))


; path display line
(define (edge-image-on e i)
  (match-define (map-edge (map-node _ sx sy) _ _ (map-node _ dx dy)) e)
  (add-line i
            (* (+ sy 0.5) map-scale) (* (+ sx 0.5) map-scale)
            (* (+ dy 0.5) map-scale) (* (+ dx 0.5) map-scale)
            "black"))

(define (path-image M path)
  (foldr edge-image-on (map-image M) path))


(define-runtime-path map-image.png "tmp/astar-map.png")
(save-image (map-image random-M) map-image.png)

(define-runtime-path path-image.png "tmp/astar-path.png")
(save-image (path-image random-M random-path) path-image.png)

(define (present? ele l)
  (cond [(null? l) #f]
        [(equal? (car l) ele) #t]
        [else (present? ele (cdr l))]))

(define (init-board x colour)
  (begin
    (vector-set! board x colour)
    (vector-set! board (+ x 17) colour)
    (vector-set! board (+ x 18) colour)
    (vector-set! board (+ x 34) colour)
    (vector-set! board (+ x 35) colour)
    (vector-set! board (+ x 36) colour)
    (vector-set! board (+ x 51) colour)
    (vector-set! board (+ x 52) colour)
    (vector-set! board (+ x 53) colour)
    (vector-set! board (+ x 54) colour)))

(define (init-board-inv x colour)
  (begin
    (vector-set! board x colour)
    (vector-set! board (- x 17) colour)
    (vector-set! board (- x 18) colour)
    (vector-set! board (- x 34) colour)
    (vector-set! board (- x 35) colour)
    (vector-set! board (- x 36) colour)
    (vector-set! board (- x 51) colour)
    (vector-set! board (- x 52) colour)
    (vector-set! board (- x 53) colour)
    (vector-set! board (- x 54) colour)))

(define (init)
  (define (set-unoccupied)
    (define (range-set-h i f)
      (if (< i f)
          (begin (vector-set! board i 'u)
                 (range-set-h (+ i 1) f))
          (vector-set! board f 'u)))
    (begin
      (range-set-h 72 76)
      (range-set-h 89 94)
      (range-set-h 106 112)
      (range-set-h 123 130)
      (range-set-h 140 148)
      (range-set-h 158 165)
      (range-set-h 176 182)
      (range-set-h 194 199)
      (range-set-h 212 216)))
  (define (init-vlocs)
    (define (init-tri vlocs x)
      (begin
        (vector-set! vlocs 0 x)
        (vector-set! vlocs 1 (+ x 17))
        (vector-set! vlocs 2 (+ x 18))
        (vector-set! vlocs 3 (+ x 34))
        (vector-set! vlocs 4 (+ x 35))
        (vector-set! vlocs 5 (+ x 36))
        (vector-set! vlocs 6 (+ x 51))
        (vector-set! vlocs 7 (+ x 52))
        (vector-set! vlocs 8 (+ x 53))
        (vector-set! vlocs 9 (+ x 54))))
    (define (init-tri-inv vlocs x)
      (begin
        (vector-set! vlocs 0  x)
        (vector-set! vlocs 1 (- x 17))
        (vector-set! vlocs 2 (- x 18))
        (vector-set! vlocs 3 (- x 34))
        (vector-set! vlocs 4 (- x 35))
        (vector-set! vlocs 5 (- x 36))
        (vector-set! vlocs 6 (- x 51))
        (vector-set! vlocs 7 (- x 52))
        (vector-set! vlocs 8 (- x 53))
        (vector-set! vlocs 9 (- x 54))))
    (begin (init-tri locs-r 4)
           (init-tri locs-b 157)
           (init-tri locs-p 166)
           (init-tri-inv locs-o 284)
           (init-tri-inv locs-y 122)
           (init-tri-inv locs-g 131)
           (init-tri wset-o 4)
           (init-tri wset-g 157)
           (init-tri wset-y 166)
           (init-tri-inv wset-r 284)
           (init-tri-inv wset-p 122)
           (init-tri-inv wset-b 131)))
  (begin 
    (set! last-player-colour (list-ref chosen-players (- (length chosen-players) 1)))
    (set-unoccupied)
    (if (present? 'p chosen-players)
        (init-board 166 'p) (init-board 166 'u))
    (if (present? 'y chosen-players)
        (init-board-inv 122 'y) (init-board-inv 122 'u))
    (if (present? 'g chosen-players)
        (init-board-inv 131 'g) (init-board-inv 131 'u))
    (if (present? 'b chosen-players)
        (init-board 157 'b) (init-board 157 'u))
    (set! moves 0)
    (init-board 4 'r)
    (init-board-inv 284 'o)
    (init-vlocs)))

(define (is-present? ele l)
  (cond [(null? l) #f]
        [(= (car l) ele) #t]
        [else (is-present? ele (cdr l))]))

(define (vlocs colour) ; vector of locations of all pieces of same colour
  (cond [(equal? colour 'o) locs-o] ; locs-colour is the general name of such vectors
        [(equal? colour 'r) locs-r]
        [(equal? colour 'p) locs-p]
        [(equal? colour 'y) locs-y]
        [(equal? colour 'g) locs-g]
        [(equal? colour 'b) locs-b]))

(define (wset colour)
  (cond [(equal? colour 'o) wset-o]
        [(equal? colour 'r) wset-r]
        [(equal? colour 'p) wset-p]
        [(equal? colour 'y) wset-y]
        [(equal? colour 'g) wset-g]
        [(equal? colour 'b) wset-b]))

; used for testing without a gui, absolutely redundant now
(define (display-board)
  (define (display-blank n)
    (if (= n 1) (display " ")
        (begin (display " ")
               (display-blank (- n 1)))))
  (define (display-board-h i)
    (if (< i 17)
        (begin (display-blank (- 17 i))
               (display (vector-drop (vector-take board (* i 17)) (* (- i 1) 17)))
               (display "\n") (display-board-h (+ i 1)))
        (display (vector-drop (vector-take board (* i 17)) (* (- i 1) 17)))))
  (display-board-h 1))
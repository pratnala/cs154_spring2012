(define (centre-coord tilenum) ; returns the coordinates of the centre of the tile corresponding to tilenum
  (let ([y (quotient tilenum 17)])
    (cons (floor (* scale (+ init-x (* (sqrt 3) (remainder tilenum 17)) (* (- 0 (/ (sqrt 3) 2)) y))))
          (floor (+ (* scale (+ init-y (* 1.5 y))) (/ ydiff 2) 3.5)))))

(define (tilenum coordpair) ; returns the tilenumber corresponding to a coordinate pair
  (define (nearest fr i)
    (cond [(<= (abs (- i fr)) 0.2) i]
          [(> i 289) void]
          [else (nearest fr (+ i 1))]))
  (define (approx coords)
    (let* ([row (floor (/ (- (cdr coords) (* scale init-y) 3.5) ydiff))]
           [xpix-start_element (car (centre-coord (* row 17)))]
           [column (floor (/ (- (car coords) xpix-start_element -12.12) xdiff))])
      (cons (+ xpix-start_element (* xdiff column))
            (floor (+ 3.5 (* scale init-y) (* ydiff (+ 0.5 row)))))))
  (let* ([centre-coord (approx coordpair)]
         [y (/ (- (/ (- (cdr centre-coord) (/ ydiff 2) 3.5) scale) init-y) 1.5)]
         [x (/ (- (/ (car centre-coord) scale) init-x (* (/ (sqrt 3) -2) y)) (sqrt 3))])
    (nearest (+ x (* 17 y)) 0)))
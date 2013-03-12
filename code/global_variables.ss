(define board (make-vector 289 'i))

(define locs-o (make-vector 10 'jk))
(define locs-b (make-vector 10 'jk))
(define locs-y (make-vector 10 'jk))
(define locs-r (make-vector 10 'jk))
(define locs-g (make-vector 10 'jk))
(define locs-p (make-vector 10 'jk))
(define wset-o (make-vector 10 'jk))
(define wset-b (make-vector 10 'jk))
(define wset-y (make-vector 10 'jk))
(define wset-r (make-vector 10 'jk))
(define wset-g (make-vector 10 'jk))
(define wset-p (make-vector 10 'jk))

(define scale 14)
(define xdiff (* 24.25 (/ scale 14))) (define ydiff (* 21 (/ scale 14)))
(define init-x 8) (define init-y 3)

(define chosen-players (list 'r 'o)) (define turn 'x)

(define 1click #f) (define selected 0)

(define last-move (cons 0 0)) (define last-player-colour 'x)

(define comp1 #f) (define comp2 #f) (define moves 0)

(define root3 1.7320508075688772) (define -0.5root3 -0.8660254037844386)
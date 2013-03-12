; checks whether a movement from initial to final is a legal move or not
(define (valid-slide? init final)
  (cond [(not (and (> final 0) (< final 288))) #f]
        [(not (and (not (equal? 'i (vector-ref board final))) (equal? 'u (vector-ref board final)))) #f]
        [(let ([diff (abs (- final init))]) (or (= 1 diff) (= 17 diff) (= 18 diff))) #t]
        [else #f]))

; returns a list of all valid final destinations from the 'initial' tile given the current state of the board
(define (valid-slide init)
  (define (next-slide diff)
    (cond [(= diff -17) 1]
          [(= diff 1) 18]
          [(= diff 18) 17]
          [(= diff 17) -1]
          [(= diff -1) -18]
          [(= diff -18) -17]))
  (define (valid-slide-h diff)
    (if (= diff -18)
        (if (valid-slide? init (+ init diff))
            (cons (+ init diff) '()) '())
        (if (valid-slide? init (+ init diff))
            (cons (+ init diff) (valid-slide-h (next-slide diff)))
            (valid-slide-h (next-slide diff)))))
  (valid-slide-h -17))

(define (valid-slide-length+ init) ; the same as valid-slide, except each result if cons-ed with the move-length, which in the case of a slide is always 1
  (map (lambda (new-pos) (cons new-pos 1)) (valid-slide init)))

(define (valid-single-hop? init final) ; checks if a hop from 'initial' tilenumber to 'final' tilenumber is legal or not
  (let ([diff (- final init)])
    (if (and (> final 0) (< final 288) (not (equal? 'i (vector-ref board final)))
             (equal? 'u (vector-ref board final)) (even? diff))
        (let ([adiff (abs diff)]
              [piece (vector-ref board (+ init (/ diff 2)))])
          (cond [(and (not (equal? 'u piece)) (= 2 adiff)) #t]
                [(and (not (equal? 'u piece)) (= 34 adiff)) #t]
                [(and (not (equal? 'u piece)) (= 36 adiff)) #t]
                [else #f]))
        #f)))

; returns a list of all legal single hops from the 'initial' tile
; a single hop means a hop in which only one tile if hopped over
(define (valid-single-hop init)
  (define (next-hop diff)
    (cond [(= diff -34) 2]
          [(= diff 2) 36]
          [(= diff 36) 34]
          [(= diff 34) -2]
          [(= diff -2) -36]
          [(= diff -36) -34]))
  (define (valid-single-hop-h diff)
    (if (= diff -36)
        (if (valid-single-hop? init (+ init diff))
            (cons (+ init diff) '()) '())
        (if (valid-single-hop? init (+ init diff))
            (cons (+ init diff) (valid-single-hop-h (next-hop diff)))
            (valid-single-hop-h (next-hop diff)))))
  (valid-single-hop-h -34))

; the same as valid-single-hop, except that the move-length is cons-ed with each element of the list
(define (valid-single-hop-length+ init depth)
  (map (lambda (new-pos) (cons new-pos depth)) (valid-single-hop init)))

(define (concat l)
  (foldr append '() l))

(define (drop-prev l elist)
  (define (dph list ele)
    (cond [(null? list) '()]
          [(= (caar list) ele) (cdr list)]
          [else (cons (car list) (dph (cdr list) ele))]))
  (if (null? elist) l
      (drop-prev (dph l (car elist)) (cdr elist))))

; returns a list of all valid hops from the 'initial' tile
(define (valid-hop init)
  (map (lambda (x) (car x)) (valid-hop-length+ init)))

; returns the consing of each element of the list in valid-hop with the move-length
(define (valid-hop-length+ init)
  (define noback '())
  (define (valid-hop-h init depth)
    (let* ([lsingle (drop-prev (valid-single-hop-length+ init depth) noback)]
           [lsingle-car (map (lambda (x) (car x)) lsingle)])
      (if (null? lsingle) '()
          (begin
            (set! noback (remove-duplicates (append lsingle-car noback)))
            (append lsingle (concat (map (lambda (new-init-length+)
                                           (let ([new-init (car new-init-length+)]
                                                 [colour (vector-ref board init)])
                                             (begin
                                               (vector-set! board new-init colour)
                                               (vector-set! board init 'u)
                                               (let ([next-hops (valid-hop-h new-init (+ depth 1))])
                                                 (begin 
                                                   (vector-set! board new-init 'u)
                                                   (vector-set! board init colour)
                                                   next-hops)))))
                                         lsingle)))))))
  (begin (set! noback (list init))
         (valid-hop-h init 1)))

; checks if a hop from the inital to the final tiles is legal or not
(define (valid-hop? init final)
  (cond [(valid-single-hop? init final) #t]
        [(is-present? final (valid-hop init)) #t]
        [else #f]))

; returns a list of all valid moves from a given tile
(define (valid-moves init)
  (append (valid-hop init) (valid-slide init )))

; analogous to the other 'length+' functions
(define (valid-moves-length+ init)
  (append (valid-hop-length+ init) (valid-slide-length+ init)))

; checks if a move between init and final is legal or not
(define (valid-move? init final)
  (cond [(valid-hop? init final) #t]
        [(valid-slide? init final) #t]
        [else #f]))

(define (all-moves colour) ; all moves for each piece of the same colour
  (define temp-vlocs (vlocs colour))
  (define (all-moves-h i)
    (let ([init (vector-ref temp-vlocs i)])
      (if (= i 9)
          (cons (map (lambda (x) (cons init x))
                     (valid-moves-length+ init)) '())
          (cons (map (lambda (x) (cons init x))
                     (valid-moves-length+ init)) (all-moves-h (+ i 1))))))
  (foldr append '() (filter (lambda (x) (not (null? x))) (all-moves-h 0)))) ; concat

(define (all-moves-length- colour) ; all moves for each piece of the same colour
  (define temp-vlocs (vlocs colour))
  (define (all-moves-h i)
    (let ([init (vector-ref temp-vlocs i)])
      (if (= i 9)
          (cons (map (lambda (x) (cons init x))
                     (valid-moves init)) '())
          (cons (map (lambda (x) (cons init x))
                     (valid-moves init)) (all-moves-h (+ i 1))))))
  (foldr append '() (filter (lambda (x) (not (null? x))) (all-moves-h 0)))) ; concat

#|
(define (all-longest-moves colour) ; all possible longest moves
  (define (pmax l)
    (cond [(null? l) 'error]
          [(null? (cdr l)) (cons (car l) '())]
          [(> (cddar l) (cddadr l)) (pmax (cons (car l) (cddr l)))]
          [(< (cddar l) (cddadr l)) (pmax (cdr l))]
          [(= (cddar l) (cddadr l)) (cons (car l) (pmax (cdr l)))]))
  (pmax (all-moves colour)))
|#

;checks if any player has won
(define (check-win)
  (define (wins? colour)
    (equal? (sort (vector->list (vlocs colour)) <)
            (sort (vector->list (wset colour)) <)))
  (define (check-win-h players)
    (cond [(null? players) #f]
          [(wins? (car players)) #t]
          [else (check-win-h (cdr players))]))
  (check-win-h chosen-players))

;executes a move
(define (exec-move init final)
  (define (vreplace vec init final)
    (vector-map! (lambda (x) (if (= x init) final x)) vec))
  (let ([colour (vector-ref board init)])
    (cond [(and (valid-move? init final) (not (equal? colour 'u)) (not (equal? colour 'i)))
           (begin (vector-set! board final colour) (vreplace (vlocs colour) init final)
                  (vector-set! board init 'u) (send board_canvas refresh-now) (set! moves (+ moves 1))
                  (cond [(check-win) (begin (send msg set-label (string-append (text turn) " won in "
                                                                               (number->string moves) " moves!"))
                                            (set! turn 'x))]
                        [else (cycle)]))])))

(define (force-move init final)
  (define (vreplace vec init final)
    (vector-map! (lambda (x) (if (= x init) final x)) vec))
  (let ([colour (vector-ref board init)])
    (begin (vector-set! board final colour)
           (vreplace (vlocs colour) init final)
           (vector-set! board init 'u))))
;frame in which the canvas is placed
(define frame (new frame% [label "Chinese Checkers"] [width 434] [height 550]))

;display pane for the list of choices
(define hp1 (new horizontal-pane% (parent frame) (stretchable-height #f)))

(define hp2 (new horizontal-pane% (parent frame) (stretchable-height #f)))

(define hp3 (new horizontal-pane% (parent frame) (stretchable-height #f)))

(new button% (parent hp3) (label "Play") (callback (lambda (button event) (start-game) (comp-play))))

(new radio-box% [parent hp1] [label "Player 1"] [choices (list "Human" "MinMax2" "AlphaBeta3" "Random")] [style '(horizontal)]
     (callback (lambda (radiobox event)
                 (let ([choice (send radiobox get-selection)])
                   (cond [(= choice 0) (set! comp1 #f)]
                         [(= choice 1) (begin (set! comp1 #t) (set! play-comp1 (lambda () (minimax 'r 2))))]
                         [(= choice 2) (begin (set! comp1 #t) (set! play-comp1 (lambda () (alpha-beta 'r 3))))]
                         [(= choice 3) (begin (set! comp1 #t) (set! play-comp1 (lambda () (rand-move 'r + <))))])
                   (start-game)))))

(new message% [parent hp3] [label "Human-Only Players "])

(new check-box% [parent hp3] [label "3 and 4"]
     (callback (lambda (checkbox event)
                 (if (send checkbox get-value)
                     (begin (set! chosen-players (append chosen-players (list 'p)))
                            (set! chosen-players (append chosen-players (list 'y))))
                     (begin (delete-colour 'y)
                            (delete-colour 'p)))
                 (start-game))))

(new radio-box% [parent hp2] [label "Player 2"] [choices (list "Human" "AlphaBeta2" "AlphaBeta3" "Random")] [style '(horizontal)]
     (callback (lambda (radiobox event)
                 (let ([choice (send radiobox get-selection)])
                   (cond [(= choice 0) (set! comp2 #f)]
                         [(= choice 1) (begin (set! comp2 #t) (set! play-comp2 (lambda () (alpha-beta 'o 2))))]
                         [(= choice 2) (begin (set! comp2 #t) (set! play-comp2 (lambda () (alpha-beta 'o 3))))]
                         [(= choice 3) (begin (set! comp2 #t) (set! play-comp2 (lambda () (rand-move 'o - >))))])
                   (start-game)))))

(new check-box% [parent hp3] [label "5 and 6"]
     (callback (lambda (checkbox event)
                 (if (send checkbox get-value)
                     (begin (set! chosen-players (append chosen-players (list 'g)))
                            (set! chosen-players (append chosen-players (list 'b))))
                     (begin (delete-colour 'b)
                            (delete-colour 'g)))
                 (start-game))))

;undo button
(new button% (parent hp3) (label "Undo") (callback (lambda (button event) (undo))))

;the canvas% object on which we play the game
(define my-canvas%
  (class canvas%
    (define (is? type tilenum)
      (equal? type (vector-ref board tilenum)))
    (define/override (on-event event)
      (let* ([xclick (send event get-x)]
             [yclick (send event get-y)]
             [tilenumber (tilenum (cons xclick yclick))]) ; current click tilenumber
        (cond [(send event button-down? 'left)
               (if (number? tilenumber)
                   (begin (cond [(and (not 1click) (is? turn tilenumber))
                                 (begin (set! 1click tilenumber) (set! selected tilenumber)
                                        (send board_canvas refresh-now))]
                                [(and 1click (is? 'u tilenumber) (is? turn 1click))
                                 (begin (set! selected 0)
                                        (exec-move 1click (tilenum (cons xclick yclick)))
                                        (set! last-move (cons 1click (tilenum (cons xclick yclick))))
                                        (set! 1click #f))]
                                [(and 1click (is? turn tilenumber))
                                 (begin (set! selected tilenumber) (set! 1click tilenumber)
                                        (send board_canvas refresh-now))]
                                [else (begin (set! selected tilenumber)
                                             (send board_canvas refresh-now)
                                             (set! 1click #f))]))
                   (cond [1click (begin (set! 1click #f) (set! selected tilenumber)
                                        (send board_canvas refresh-now))]))])))
    (super-new)))

(define board_canvas
  (new my-canvas% [parent frame]
       [paint-callback
        (lambda (canvas dc)
          (send dc set-scale scale scale) (send dc set-pen "black" 0 'solid)
          (send dc set-smoothing 'smoothed) (draw-the-board dc))]))

;display pane for the winner, for the current turn and for error messages
(define hp4 (new horizontal-pane% (parent frame) (stretchable-height #f)))

(define msg (new message% [parent hp4] [label "Red (Player 1) Plays Now"] [min-width 200]))

(define (draw-the-board dc) ; draws the board
  (define (find-colour-name tilenum) ; returns the colour corresponding to tilenum
    (let ([sym (vector-ref board tilenum)])
      (cond [(equal? tilenum selected) (lighter-colour-name tilenum)]
            [(equal? sym 'o) "orange"] [(equal? sym 'r) "red"]
            [(equal? sym 'y) "yellow"] [(equal? sym 'g) "green"]
            [(equal? sym 'b) "royalblue"] [(equal? sym 'p) "purple"]
            [(equal? sym 'u) "lightgray"] [(equal? sym 'i) "white"]
            [else (error "unidentified tile.")])))
  (define (lighter-colour-name tilenum) ; returns the colour for a selected tile
    (let ([sym (vector-ref board tilenum)])
      (cond [(equal? sym 'o) "chocolate"] [(equal? sym 'r) "lightcoral"]
            [(equal? sym 'y) "peru"] [(equal? sym 'g) "darkgreen"]
            [(equal? sym 'b) "skyblue"] [(equal? sym 'p) "plum"]
            [(equal? sym 'u) "ivory"] [(equal? sym 'i) "white"]
            [else (error "unidentified tile.")])))
  (define relative-coords ; relative coordinates of a hexagon
    (list (cons 0 0) (cons (/ (sqrt 3) 2) 0.5) (cons (/ (sqrt 3) 2) 1.5)
          (cons 0 2) (cons (/ (sqrt 3) -2) 1.5) (cons (/ (sqrt 3) -2) 0.5)))
  (define (draw-hexagon x-offset y-offset dc tile-number) ; draws a single hexagon
    (let ([hexcolour (find-colour-name tile-number)])
      (cond [(not (equal? hexcolour "white"))
             (begin (send dc set-brush hexcolour 'opaque)
                    (send dc draw-polygon relative-coords x-offset y-offset 'odd-even))])))
  (define (draw-row-number y dc)
    (define (draw-row-helper x)
      (if (< x 0) void
          (begin (draw-row-helper (- x 1))
                 (draw-hexagon (+ init-x (* root3 x) (* -0.5root3 y))
                               (+ init-y (* 1.5 y)) dc (+ x (* 17 y))))))
    (draw-row-helper 16))
  (vector-map (lambda (x) (draw-row-number x dc)) (build-vector 17 (lambda (x) x))))

(send board_canvas set-canvas-background (make-object color% 240 240 240))

(define (delete-colour colour)
  (define (delete-colour-h l)
    (cond [(null? l) void]
          [(equal? colour (car l)) (cdr l)]
          [else (cons (car l) (delete-colour-h (cdr l)))]))
  (set! chosen-players (delete-colour-h chosen-players)))

(define (start-game)
  (begin (set! turn 'r) (init)
         (send board_canvas refresh-now)))

(define (comp-play)
  (cond [comp1 (play-comp1)]))

(define (undo)
  (cond [(or (equal? last-move (cons 0 0))
             (equal? turn 'x)) (send msg set-label "Cannot undo anything right now!")]
        [(let ([last-played (cycle-hof (reverse chosen-players))])
           (and (or (equal? 'r last-played) (equal? 'o last-played))
                (or comp1 comp2))) (send msg set-label "Cannot undo computer's move!")]
        [else (begin (force-move (cdr last-move) (car last-move))
                     (set! last-move (cons 0 0)) (set! moves (- moves 1))
                     (send board_canvas refresh-now) (cycle-back))]))

(define (cycle-hof constl)
  (define (cycle-helper l)
    (cond  ;[(null? l) (error "oops! something went wrong with cycling of turns!")]
      [(null? (cdr l)) (car constl)]
      [(equal? turn (car l)) (cadr l)]
      [else (cycle-helper (cdr l))]))
  (cycle-helper constl))

(define (text turn)
  (cond [(equal? turn 'o) "Orange (Player 2)"]
        [(equal? turn 'r) "Red (Player 1)"]
        [(equal? turn 'p) "Purple (Player 3)"]
        [(equal? turn 'b) "Blue (Player 6)"]
        [(equal? turn 'g) "Green (Player 5)"]
        [(equal? turn 'y) "Yellow (Player 4)"]))

(define (cycle)
  (begin (set! turn (cycle-hof chosen-players))
         (send msg set-label (string-append (text turn) " Plays Now"))
         (cond [(and (equal? 'r turn) comp1) (play-comp1)]
               [(and (equal? 'o turn) comp2) (play-comp2)])))

(define (cycle-back)
  (begin (set! turn (cycle-hof (reverse chosen-players)))
         (send msg set-label (string-append (text turn) " Plays Now"))))

(define (play-comp1)
  (error "AI 1 Not Set"))

(define (play-comp2)
  (error "AI 2 Not Set"))

(define (open-game)
  (begin (init) (start-game) (send frame show #t)))
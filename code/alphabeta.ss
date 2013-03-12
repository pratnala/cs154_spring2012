;evaluation function
(define (evaluate vlocs-self vlocs-opp colour)
  ;evaluation for a single tile
  (define (evaluate-tile tilenum)
    ;the relative importance given to vertical displacement over horizontal displacement
    ;weight-factor is inversely correlated with vertical displacement of tilenum
    (define (weight-factor tilenum)
      (+ 0.75 (left-behind tilenum)))
    (define (left-behind tilenum)
      (- 1.5 (* 0.03125 (vertical-displacement tilenum)))) 
    ;returns the horizontal displacement of a tile
    (define (horizontal-displacement tilenum)
      ;returns the central tile if the row is an odd number, else returns
      ;the average of the two central tilenums when the row is even-numbered
      ;the first row is row number zero
      (define (central-tile row-num)
        (let ([r (* 17.5 row-num)]) (+ 4 r)))
      (let ([c (central-tile (quotient tilenum 17))])
        (floor (abs (- tilenum c)))))
    ;returns the vertical displacement of a tile
    ;(quotient tilenum 17) is merely the row number
    (define (vertical-displacement tilenum)
      (if (vector-member tilenum vlocs-self)
          (cond [(equal? colour 'o) (- 16 (quotient tilenum 17))]
                [(equal? colour 'r) (quotient tilenum 17)])
          (cond [(equal? colour 'r) (- 16 (quotient tilenum 17))]
                [(equal? colour 'o) (quotient tilenum 17)])))
    ;evaluation for a single tile
    (begin
      (- (* (weight-factor tilenum) (vertical-displacement tilenum))
         (abs (horizontal-displacement tilenum)))))
  ;evaluation-helper returns the sum of evaluations for all tiles
  (define (evaluation-helper vlocs)
    (foldr (lambda (fst acc) (+ (evaluate-tile fst) acc)) 0 vlocs))
  ;evaluation for a colour takes into account both the vlocs of colour
  ;as well as opponent colour
  (- (evaluation-helper (vector->list vlocs-self))
     (evaluation-helper (vector->list vlocs-opp))))

;returns the colour of the opponent
(define (opponent colour)
  (cond [(equal? colour 'o) 'r]
        [(equal? colour 'r) 'o]))

;returns vlocs of the colour of the opponent
;vlocs = vector of locations
(define (opponent-vlocs colour)
  (vlocs (opponent colour)))

;the structure we use for the tree
(define-struct node (subnodes vlocs-self vlocs-opp type) #:transparent)

;finds the first point of difference between two lists and conses them
;assumes the lists are of equal length
;in our code, they always will be!
(define (find-diff l1 l2)
  (if (= (car l1) (car l2))
      (find-diff (cdr l1) (cdr l2))
      (cons (car l1) (car l2))))

;MINIMAX
;returns a consing of two tilenums
;the car is the tilenum to be moved
;the cdr is the destination
;executes the move
(define (minimax colour ply)
  ;higher order function used to define max2, max and min
  (define (absolute op l val) ; op '>' for max
    (cond [(null? l) val]
          [(op val (car l)) (absolute op (cdr l) val)]
          [else (absolute op (cdr l) (car l))]))
  ;returns the max of the car's of a list
  (define (max2 l) (absolute (lambda (x y) (> (car x) (car y))) l (car l)))
  ;returns the max of a list
  (define (max l) (absolute > l (car l)))
  ;returns the min of a list
  (define (min l) (absolute < l (car l)))
  ;returns the evaluation that satisfies the minimax algorithm for a given node
  (define (minimax-h node)
    (cond [(null? (node-subnodes node))
           ;evaluation number for whichever player's hypothetical turn it is
           ;if the node is 'max it's the AI's turn, else it's its opponent's turn
           (evaluate (node-vlocs-self node) (node-vlocs-opp node)
                     (if (equal? 'max (node-type node)) colour (opponent colour)))]
          [(equal? (node-type node) 'max)
           (max (map minimax-h (node-subnodes node)))]
          [(equal? (node-type node) 'min)
           (min (map minimax-h (node-subnodes node)))]
          [else (error "?2?")]))
  (define (minimax-root node) ;expects only a node of 'max type
    (cond [(equal? (node-type node) 'max)
           (let* ([eval.play (map (lambda (x) (cons (minimax-h x) (node-vlocs-opp x))) ; self becomes opp!
                                  (node-subnodes node))]
                  [best-eval (car (max2 eval.play))]
                  [best-vloc (car (filter (lambda (vm) (= (car vm) best-eval)) eval.play))]
                  [inifin (find-diff (vector->list (vlocs colour)) (vector->list (cdr best-vloc)))])
             (exec-move (car inifin) (cdr inifin)))]
          [else (error "?3?")]))
  (let ([tree (search-tree colour ply)])
    ;minimax needs a tree to search
    (minimax-root tree)))

;the search tree on which the minimax algorithm operates
(define (search-tree colour search-depth)
  ;returns 'max if the node type is 'min and vice versa
  (define (invert type)
    (if (equal? type 'max) 'min 'max))
  (define (game-tree-h colour depth-left type self_vlocs oppn_vlocs)
    (cond [(= depth-left 1)
           (node (map (lambda (initfinal)
                        (begin (force-move (car initfinal) (cdr initfinal))
                               (let ([res (node '()
                                                (vector-map (lambda (x)
                                                              (if (= x (car initfinal)) (cdr initfinal) x))
                                                            oppn_vlocs)
                                                (vector-map (lambda (x)
                                                              (if (= x (car initfinal)) (cdr initfinal) x))
                                                            self_vlocs)
                                                (invert type))])
                                 (begin (force-move (cdr initfinal) (car initfinal)) res))))
                      (all-moves-length- colour))
                 self_vlocs
                 oppn_vlocs
                 type)]
          [else
           (node (map (lambda (initfinal)
                        (begin (force-move (car initfinal) (cdr initfinal))
                               (let ([res (game-tree-h (opponent colour)
                                                       (- depth-left 1)
                                                       (invert type)
                                                       (vector-map
                                                        (lambda (x) (if (= x (car initfinal)) (cdr initfinal) x))
                                                        oppn_vlocs)
                                                       (vector-map
                                                        (lambda (x) (if (= x (car initfinal)) (cdr initfinal) x))
                                                        self_vlocs))])
                                 (begin (force-move (cdr initfinal) (car initfinal)) res))))
                      (all-moves-length- colour))
                 self_vlocs
                 oppn_vlocs
                 type)]))
  (game-tree-h colour search-depth 'max (vlocs colour) (opponent-vlocs colour)))

#|
to call the alpha-beta-h function, use the following arguments
 node -> the node at which we wish to evaluate the move to be taken
 alpha -> -inf.0
 beta -> +inf.0
 search-depth -> the depth until which we wish to search
|#

(define (invert type)
  (if (equal? type 'max) 'min 'max))

(define ans (cons -inf.0 +inf.0))

;ALPHA-BETA
;returns a consing of two tilenums
;the car is the tilenum to be moved
;the cdr is the destination
;executes the move
(define (alpha-beta colour search-depth)
  (define (alpha-beta-h2 colour node alpha beta depth-left)
    (let ([temp-all-moves (all-moves-length- colour)])
      (alpha-beta-h1 colour node alpha beta depth-left temp-all-moves)))
  (define (alpha-beta-h1 colour node alpha beta depth-left t-a-m)
    ;performs a consing only if we are at either search-depth or (search-depth - 1).
    (define (optional-cons x)
      (if (or (= depth-left (- search-depth 1)) (= depth-left search-depth))
          (cons x (cdr ans)) x))
    (define (optional-car x)
      (cond [(= depth-left search-depth) (car x)]
            [(= depth-left (- search-depth 1))
             (begin (if (equal? (node-type node) 'min)
                        (let ([pos-num (evaluate (node-vlocs-opp node) (node-vlocs-self node) (opponent colour))])
                          (if (< (car ans) pos-num)
                              (set! ans (cons pos-num (node-vlocs-opp node)))
                              void))
                        void)
                    x)] ; not best but last vlocs set but correct depth!
            [else x])) ; t-a-m= temp-all-moves
    (cond [(= 0 depth-left)
           (evaluate (node-vlocs-self node) (node-vlocs-opp node)
                     (if (equal? 'max (node-type node)) colour (opponent colour)))]
          [else
           (if (null? t-a-m)
               ;end of iteration reached, now return alpha or beta depending on the type of node
               (if (eq? (node-type node) 'max)
                   (optional-cons alpha)
                   (optional-cons beta))
               (let* ([initfinal (car t-a-m)]
                      ;a subnode forming by forcing a move from (car initfinal) to (cdr initifnal)
                      [new-subnode
                       (make-node '() (vector-map (λ (x) (if (= x (car initfinal)) (cdr initfinal) x)) (node-vlocs-opp node))
                                  (vector-map (λ (x) (if (= x (car initfinal)) (cdr initfinal) x)) (node-vlocs-self node))
                                  (invert (node-type node)))])
                 (if (eq? (node-type node) 'min)
                     (begin
                       ;we need to change the board here
                       ;otherwise it is useless to call alpha-beta-h2
                       (force-move (car initfinal) (cdr initfinal))
                       (set! beta (min beta (optional-car (alpha-beta-h2 (opponent colour) new-subnode alpha beta (- depth-left 1)))))
                       ;and now we undo the move just made to bring the board to its current state again
                       (force-move (cdr initfinal) (car initfinal))
                       (if (>= alpha beta) (optional-cons beta)
                           (alpha-beta-h1 colour node alpha beta depth-left (cdr t-a-m))))
                     (begin
                       ;we need to change the board here
                       (force-move (car initfinal) (cdr initfinal))
                       (set! alpha (max alpha (optional-car (alpha-beta-h2 (opponent colour) new-subnode alpha beta (- depth-left 1)))))
                       ;and now we undo the move just made to bring the board to its current state again
                       (force-move (cdr initfinal) (car initfinal))
                       (if (>= alpha beta) (optional-cons alpha)
                           (alpha-beta-h1 colour node alpha beta depth-left (cdr t-a-m)))))))]))
  (let* ([res (alpha-beta-h2 colour (node '() (vlocs colour) (opponent-vlocs colour) 'max) -inf.0 +inf.0 search-depth)]
         [inifin (find-diff (vector->list (vlocs colour)) (vector->list (cdr res)))])
         ;initfin is the move to be made)
    (begin (set! ans (cons -inf.0 +inf.0))
           ;executes the move returned by alpha-beta
           (exec-move (car inifin) (cdr inifin)))))
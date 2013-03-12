;executes a random move for colour
(define (rand-move colour op1 op2) ; op1: + , op2: < for red
  (define (check-all-lower list)
    (cond [(null? list) #t] [(car list) (check-all-lower (cdr list))] [else #f]))
  (define (rand-move-h vlocs)
    (if (null? vlocs) void ; no moves left at all
        (let* ([init (car vlocs)] [fll (filter (lambda (move-info) (not (op2 (op1 (car move-info) 2) init))) ; backward moves rejected
                                               (valid-moves-length+ init))]) ; final length - list
          (if (null? fll) (begin (rand-move-h (cdr vlocs))) ; unmovable piece chosen
              (let* ([fl (car (shuffle fll))] ; final length - element
                     [ifl (cons init fl)]) ; initial final length - element
                (begin (exec-move (car ifl) (cadr ifl)))))))) ; move executed
  (rand-move-h (shuffle (vector->list (vlocs colour)))))
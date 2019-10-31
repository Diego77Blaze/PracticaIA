#lang racket
(define sudoku'((1 2 3 4 5 6 7 8 9)
                (4 5 6 1 2 3 9 7 8)
                (7 8 9 3 1 2 4 5 6)
                (4 5 6 1 2 3 7 8 9)))
(define board                   
  '((0 0 3 0 2 0 6 0 0)
    (9 0 0 3 0 5 0 0 1)
    (0 0 1 8 0 6 4 0 0)
    (0 0 8 1 0 2 9 0 0)
    (7 0 0 0 0 0 0 0 8)
    (0 0 6 7 0 8 2 0 0)
    (0 0 2 6 0 9 5 0 0)
    (8 0 0 2 0 3 0 0 9)
    (0 0 5 0 1 0 3 0 0)))

;(list-ref '(a b c) 2) 
(define (printLista lista)
(for* ([i 4]
       [j 9])
  (cond
    ([= j 0](display "|")(write (list-ref (list-ref lista i) j)))
    ([= j 8](write (list-ref (list-ref lista i) j))(display "|\n"))
    (else (write (list-ref (list-ref lista i) j)))))
)
(printLista sudoku)

(define (sudokucorrecto sudoku)
  (for*/first ([i 4]
              [j 9]
              #:when [(numberinlisth (list-ref (list-ref sudoku i) j) (car (list-ref sudoku i)) ) ]
              )
    (write i)
    )
  )

(define (numberinlisth number fila)       
  (cond
    ([not(equal? '() fila)](numberinlisth number (car fila)))
    (else (member number fila))
        ;([not(equal? #f(member number (list-ref sudoku fila)))] (car (member number(list-ref sudoku fila))))
        ;(else (member number (list-ref sudoku fila)))
      )
  )

(define primeralinea '(1 2 3 4 5 6 0 8 9)) 
(define (findfirstzero sudoku)
  (for/first ([i 9]
    #:when [= (list-ref sudoku i) 0]) i))

(define listaprueba empty)
;(findfirstzero primeralinea)
(sudokucorrecto board)
;(display sudoku)

(define lista '((0 0 3 0 2 0 6 0 0)
                (9 0 0 3 0 5 0 0 1)))
(define lista2 empty)
;(display (cons 15(cons (list-ref(list-ref lista 1) 3)empty)))


#lang racket
(define sudoku'((1 2 3 4 5 6 7 8 9)
                (4 5 6 1 2 3 9 7 8)
                (7 8 9 3 1 2 4 5 6)
                (4 5 6 1 2 3 7 8 9)))
(define board                   
  '((0 0 3 0 2 0 6 3 0)
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
(for* ([i 9]
       [j 9])
  (cond
    ([= j 0](display "|")(write (list-ref (list-ref lista i) j)))
    ([= j 8](write (list-ref (list-ref lista i) j))(display "|\n"))
    (else (write (list-ref (list-ref lista i) j)))))
)
(printLista board)

(define (sudokucorrecto sudoku)
  (for*/first ([i 4]
              [j 9]
              #:when [(numberInListH sudoku( list-ref (list-ref sudoku i) j) (cdr(member ( list-ref (list-ref sudoku i) j) (list-ref sudoku i))) ) ]
              )
    (write i)
    )
  )

(define (numberInListH number fila)
  (cond
    ;[(equal? empty fila)(#t)]
    [(equal? #f (member number fila))#f]
    [else #t]))
    
(define primeralinea '(1 2 3 4 5 6 0 8 9)) 
;(numberInListH 7 primeralinea)
(define (numberInListV sudoku number columna)
  (for*/first ([i 8]
              #:when [= number ( list-ref (list-ref sudoku i) columna)])    
    #t
      )

 )
;(numberInListV board 8 8)


(define(numberInListC number sudoku posicionx posiciony)
  (cond
    [(> 3 posicionx)
     (cond
       [(> 3 posiciony)1]
       [(and (< 3 posiciony)(> 6 posiciony))2]
       [(< 6 posiciony)3])
    ]
    [(and (< 2 posicionx)(> 6 posicionx))
     (cond
       [(> 3 posiciony)4]
       [(and (< 3 posiciony)(> 6 posiciony))5]
       [(< 6 posiciony)6])
     
     ]
    [(< 6 posicionx)
     (cond
       [(> 3 posiciony)7]
       [(and (< 3 posiciony)(> 6 posiciony))8]
       [(< 6 posiciony)9])
     ]

   )
)
(define(crearCuadrante numeroCuadrante sudoku)
  (cond
    [(= numeroCuadrante 1)(filter (for ([i 8][j 3]) (cond [(( list-ref (list-ref sudoku i) j))]) ))]
    [(= numeroCuadrante 2)]
    [(= numeroCuadrante 3)]
    [(= numeroCuadrante 4)]
    [(= numeroCuadrante 5)]
    [(= numeroCuadrante 6)]
    [(= numeroCuadrante 7)]
    [(= numeroCuadrante 8)]
    [(= numeroCuadrante 9)]
    )
  )
(numberInListC 3 board 3 5)

;(define primeralinea '(1 2 3 4 5 6 0 8 9)) 
(define (firstZeroRow lista)
  (for/first ([i 9]
    #:when [= (list-ref lista i) 0]) i))

(define (firstZero sudoku)
  (for*/first ([i 8]
               [j 8]
               #:when (= (list-ref (list-ref sudoku i)j)0))(list i j)))

(define (validNumber? numero posicion sudoku)
  (nor (numberInListH numero (list-ref sudoku(car posicion)))(numberInListV sudoku numero (cadr posicion))))
;---------------------------------------------------------------
;------------------------------------------------------------
#|(define (getSucesores sudoku)
  (cond
    [(empty? (firstZero sudoku))#f]
    [else (for ([i (in-range 1 9)]
                (if(validNumber? i (firstZero sudoku) sudoku)(list i '()))))]))
|#
(define (reemplazarCero numero posicion sudoku)
  (printLista(reemplazarElementoLista (reemplazarElementoLista numero (cadr posicion) (list-ref sudoku (car posicion)))(car posicion)sudoku)))

(define (reemplazarElementoLista elemento indice lista)
  (cond
    [(equal? 0 indice) (cons elemento (cdr lista))]
    [else (cons (car lista)(reemplazarElementoLista elemento (- indice 1) (cdr lista)))]))


;(findfirstzero primeralinea)
;(sudokucorrecto board)
;(display sudoku)'
(define lista '((0 0 3 0 2 0 6 0 0)
                (9 0 0 3 0 5 0 0 1)))
(define lista2 empty)
;(display (cons 15(cons (list-ref(list-ref lista 1) 3)empty)))




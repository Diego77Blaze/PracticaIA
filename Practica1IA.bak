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
       [(> 3 posiciony)(equal? #f (member number (construirCuadrante 1 board)) #f) #t]
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

(define(construirCuadrante numeroCuadrante board)
  (cond
    [(= numeroCuadrante 1)(list (list-ref (car(listaConstruir3PrimerasFilas board 0))0)
                                (list-ref (car(listaConstruir3PrimerasFilas board 0))1)
                                (list-ref (car(listaConstruir3PrimerasFilas board 0))2)
                                (list-ref (car(listaConstruir3PrimerasFilas board 1))0)
                                (list-ref (car(listaConstruir3PrimerasFilas board 1))1)
                                (list-ref (car(listaConstruir3PrimerasFilas board 1))2)
                                (list-ref (car(listaConstruir3PrimerasFilas board 2))0)
                                (list-ref (car(listaConstruir3PrimerasFilas board 2))1)
                                (list-ref (car(listaConstruir3PrimerasFilas board 2))2))]
    
    [(= numeroCuadrante 2)(list (list-ref (car(listaConstruir3PrimerasFilas board 0))3)
                                (list-ref (car(listaConstruir3PrimerasFilas board 0))4)
                                (list-ref (car(listaConstruir3PrimerasFilas board 0))5)
                                (list-ref (car(listaConstruir3PrimerasFilas board 1))3)
                                (list-ref (car(listaConstruir3PrimerasFilas board 1))4)
                                (list-ref (car(listaConstruir3PrimerasFilas board 1))5)
                                (list-ref (car(listaConstruir3PrimerasFilas board 2))3)
                                (list-ref (car(listaConstruir3PrimerasFilas board 2))4)
                                (list-ref (car(listaConstruir3PrimerasFilas board 2))5))]
    
    [(= numeroCuadrante 3)(list (list-ref (car(listaConstruir3PrimerasFilas board 0))6)
                                (list-ref (car(listaConstruir3PrimerasFilas board 0))7)
                                (list-ref (car(listaConstruir3PrimerasFilas board 0))8)
                                (list-ref (car(listaConstruir3PrimerasFilas board 1))6)
                                (list-ref (car(listaConstruir3PrimerasFilas board 1))7)
                                (list-ref (car(listaConstruir3PrimerasFilas board 1))8)
                                (list-ref (car(listaConstruir3PrimerasFilas board 2))6)
                                (list-ref (car(listaConstruir3PrimerasFilas board 2))7)
                                (list-ref (car(listaConstruir3PrimerasFilas board 2))8))]
    
    [(= numeroCuadrante 4)(list (list-ref (car(listaConstruir3FilasDelMedio board 3))0)
                                (list-ref (car(listaConstruir3FilasDelMedio board 3))1)
                                (list-ref (car(listaConstruir3FilasDelMedio board 3))2)
                                (list-ref (car(listaConstruir3FilasDelMedio board 4))0)
                                (list-ref (car(listaConstruir3FilasDelMedio board 4))1)
                                (list-ref (car(listaConstruir3FilasDelMedio board 4))2)
                                (list-ref (car(listaConstruir3FilasDelMedio board 5))0)
                                (list-ref (car(listaConstruir3FilasDelMedio board 5))1)
                                (list-ref (car(listaConstruir3FilasDelMedio board 5))2))]
    
    [(= numeroCuadrante 5)(list (list-ref (car(listaConstruir3FilasDelMedio board 3))3)
                                (list-ref (car(listaConstruir3FilasDelMedio board 3))4)
                                (list-ref (car(listaConstruir3FilasDelMedio board 3))5)
                                (list-ref (car(listaConstruir3FilasDelMedio board 4))3)
                                (list-ref (car(listaConstruir3FilasDelMedio board 4))4)
                                (list-ref (car(listaConstruir3FilasDelMedio board 4))5)
                                (list-ref (car(listaConstruir3FilasDelMedio board 5))3)
                                (list-ref (car(listaConstruir3FilasDelMedio board 5))4)
                                (list-ref (car(listaConstruir3FilasDelMedio board 5))5))]
    
    [(= numeroCuadrante 6)(list (list-ref (car(listaConstruir3FilasDelMedio board 3))6)
                                (list-ref (car(listaConstruir3FilasDelMedio board 3))7)
                                (list-ref (car(listaConstruir3FilasDelMedio board 3))8)
                                (list-ref (car(listaConstruir3FilasDelMedio board 4))6)
                                (list-ref (car(listaConstruir3FilasDelMedio board 4))7)
                                (list-ref (car(listaConstruir3FilasDelMedio board 4))8)
                                (list-ref (car(listaConstruir3FilasDelMedio board 5))6)
                                (list-ref (car(listaConstruir3FilasDelMedio board 5))7)
                                (list-ref (car(listaConstruir3FilasDelMedio board 5))8))]
    
    [(= numeroCuadrante 7)(list (list-ref (car(listaConstruir3UltimasFilas board 6))0)
                                (list-ref (car(listaConstruir3UltimasFilas board 6))1)
                                (list-ref (car(listaConstruir3UltimasFilas board 6))2)
                                (list-ref (car(listaConstruir3UltimasFilas board 7))0)
                                (list-ref (car(listaConstruir3UltimasFilas board 7))1)
                                (list-ref (car(listaConstruir3UltimasFilas board 7))2)
                                (list-ref (car(listaConstruir3UltimasFilas board 8))0)
                                (list-ref (car(listaConstruir3UltimasFilas board 8))1)
                                (list-ref (car(listaConstruir3UltimasFilas board 8))2))]
    
    [(= numeroCuadrante 8)(list (list-ref (car(listaConstruir3UltimasFilas board 6))3)
                                (list-ref (car(listaConstruir3UltimasFilas board 6))4)
                                (list-ref (car(listaConstruir3UltimasFilas board 6))5)
                                (list-ref (car(listaConstruir3UltimasFilas board 7))3)
                                (list-ref (car(listaConstruir3UltimasFilas board 7))4)
                                (list-ref (car(listaConstruir3UltimasFilas board 7))5)
                                (list-ref (car(listaConstruir3UltimasFilas board 8))3)
                                (list-ref (car(listaConstruir3UltimasFilas board 8))4)
                                (list-ref (car(listaConstruir3UltimasFilas board 8))5))]
    
    [(= numeroCuadrante 9)(list (list-ref (car(listaConstruir3UltimasFilas board 6))6)
                                (list-ref (car(listaConstruir3UltimasFilas board 6))7)
                                (list-ref (car(listaConstruir3UltimasFilas board 6))8)
                                (list-ref (car(listaConstruir3UltimasFilas board 7))6)
                                (list-ref (car(listaConstruir3UltimasFilas board 7))7)
                                (list-ref (car(listaConstruir3UltimasFilas board 7))8)
                                (list-ref (car(listaConstruir3UltimasFilas board 8))6)
                                (list-ref (car(listaConstruir3UltimasFilas board 8))7)
                                (list-ref (car(listaConstruir3UltimasFilas board 8))8))]
    )
  )
(define(listaConstruir3PrimerasFilas sudoku columna)(cond [(< columna 3) (cons (list-ref sudoku columna) (listaConstruir3PrimerasFilas sudoku (+ columna 1)) )]))
(define(listaConstruir3FilasDelMedio sudoku columna)(cond [(< columna 6) (cons (list-ref sudoku columna) (listaConstruir3FilasDelMedio sudoku (+ columna 1)) )]))
(define(listaConstruir3UltimasFilas sudoku columna)(cond [(< columna 9) (cons (list-ref sudoku columna) (listaConstruir3UltimasFilas sudoku (+ columna 1)) )]))
;(numberInListC 3 board 3 5)
(construirCuadrante 5 board)
;(listaConstruir3FilasDelMedio board 3)
;(listaConstruir3UltimasFilas board 6)
;(construirPrimerCuadrante board)
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

(define (reemplazarCero numero posicion sudoku)
  (reemplazarElementoLista (reemplazarElementoLista numero (cadr posicion) (list-ref sudoku (car posicion)))(car posicion)sudoku))

(define (reemplazarElementoLista elemento indice lista)
  (cond
    [(equal? 0 indice) (cons elemento (cdr lista))]
    [else (cons (car lista)(reemplazarElementoLista elemento (- indice 1) (cdr lista)))]))


(define (push elemento pila)
  (cons elemento pila))
(define (pop pila)(car pila))


(define (goalTest sudoku)
  (cond
    []))



(define (resolverSudoku sudoku)
  )


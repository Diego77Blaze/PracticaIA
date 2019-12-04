#lang racket

#|
Parametros: numero lista(fila de sudoku)
Descripcion: La funcion comprueba si el numero dado esta dentro de la lista dada
Retorno: true -> numero esta en lista
         false -> numero no esta en lista
|#
(define (numberInListH number fila)
  (cond
    [(equal? #f (member number fila))#f]
    [else #t]))

#|
Parametros: lista(matriz sudoku) numero1 numero2(indice de columna)
Descripcion: Comprueba si el numero1 se encuentra dentro de la columna numero2 de lista(sudoku)
Retorno: true -> si el numero1 se encuentra en la columna numero2 
|#

(define (numberInListV sudoku number columna)
  (for*/first ([i 9]
              #:when [= number ( list-ref (list-ref sudoku i) columna)])    
    #t
      )

 )

(define(listaConstruir3PrimerasFilas sudoku columna)(cond [(< columna 3) (cons (list-ref sudoku columna) (listaConstruir3PrimerasFilas sudoku (+ columna 1)) )]))
(define(listaConstruir3FilasDelMedio sudoku columna)(cond [(< columna 6) (cons (list-ref sudoku columna) (listaConstruir3FilasDelMedio sudoku (+ columna 1)) )]))
(define(listaConstruir3UltimasFilas sudoku columna)(cond [(< columna 9) (cons (list-ref sudoku columna) (listaConstruir3UltimasFilas sudoku (+ columna 1)) )]))

#|
Parametros:numeroCuadrante(entero) lista(matriz board)
Descripcion:Forma los cuadrantes del sudoku
Retorno:lista(cuadrante)
|#
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



#|
Parametros:number lista(matriz sudoku) posiciony posicionx
Descripcion:busca el numero introducido en el cuadrante que corresponda segun las coordenadas pasadas.
Retorno:#f->si encuentra el numero introducido en el cuadrante selecionado
        #t->si no encuentra el numero en el cuadrante.
|#
(define(numberInListC number sudoku posiciony posicionx)
  (cond
    [(> 3 posiciony)
     (cond
       [(> 3 posicionx) (if(equal? #f (member number (construirCuadrante 1 sudoku))) #f #t)]
       [(and (< 2 posicionx)(> 6 posicionx))(if(equal? #f (member number (construirCuadrante 2 sudoku))) #f #t)]
       [(< 5 posicionx)(if(equal? #f (member number (construirCuadrante 3 sudoku))) #f #t)])
    ]
    [(and (< 2 posiciony)(> 6 posiciony))
     (cond
       [(> 3 posicionx)(if(equal? #f (member number (construirCuadrante 4 sudoku))) #f #t)]
       [(and (< 2 posicionx)(> 6 posicionx))(if(equal? #f (member number (construirCuadrante 5 sudoku))) #f #t)]
       [(< 5 posicionx)(if(equal? #f (member number (construirCuadrante 6 sudoku))) #f #t)])
     
     ]
    [(< 5 posiciony)
     (cond
       [(> 3 posicionx)(if(equal? #f (member number (construirCuadrante 7 sudoku))) #f #t)]
       [(and (< 2 posicionx)(> 6 posicionx))(if(equal? #f (member number (construirCuadrante 8 sudoku))) #f #t)]
       [(< 5 posicionx)(if(equal? #f (member number (construirCuadrante 9 sudoku))) #f #t)])
     ]

   )
)

#|
Parametros: lista(matriz sudoku)
Descripcion: Recorre el sudoku posicion a posicion hasta que se encuentra un 0
Retorno: lista del formato -> fila, columna
|#
(define (firstZero sudoku)
  (for*/first ([i 9]
               [j 9]
               #:when (= (list-ref (list-ref sudoku i)j)0))(list i j)))


#|
Parametros: numero lista(fila,columna) lista(matriz sudoku)
Descripcion: dado un numero comprueba si es posible colocarlo en la posicion dada del sudoku dado (siguiendo las reglas del sudoku)
Retorno: true -> el numero no esta ni en fila ni en columna ni en cuadrante de la posicion
         false -> el numero no cumple la condicion anterior y no es valido en esa posicionn
|#
(define (numeroValido? numero posicion sudoku)
  (nor (numberInListH numero (list-ref sudoku(car posicion)))(numberInListV sudoku numero (cadr posicion))(numberInListC numero sudoku (car posicion)(cadr posicion))))


#|
Parametros: numero lista(posicion) lista(matriz sudoku)
Descripcion: Reemplaza cero encontrado en la lista posicion por el numero dado
Retorno: lista (matriz sudoku)
|#
(define (reemplazarCero numero posicion sudoku)
  (reemplazarElementoLista (reemplazarElementoLista numero (cadr posicion) (list-ref sudoku (car posicion)))(car posicion)sudoku))


#|
Parametros: elemento indice lista
Descripcion: reemplaza el elemento que se encuentra en el indice de lista con el elemento dado
Retorno: lista
|#
(define (reemplazarElementoLista elemento indice lista)
  (cond
    [(equal? 0 indice) (cons elemento (cdr lista))]
    [else (cons (car lista)(reemplazarElementoLista elemento (- indice 1) (cdr lista)))]))
#|
Parametros: elemento pila
Descripcion: añade el elemento a la cima de la pila
Retorno: void
|#

(define (push elemento pila)
  (cons elemento pila))


#|
Parametros: pila
Descripcion: saca la cima de la pila
Retorno: elemento en la cima de la pila
|#
(define (pop pila)(car pila))


#|
Parametros: elemento cola
Descripcion: añade elemento al final de cola
Retorno: void
|#
(define (encolar elemento cola)
  (cons elemento cola))


#|
Parametros: cola
Descripcion: obtiene el primero de la cola
Retorno: elemento al principio de la cola
|#
(define (obtenerPrimeroCola cola)
  (last cola))


#|
Parametros: cola
Descripcion: desencola el elemento primero de la cola
Retorno: cola 
|#
(define (desencolar cola)
  (reverse(cdr (reverse cola))))


#|
Parametros: numero lista(posicion) lista(matriz sudoku) lista(vacia)
Descripcion: añade a la lista vacía dada los numero que pueden insertarse en una posicion con cero dentro de sudoku
Retorno: lista
|#
(define (getOperacionesValidas iteracion posicion sudoku lista)
  (cond
    [(equal? 0 iteracion) lista]
    [(numeroValido? iteracion posicion sudoku)(getOperacionesValidas (- iteracion 1) posicion sudoku (cons iteracion lista))]
    [else (getOperacionesValidas (- iteracion 1) posicion sudoku lista)]))



#|
Parametros: lista(numerosValidos) lista(posicion) lista(matriz sudooku) pila 
Descripcion: dada una lista de numeros validos, genera nuevos sudokus sustituyendo el primer cero por cada uno de los numeros de la lista y apilandolos uno encima de otro
Retorno: pila de sucesores
|#
(define (apilarSucesores numerosValidos posicion sudoku pila)
  (cond
    [(empty? numerosValidos)pila]
    [else (apilarSucesores (cdr numerosValidos) posicion sudoku (push(reemplazarCero (car numerosValidos)posicion sudoku) pila))]))


#|
Parametros: lista(numerosValidos) lista(posicion) lista(matriz sudoku) cola
Descripcion: dada una lista de numeros validos, genera nuevos sudokus sustituyendo el primer cero por cada uno de los numeros de la lista y encolando uno detras de otro
Retorno: cola de sucesores
|#
(define (encolarSucesores numerosValidos posicion sudoku cola)
  (cond
    [(empty? numerosValidos)cola]
    [else (encolarSucesores (cdr numerosValidos) posicion sudoku (encolar(reemplazarCero(car numerosValidos)posicion sudoku) cola))]))



#|
Parametros: lista(matriz sudoku)
Descripcion: comprueba si el sudoku dado es un estado meta o no
Retorno: true -> si sudoku estado meta
         false -> si sudoku no es estado meta
|#
(define (goalTest sudoku)
  (cond
    [(list? (firstZero sudoku)) #f]
    [else #t]))

#|
Parametros:lista(matriz filas sudoku)
Descripcion:muestra las 3 filas introducidas
|#
(define (visualizar3Filas sudoku)
  (for*
      [(i 4)(j 24)]
    (cond
      [(= i 3)(cond
                [(= j 7)(display "+")]
                [(= j 15)(display "+")]
                [(= j 23)(display "\n")]
                [else (display "-")]
                )
      ]
      
      [(< j 9)(cond
                 [(= 3 j)(display " | ")(write (list-ref (list-ref sudoku i) j))]
                 [(= 6 j)(display " | ")(write (list-ref (list-ref sudoku i) j))]                
                 [(= j 8)(display " ")(write (list-ref (list-ref sudoku i) j)) (display "\n")]
                 [else (display " ")(write (list-ref (list-ref sudoku i) j))]
      )]
    
    )
    
  )
)


#|
Parametros:lista (matriz sudoku)
Descripcion:muestra por pantalla el sudoku introducido
|#
(define (visualizarSudoku sudoku)
  (display "-------+-------+-------\n")
  (visualizar3Filas (listaConstruir3PrimerasFilas sudoku 0))
  (visualizar3Filas (listaConstruir3FilasDelMedio sudoku 3))
  (visualizar3Filas (listaConstruir3UltimasFilas sudoku 6))
  )


#|
Parametros: lista (matriz sudoku inicial) lista (matriz sudoku actual) lista booleano

Descripcion: aplicando el algoritmo de busqueda en profundidad y gracias a una lista de abiertos y la recursividad de la funcion,
explora un arbol de estados hasta encontrar, si es que tiene, la solucon del sudoku inicial dado, ademas mediante un booleano se puede indicar si
se quiere obtener solamente la solucion o la secuencia de pasos que llevan a ella

Retorno: se visualiza en pantalla o bien el sudoku solucion o la secuencia de pasos a la solucion
|#
(define (resolverSudokuDFS sudokuInicial sudoku abiertos backtracking)
  (cond
    [(goalTest sudoku)(if backtracking (pasosSolucion sudokuInicial sudoku)(visualizarSudoku sudoku))]
    [else (resolverSudokuDFS sudokuInicial(pop(apilarSucesores(getOperacionesValidas 9(firstZero sudoku)sudoku '())(firstZero sudoku)sudoku abiertos))
                          (cdr(apilarSucesores (getOperacionesValidas 9(firstZero sudoku)sudoku '())(firstZero sudoku)sudoku abiertos))backtracking)]))



#|
Parametros: lista (matriz sudoku inicial) lista (matriz sudoku actual) lista booleano

Descripcion: aplicando el algoritmo de busqueda en anchura y gracias a una lista de abiertos y la recursividad de la funcion,
explora un arbol de estados hasta encontrar, si es que tiene, la solucon del sudoku inicial dado, ademas mediante un booleano se puede indicar si
se quiere obtener solamente la solucion o la secuencia de pasos que llevan a ella

Retorno: se visualiza en pantalla o bien el sudoku solucion o la secuencia de pasos a la solucion
|#
(define (resolverSudokuBFS sudokuInicial sudoku abiertos backtracking)
  (cond
    [(goalTest sudoku)(if backtracking (pasosSolucion sudokuInicial sudoku)(visualizarSudoku sudoku))]
    [else (resolverSudokuBFS sudokuInicial(obtenerPrimeroCola(encolarSucesores (getOperacionesValidas 9(firstZero sudoku)sudoku '())(firstZero sudoku)sudoku abiertos))
                             (desencolar(encolarSucesores (getOperacionesValidas 9(firstZero sudoku)sudoku '())(firstZero sudoku)sudoku abiertos))backtracking)]))



#|
Parametros: lista (matriz sudoku inicial) lista (matriz sudoku actual)

Descripcion: crea uno de los pasos dados para recrear el procedimiento de solución

Retorno: sudoku (lista de 9 listas)
|#
(define (backtracking sudInicial sudFinal)
  (reemplazarElementoLista (reemplazarElementoLista (list-ref (list-ref sudFinal (car(firstZero sudInicial))) (cadr(firstZero sudInicial))) (cadr(firstZero sudInicial)) (list-ref sudInicial (car(firstZero sudInicial)))) (car(firstZero sudInicial)) sudInicial)
  )


#|
Parametros: lista (matriz sudoku inicial) lista (matriz sudoku actual)

Descripcion: imprime por pantalla todos los pasos que se han seguido para alcanzar la solución del sudoku

Retorno:se visualiza en pantalla la secuencia de pasos solución
|#
(define (pasosSolucion sudInicial sudFinal)
  (cond
    [(equal? #f (equal? sudInicial sudFinal))
     (cond
       [(pair? (firstZero sudInicial))
        (visualizarSudoku (backtracking sudInicial sudFinal))
        (display "\n")
        (pasosSolucion (backtracking sudInicial sudFinal) sudFinal)]
       )
     ]
    )
  )

(define (menuInicial)
  (display "- - - - - - - - - - RESOLVEDOR DE SUDOKUS - - - - - - - - - -\n")
  (display "¿Desea que se muestren todos los pasos hasta llegar a la solución? S/N\n")
  (define backtr (read-line (current-input-port) 'any))
  (display "¿Desea resolverlo mediante el algoritmo de DFS (búsqueda en profundidad) o el algoritmo de BFS (búsqueda en anchura)? DFS/BFS \n")
  (define metodo (read-line (current-input-port) 'any))
  (display "¿Qué sudoku desea resolver del 1 al 20? Ej: 1, 2, 3... 20\n")
  (define numSudoku (string->number (read-line (current-input-port) 'any)))
  
  (cond
    [(equal? numSudoku 1) (cond                           
                            [(not(compruebaSudoku test1)) (cond
                                                             [(equal? "S" backtr) (cond
                                                 
                                                                                    [(equal? "DFS" metodo)(resolverSudokuDFS test1 test1 '() #t)]
                                                                                    [(equal? "BFS" metodo)(resolverSudokuBFS test1 test1 '() #t)]
                                                                                    [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [(equal? "N" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test1 test1 '() #f)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test1 test1 '() #f)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                             ) 
                                                           ]
                            [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 2)(cond
                           [(not(compruebaSudoku test2)) (cond
                                                           [(equal? "S" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test2 test2 '() #t)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test2 test2 '() #t)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [(equal? "N" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test2 test2 '() #f)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test2 test2 '() #f)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                           ) 
                                                         ]
                           [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 3)(cond
                           [(not(compruebaSudoku test3)) (cond
                                                           [(equal? "S" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test3 test3 '() #t)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test3 test3 '() #t)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [(equal? "N" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test3 test3 '() #f)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test3 test3 '() #f)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                           ) 
                                                         ]
                           [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 4)(cond
                           [(not(compruebaSudoku test4)) (cond
                                                           [(equal? "S" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test4 test4 '() #t)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test4 test4 '() #t)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [(equal? "N" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test4 test4 '() #f)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test4 test4 '() #f)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                           ) 
                                                         ]
                           [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 5)(cond
                           [(not(compruebaSudoku test5)) (cond
                                                           [(equal? "S" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test5 test5 '() #t)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test5 test5 '() #t)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [(equal? "N" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test5 test5 '() #f)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test5 test5 '() #f)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                           ) 
                                                         ]
                           [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 6)(cond
                           [(not(compruebaSudoku test6)) (cond
                                                           [(equal? "S" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test6 test6 '() #t)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test6 test6 '() #t)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [(equal? "N" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test6 test6 '() #f)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test6 test6 '() #f)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                           ) 
                                                         ]
                           [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 7)(cond
                           [(not(compruebaSudoku test7)) (cond
                                                           [(equal? "S" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test7 test7 '() #t)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test7 test7 '() #t)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [(equal? "N" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test7 test7 '() #f)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test7 test7 '() #f)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                           ) 
                                                         ]
                           [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 8)(cond
                           [(not(compruebaSudoku test8)) (cond
                                                           [(equal? "S" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test8 test8 '() #t)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test8 test8 '() #t)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [(equal? "N" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test8 test8 '() #f)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test8 test8 '() #f)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                           ) 
                                                         ]
                           [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 9)(cond
                           [(not(compruebaSudoku test9)) (cond
                                                           [(equal? "S" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test9 test9 '() #t)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test9 test9 '() #t)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [(equal? "N" backtr)(cond
                                                                                 [(equal? "DFS" metodo)(resolverSudokuDFS test9 test9 '() #f)]
                                                                                 [(equal? "BFS" metodo)(resolverSudokuBFS test9 test9 '() #f)]
                                                                                 [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                           [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                           ) 
                                                         ]
                           [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 10)(cond
                            [(not(compruebaSudoku test10)) (cond
                                                             [(equal? "S" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test10 test10 '() #t)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test10 test10 '() #t)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [(equal? "N" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test10 test10 '() #f)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test10 test10 '() #f)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                             ) 
                                                           ]
                            [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 11)(cond
                            [(not(compruebaSudoku test11)) (cond
                                                             [(equal? "S" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test11 test11 '() #t)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test11 test11 '() #t)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [(equal? "N" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test11 test11 '() #f)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test11 test11 '() #f)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                             ) 
                                                           ]
                            [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 12)(cond
                            [(not(compruebaSudoku test12)) (cond
                                                             [(equal? "S" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test12 test12 '() #t)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test12 test12 '() #t)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [(equal? "N" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test12 test12 '() #f)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test12 test12 '() #f)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                             ) 
                                                           ]
                            [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 13)(cond
                            [(not(compruebaSudoku test13)) (cond
                                                             [(equal? "S" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test13 test13 '() #t)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test13 test13 '() #t)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [(equal? "N" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS numSudoku numSudoku '() #f)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS numSudoku numSudoku '() #f)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                             ) 
                                                           ]
                            [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 14)(cond
                            [(not(compruebaSudoku test14)) (cond
                                                             [(equal? "S" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test14 test14 '() #t)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test14 test14 '() #t)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [(equal? "N" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test14 test14 '() #f)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test14 test14 '() #f)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                             ) 
                                                           ]
                            [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 15)(cond
                            [(not(compruebaSudoku test15)) (cond
                                                             [(equal? "S" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test15 test15 '() #t)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test15 test15 '() #t)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [(equal? "N" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test15 test15 '() #f)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test15 test15 '() #f)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                             ) 
                                                           ]
                            [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 16)(cond
                            [(not(compruebaSudoku test16)) (cond
                                                             [(equal? "S" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test16 test16 '() #t)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test16 test16 '() #t)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [(equal? "N" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test16 test16 '() #f)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test16 test16 '() #f)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                             ) 
                                                           ]
                            [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 17)(cond
                            [(not(compruebaSudoku test17)) (cond
                                                             [(equal? "S" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test17 test17 '() #t)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test17 test17 '() #t)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [(equal? "N" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test17 test17 '() #f)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test17 test17 '() #f)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                             ) 
                                                           ]
                            [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 18)(cond
                            [(not(compruebaSudoku test18)) (cond
                                                             [(equal? "S" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test18 test18 '() #t)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test18 test18 '() #t)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [(equal? "N" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test18 test18 '() #f)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test18 test18 '() #f)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                             ) 
                                                           ]
                            [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 19)(cond
                            [(not(compruebaSudoku test19)) (cond
                                                             [(equal? "S" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test19 test19 '() #t)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test19 test19 '() #t)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [(equal? "N" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test19 test19 '() #f)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test19 test19 '() #f)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                             ) 
                                                           ]
                            [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [(equal? numSudoku 20)(cond
                            [(not(compruebaSudoku test20)) (cond
                                                             [(equal? "S" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test20 test20 '() #t)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test20 test20 '() #t)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [(equal? "N" backtr)(cond
                                                                                   [(equal? "DFS" metodo)(resolverSudokuDFS test20 test20 '() #f)]
                                                                                   [(equal? "BFS" metodo)(resolverSudokuBFS test20 test20 '() #f)]
                                                                                   [else (display "Error: Especifique correctamente el método de búsqueda--> DFS/BFS")])]
                                                             [else (display "Error: Elija correctamente la opción del backtracking--> S/N")]
                                                             ) 
                                                           ]
                            [else (display "Error: el sudoku seleccionado no es resoluble")])]
    [else (display "Error: Especifique correctamente el sudoku del 1 al 20--> 1, 2, 3... 20")]
    ))


(define test1
  '((5 0 0 0 0 0 0 0 0)
    (0 2 8 4 0 0 5 0 3)
    (1 0 0 2 7 0 0 0 6)
    (0 0 3 0 5 2 1 9 0)
    (7 0 6 0 1 0 2 0 8)
    (0 1 9 7 4 0 3 0 0)
    (6 0 0 0 9 4 0 0 2)
    (8 0 1 0 0 6 7 5 0)
    (0 0 0 0 0 0 0 0 4)))
(define test2
  '((5 6 0 0 0 4 1 0 0) 
    (0 7 0 6 0 0 0 3 0) 
    (0 0 0 1 2 0 0 6 0) 
    (0 0 0 3 0 8 6 9 2) 
    (0 8 0 0 7 0 0 4 0) 
    (4 3 2 9 0 6 0 0 0) 
    (0 5 0 0 9 7 0 0 0) 
    (0 2 0 0 0 1 0 5 0) 
    (0 0 9 5 0 0 0 8 6)))
(define test3
  '((0 0 4 0 3 0 0 6 0) 
    (1 6 0 0 0 2 0 3 0) 
    (9 0 2 8 0 6 4 5 0) 
    (0 0 0 0 6 0 9 1 0) 
    (0 0 0 0 0 0 0 0 0)
    (0 1 8 0 5 0 0 0 0)
    (0 2 6 1 0 5 3 0 4) 
    (0 7 0 6 0 0 0 8 2) 
    (0 4 0 0 2 0 6 0 0)))
(define test4
  '((0 0 0 0 0 4 8 0 0) 
    (0 2 0 0 0 3 0 1 0) 
    (6 0 1 0 0 0 7 0 5) 
    (5 8 0 0 0 9 0 7 0) 
    (0 0 4 0 0 0 5 0 0) 
    (0 1 0 7 0 0 0 8 6) 
    (4 0 3 0 0 0 9 0 2) 
    (0 6 0 3 0 0 0 4 0) 
    (0 0 2 0 6 0 0 0 0)))
(define test5
  '((9 0 8 0 0 2 0 0 0) 
    (0 0 0 0 7 0 0 5 8) 
    (0 7 0 0 0 9 0 0 0) 
    (0 9 4 0 1 8 5 0 7) 
    (5 0 0 0 9 0 0 0 1) 
    (2 0 1 7 6 0 3 9 0) 
    (0 0 0 5 0 0 0 3 0) 
    (8 1 0 0 3 0 0 0 0) 
    (0 0 0 9 0 0 7 0 6)))
(define test6
  '((0 1 0 9 4 2 7 0 0) 
    (0 0 0 6 0 0 2 0 0) 
    (2 0 0 0 7 1 9 0 0) 
    (4 8 0 0 0 0 0 0 2) 
    (7 0 0 0 0 0 0 0 9) 
    (1 0 0 0 0 0 0 8 6) 
    (0 0 7 8 2 0 0 0 1) 
    (0 0 1 0 0 9 0 0 0) 
    (0 0 8 5 1 6 0 2 0)))
(define test7
  '((6 1 0 3 5 4 0 9 0) 
    (0 9 0 0 0 0 0 0 4) 
    (8 4 0 0 9 0 6 1 0) 
    (0 3 0 4 0 0 8 0 0) 
    (0 0 0 9 6 1 0 0 0) 
    (0 0 1 0 0 3 0 5 0) 
    (0 6 4 0 3 0 0 8 2) 
    (2 0 0 0 0 0 0 3 0) 
    (0 8 0 5 1 2 0 6 9)))
(define test8
  '((5 0 0 3 4 0 0 0 1) 
    (0 1 4 0 0 6 3 7 0) 
    (7 0 3 9 0 8 4 0 0) 
    (2 7 9 0 0 0 0 0 0) 
    (0 4 0 0 0 0 0 3 0) 
    (0 0 0 0 0 0 6 1 7) 
    (0 0 7 2 0 3 8 0 6) 
    (0 5 6 8 0 0 1 2 0) 
    (8 0 0 0 6 4 0 0 9)))
(define test9
  '((0 0 5 0 0 7 0 6 8) 
    (0 0 0 0 0 0 0 0 0) 
    (9 8 0 6 3 0 2 7 0) 
    (0 0 6 1 0 0 0 2 4) 
    (0 0 0 0 4 0 0 0 0) 
    (2 3 0 0 0 8 9 0 0) 
    (0 5 8 0 1 3 0 9 2) 
    (0 0 0 0 0 0 0 0 0) 
    (4 6 0 2 0 0 5 0 0)))
(define test10
  '((1 0 0 0 9 6 0 0 0) 
    (0 9 0 3 5 7 0 6 8) 
    (0 0 0 0 8 0 7 0 0) 
    (0 1 0 7 0 0 6 5 0) 
    (0 0 0 0 0 0 0 0 0) 
    (0 3 4 0 0 9 0 8 0) 
    (0 0 1 0 7 0 0 0 0) 
    (9 8 0 1 6 4 0 7 0) 
    (0 0 0 5 3 0 0 0 6)))
(define test11
  '((0 0 0 2 1 9 0 0 6) 
    (0 2 7 6 0 0 0 0 5) 
    (0 0 0 5 0 3 8 0 0) 
    (1 0 0 0 9 0 0 6 0) 
    (7 0 0 1 0 8 0 0 3) 
    (0 9 0 0 4 0 0 0 7) 
    (0 0 9 8 0 1 0 0 0) 
    (8 0 0 0 0 6 5 3 0) 
    (5 0 0 9 3 7 0 0 0)))
(define test12
  '((3 5 0 2 0 0 8 9 0) 
    (0 4 0 7 0 8 0 0 0) 
    (1 0 8 0 3 0 0 0 0) 
    (4 0 5 0 7 2 0 1 0) 
    (0 0 6 8 0 1 9 0 0) 
    (0 9 0 3 5 0 2 0 7) 
    (0 0 0 0 2 0 3 0 5) 
    (0 0 0 6 0 5 0 2 0) 
    (0 1 2 0 0 3 0 6 9)))
(define test13
  '((1 6 0 9 3 0 0 8 0) 
    (0 0 0 1 0 0 0 0 9) 
    (0 0 0 8 5 0 1 3 0) 
    (9 0 5 0 1 0 6 4 2) 
    (0 0 0 0 0 0 0 0 0) 
    (4 8 2 0 6 0 5 0 1) 
    (0 4 3 0 8 2 0 0 0) 
    (5 0 0 0 0 3 0 0 0) 
    (0 2 0 0 9 1 0 5 6)))
(define test14
  '((0 0 0 0 6 3 0 5 4) 
    (1 0 5 7 0 0 0 2 3) 
    (0 6 7 0 0 2 0 0 0) 
    (0 0 0 0 4 0 3 8 6) 
    (8 0 0 5 0 6 0 0 1) 
    (4 1 6 0 9 0 0 0 0) 
    (0 0 0 8 0 0 1 6 0) 
    (6 3 0 0 0 7 8 0 5) 
    (7 8 0 6 1 0 0 0 0)))
(define test15
  '((0 5 4 6 2 0 0 0 0) 
    (6 0 7 0 0 8 0 3 1) 
    (8 0 0 0 0 5 2 0 4) 
    (0 0 0 9 0 1 0 7 0) 
    (7 0 0 5 0 4 0 0 6) 
    (0 9 0 7 0 2 0 0 0) 
    (3 0 1 8 0 0 0 0 2) 
    (2 8 0 3 0 0 4 0 9) 
    (0 0 0 0 1 6 3 8 0)))
(define test16
  '((9 7 0 0 0 3 4 5 0) 
    (1 0 0 0 2 4 0 3 0) 
    (4 3 0 0 0 9 2 0 6) 
    (7 0 4 8 0 0 0 0 0) 
    (0 9 0 0 0 0 0 8 0) 
    (0 0 0 0 0 2 7 0 5) 
    (2 0 7 9 0 0 0 6 8) 
    (0 4 0 3 6 0 0 0 7) 
    (0 5 3 2 0 0 0 9 4)))
(define test17
  '((3 0 8 0 0 0 0 0 0) 
    (0 0 0 0 4 0 5 0 7) 
    (6 7 0 0 5 8 0 1 9) 
    (1 8 0 0 0 0 0 0 0) 
    (7 9 0 8 2 3 0 4 5) 
    (0 0 0 0 0 0 0 9 3) 
    (4 6 0 7 8 0 0 3 1) 
    (2 0 9 0 1 0 0 0 0) 
    (0 0 0 0 0 0 4 0 6)))
(define test18
  '((0 7 6 0 0 3 0 0 0 ) 
    (0 0 0 8 0 0 6 0 0) 
    (3 0 0 0 5 0 0 2 4) 
    (7 8 3 5 0 0 0 9 0) 
    (1 2 9 6 0 8 7 4 5) 
    (0 4 0 0 0 7 8 1 3) 
    (8 5 0 0 6 0 0 0 2) 
    (0 0 7 0 0 1 0 0 0) 
    (0 0 0 4 0 0 3 8 0)))
(define test19
  '((0 0 0 0 0 8 0 0 0)
    (3 0 8 0 0 0 7 0 0)
    (2 9 4 7 0 0 0 8 1)
    (4 0 0 8 6 0 0 7 0)
    (0 0 0 0 0 0 0 0 0)
    (0 7 0 0 9 2 0 0 6)
    (6 5 0 0 0 1 2 4 3)
    (0 0 3 0 0 0 8 0 9)
    (0 0 0 5 0 0 0 0 0)))
(define test20
  '((8 0 0 4 6 0 0 0 0) 
    (0 0 0 0 1 0 0 0 9) 
    (0 7 0 3 0 8 6 0 0) 
    (2 0 0 0 0 0 0 9 0) 
    (3 8 0 0 7 0 0 2 1) 
    (0 5 0 0 0 0 0 0 8) 
    (0 0 4 5 0 6 0 1 0) 
    (5 0 0 4 0 0 0 0 0) 
    (0 0 0 0 9 3 0 0 6)))


(provide (all-defined-out))

(menuInicial)

#|
Parametros: lista (matriz sudoku)

Descripcion:comprueba si los valores del sudoku estan entre 1 y 9 y que el sudoku no tiene valores repetidos en la misma linea columna o cuadrante

Retorno:si ha encontrado algun error en el sudoku retorna #t y si no retorna #f
|#
(define (compruebaSudoku sudoku)
  (for*/first ([i 9]
               [j 9]
               #:when [or  (< 9 (list-ref(list-ref sudoku i) j)) (> 0 (list-ref(list-ref sudoku i) j)) (and(not(= 0 (list-ref(list-ref sudoku i) j))) (or (numberInListH (list-ref(list-ref sudoku i) j) (list-ref (reemplazarCero 0 (list i j) sudoku) i))
                                                                           (numberInListV (reemplazarCero 0 (list i j) sudoku)  (list-ref(list-ref sudoku i) j) j)
                                                                           (numberInListC (list-ref(list-ref sudoku i) j) (reemplazarCero 0 (list i j) sudoku) i j)
                                                                           ))]) 
    #f #t
    ))



#||
Proyecto: Polynomial Calculator

Autores:  Gustavo Fallas
          Ricardo Chang
          Randy Martínez

Contenidos:
          1. Declaración de Funciones
          2. Declaración de Variables Globales
          3. Invocación de Funciones
||#


#||----------Declaración de Variables Globales----------||#
(defvar *result* (make-array 100 :fill-pointer 0))
(defvar *poly1* (make-array 100 :fill-pointer 0))
(defvar *poly2* (make-array 100 :fill-pointer 0))
(defvar *polyEval* (make-array 3 :fill-pointer 0))

(defvar *isCorrect* 1) ;retorna 1 verdadero, 0 es falso
(defvar *x*)
(defvar *y*)

#||----------Declaración de Funciones----------||#

#||Inserta un vector en una matriz.||#
(defun addPoly(vec1 polynomial);agregar la entrada a la matriz polinomio asignada
    (setq triplets (make-array 4 :fill-pointer 0))
    (setq vec1_length (length vec1)) 
    (setq index 0)
    (loop 
        (if (char= (char vec1 index)  (code-char 124))
            (progn
                (vector-push triplets polynomial)
                (setq triplets (make-array 4 :fill-pointer 0))
            )       
        )
        (if ( and (char/= (char vec1 index) #\-) ( and (char/= (char vec1 index)  #\Space) (char/= (char vec1 index)  (code-char 124)) ))
                 (progn
                    (vector-push (digit-char-p (char vec1 index)) triplets)
                 )
        )
        (if (char= (char vec1 index) #\-)
            (progn
                (vector-push (-(digit-char-p (char vec1 (+ index 1)))) triplets)
               (setq index (+ index 1)) ; +1
            )  
        )
        (setq index (+ index 1))
        (when (>= index vec1_length) (return index))
    )
    (vector-push triplets polynomial)
)

#|
Funcion para realizar suma y resta
Recibe como parametro los dos polinomios que desea suma
y la operacion (suma/resta)
Guarda el resultado en la variable "*result*".
|#
(defun suma_resta(polynomialA polynomialB operation)
    (setq *result* (make-array 100 :fill-pointer 0))
    (setq sizePolyA (length polynomialA))
    (setq sizePolyA (- sizePolyA 1));cantidad de tripletas del polinomio A (pA)
    (loop for index from 0 to sizePolyA 
        do(progn
            (setq tripletA (elt polynomialA index));tripleta de pB
            (setq a (elt tripletA 0));el valor de a de la tripletaA
            (setq b (elt tripletA 1));el valor de b de la tripletaA
            (setq c (elt tripletA 2));el valor de c de la tripletaA
            (setq j 0)
            (setq flagWhile 1)

            (loop (progn
                (setq sizePolyB (length polynomialB))
                (setq sizePolyB (- sizePolyB 1));cantidad de tripletas del polinomio B (pB)
                (if (> j sizePolyB) ;verifica si el indice j supero la cantidad de tripletas, ya que no tiene con quien (sumar/restar)
                    (progn
                        (vector-push tripletA *result*) ;agrega la tripleta selecciona de pA al resultado
                        (setq flagWhile 0)
                        (return nil) ;se sale del loop secunadario

                    )
                )
                (setq tripletB (elt polynomialB j)) ;seleccion una tripleta de pB, de acuerdo con el indice j
                ;;compara el valor de 'b' y 'c' con la tripleta de pB
                (if ( and (= (elt tripletB 1) b) (= (elt tripletB 2) c) )
                    (progn
                        (setq tempTriplet_Result (make-array 3 :fill-pointer 0)) ;vector temporal para agregar los resultados
                        (setq temp_result_a ( funcall  operation a (elt tripletB 0) ) ) ;de acuerdo al valor en 'operation' de aplica la funcion
                        (vector-push temp_result_a tempTriplet_Result) ;agrega el resultado de 'a' al vector temps
                        (vector-push b tempTriplet_Result)
                        (vector-push c tempTriplet_Result)
                        (if (/= temp_result_a 0) 
                            (progn ;si es diferente a cero lo agrega al vector 'result'
                                (vector-push tempTriplet_Result *result*)
                                (delete tripletB polynomialB) ;y lo borra de pB
                                (setq flagWhile 0)
                                (return nil) ;se sale del loop secunadario
                            )
                            (progn   ;si el resultado es cero no lo agrega al resultado final
                                (delete tripletB polynomialB)
                                (return nil) ;se sale del loop secunadario
                            )
                        )
                        
                    )
                )
                (setq j (+ j 1)) ;incremente el indice j
                (when flagWhile ));(return flagWhile) ))
            )
        )
    )
    ;;agrega las tripletas que no fueron borradas de pB
    (setq sizePolyB_F (length polynomialB))
    (setq sizePolyB_F (- sizePolyB_F 1))
    (loop for i from 0 to  sizePolyB_F
        do(vector-push (elt polynomialB i) *result*)
    )
    (princ " = ")
    (princ *result*)
    (terpri)
)

#||Ejecuta la multiplicación de dos polinomios.||#
(defun multiplication(poly1 poly2)
    (defvar aux_multi (make-array 100 :fill-pointer 0))
   

    (defvar length_poly1 (length poly1))  ; largo poly1
    (defvar length_poly2 (length poly2))  ; largo poly2

    (setq length_poly1 (- length_poly1 1)) ; largo poly1 -1
    (setq length_poly2 (- length_poly2 1)) ; largo poly2 -1

    (loop for i from 0 to length_poly1
      do (progn
            (setq tripletsR (make-array 100 :fill-pointer 0)) ; vector resultante
            (loop for j from 0 to length_poly2
              do (progn
                    (setq temporalTriplets (make-array 3 :fill-pointer 0)) ; vector temporal de respuestas
                    (setq tripletsA (elt poly1 i)) ; primer vector del poly1
                    (setq tripletsB (elt poly2 j)) ; primer vector del poly2

                    (setq first_element_p1  (elt  tripletsA 0))   ; [i][0] de poly1
                    (setq second_element_p1 (elt  tripletsA 1))   ; [i][1] de poly2
                    (setq third_element_p1  (elt  tripletsA 2))   ; [i][2] de poly1

                    (setq first_element_p2  (elt  tripletsB 0))   ; [j][0] de poly2
                    (setq second_element_p2 (elt  tripletsB 1))   ; [j][1] de poly2
                    (setq third_element_p2  (elt  tripletsB 2))   ; [j][2] de poly2s

                    (setq a (* first_element_p1 first_element_p2))    ; multi de coeficientes numericos
                    (setq b (+ second_element_p1 second_element_p2))  ; suma de exponentes variables x
                    (setq c (+ third_element_p1 third_element_p2))    ; suma de exponentes variables y

                    (vector-push a temporalTriplets) ; insertar numero en el vector temporal
                    (vector-push b temporalTriplets) ; insertar x en el vector temporal
                    (vector-push c temporalTriplets) ; insertar y en el vector temporal

                    (vector-push temporalTriplets tripletsR) ;insertar el vector temporal en el final
                )
            )
            ;;<aux_multi> guarda los polinomios generados por la multiplicacion
            (vector-push tripletsR aux_multi);agrega <tripletsR> al vector <aux_multi>
            ;;para poder tenerlos como polinomios y asi sumar
      )
    )
    
    (setq largo (length aux_multi))
    (if (= largo 1)
        (progn
            (princ "Multiplication: ")
            (write poly1) 
            (princ " * ")
            (write poly2)
            (princ " = ")
            (setq *result* (concatenate 'vector *result* aux_multi))
            (write *result*)
        )
    )
    (if (= largo 2)
        (progn
            (setq poly1R (elt aux_multi 0))
            (setq poly2R (elt aux_multi 1))
            (princ "Multiplication: ")
            (write poly1) 
            (princ " * ")
            (write poly2)
            (suma_resta poly1R poly2R #'+)  
        )
    )
    (if (= largo 3)
        (progn
            (setq poly1R (elt aux_multi 0))
            (setq poly2R (elt aux_multi 1))
            (suma_resta poly1R poly2R #'+)  
            (princ "Multiplication: ")
            (write poly1) 
            (princ " * ")
            (write poly2)
            (setq poly3R (elt aux_multi 2))
            (suma_resta *result* poly3R #'+)
        )
    )
    (if (= largo 4)
        (progn
            (setq poly1R (elt aux_multi 0))
            (setq poly2R (elt aux_multi 1))
            (suma_resta poly1R poly2R #'+)
            (setq poly3R (elt aux_multi 2))
            (suma_resta *result* poly3R #'+)  
            (princ "Multiplication: ")
            (write poly1) 
            (princ " * ")
            (write poly2)
            (setq poly4R (elt aux_multi 3))
            (suma_resta *result* poly4R #'+)
        )
    )
    
          
)

#||Ejecuta la evaluación de un polinomios con valores dados de x, y.||#
(defun evaluation(poly x y)
    (setq result 0) ; valor resultante

    (defvar length_poly (length poly))  ; largo poly

    (setq length_poly (- length_poly 1)) ; largo poly -1

    (loop for i from 0 to length_poly
        do (progn
              (setq tripletsA (elt poly i)) ; primer vector del poly1

              (setq numeral (elt tripletsA 0)) ; valor numerico de poly
              (setq coe_x (elt tripletsA 1))   ; grado exponente x de poly
              (setq coe_y (elt tripletsA 2))   ; grado exponente y de poly

              (setq x_exp (expt x coe_x )) ; valor de x (insertado) elevado al exp x (coe_x)
              (setq y_exp (expt y coe_y )) ; valor de y (insertado) elevado al exp y (coe_y)
              (setq result (+ result (* numeral x_exp y_exp))) ; sumatorio de la evaluación
        )
    )
    (terpri)
    (princ "Evaluation: ")
    (write poly)
    (princ " (x= ")
    (write x)
    (princ " ,y= ")
    (write y)
    (princ ") == ")
    (write result)
    (terpri)
    (setq *x* 0)
    (setq *y* 0)
)


#|Verifica si el polinomio digitado es correcto
Retorna la variable global isCorrect|#

(defun verifyPolynomial(polynomial)
    (setq listnumP '(0 1 2 3 4 5 6 7 8 9 -1 -2 -3 -4 -5 -6 -7 -8 -9)) ;lista de los numeros soportados
    (setq polynomial (concatenate 'string polynomial " | ")) ;se le agrega un barra al final solo para verificar que vengan 3 numeros en la ultima tripleta
    (setq sizePoly (length polynomial)) ;largo del string(polinomio) a verificar
    (setq sizePoly (- sizePoly 1));cantidad de tripletas del polinomio
    (setq amountNumbers 0) ;sirve para verificar si por tripleta solo vienen 3 numeros (los permitidos)
    (setq *isCorrect* 1) ;variable verdadera para el polinimo correcto, se hace cero si esta incorrecto
    (loop for index from 0 to sizePoly 
         do(progn 
            ;;verifica que si la cantidad de numero por tripleta es mayor a 3
            ;;retorna error (nil)
            (if (> amountNumbers 3)
                (progn
                    (princ "    ERROR: la cantidad de numeros en la tripleta es mayor a 3")
                    (terpri)
                    (princ "    Recuerde que la tripleta se forma de la manera (a*x^b*y^c).")
                    (terpri)
                    (princ "    Donde los valores que ingresa son (a b c).")
                    (terpri)
                    (setq *isCorrect* 0)
                    (return *isCorrect*)
                )
            )
            ;;verifica si cuando viene el delimitador '|' hay menos de tres numeros
            (if ( and (char= (char polynomial index)  (code-char 124)) (= amountNumbers 3))
                (progn
                    (setq amountNumbers 0)
                )       
            )
            ;;verifica si es numero
            (if ( and (char/= (char polynomial index) #\-) ( and (char/= (char polynomial index)  #\Space) (char/= (char polynomial index)  (code-char 124)) ))
                    (progn
                       ;si se encuentra en la lista permitida
                       (if (find (digit-char-p (char polynomial index)) listnumP)
                            (setq amountNumbers (+ amountNumbers 1))

                        ;else
                        (progn
                            (princ "    ERROR: el caracter en la posicion: ")
                            (write index)
                            (princ " es invalido.")
                            (terpri)
                            (princ "    Recuerde que el delimitador para separar las tripletas es '|'.")
                            (terpri)
                            (setq *isCorrect* 0)
                            (return *isCorrect*)
                        )
                       )
                    )
            );termina la verificacion de numero
        )
    );termina la verificacion del polinomio
    (if (= *isCorrect* 0)
        (progn
            (princ "    No es valido, digite de nuevo por favor!!")
            (terpri)
        )
    ;else
    (setq *isCorrect* 1)
    );termina la funcion de verificar polinomio
)

#||Toma los valores de X y Y para la evaluacion||#
(defun verifyEvaluation(xstr ystr)
    (setq *isCorrect* 1)
    (if (char= (char xstr 0) #\-)
        (progn
            (setq *x* (-(digit-char-p (char xstr 1))) );indica que es un numero negativo "-#"
        )
        ;;else (si el numero es de dos digitos)
        (if (>= (length xstr) 2) 
            (progn
                (write "ERROR-X. Solo se permite numeros de un digito")
                (terpri)
                (setq *isCorrect* 0)
            )
            ;else
            (setq *x* (digit-char-p (char xstr 0)))
        )
    )
    (if (char= (char ystr 0) #\-)
        (progn
            (setq *y* (-(digit-char-p (char ystr 1))) );indica que es un numero negativo "-#"
        )
        ;;else (si el numero es de dos digitos)
        (if (>= (length ystr) 2) 
            (progn
                (write "ERROR-Y. Solo se permite numeros de un digito")
                (terpri)
                (setq *isCorrect* 0)
            )
            ;else
            (setq *y* (digit-char-p (char ystr 0)))
        )
    )
)

#|Solicita ingresar el polinomio 1 
si este es incorrecto se vuelve a llamar|#
(defun inputPoly1()
    (princ "poli1 == ")       ;pide el primer polinomio
    (setq p1 (read-line))
    (verifyPolynomial p1)     ;verifica si la entrada es buena
    (if (= *isCorrect* 1)
        (addPoly p1 *poly1*)
    ;else
    (inputPoly1)
    )
)

#|Solicita ingresar el polinomio 2
si este es incorrecto se vuelve a llamar|#
(defun inputPoly2()
    (princ "poli2 == ")       ;pide el segundo polinomio
    (setq p2 (read-line))
    (verifyPolynomial p2)     ;verifica si la entrada es buena
    (if (= *isCorrect* 1)
        (addPoly p2 *poly2*)
    ;else
    (inputPoly2)
    )
)

#|Solicita ingresar los valores de "x" y "y"
para la funcion evaluacion, si son incorrectos
se vuelve a llamar|#
(defun inputEval(poly)
    (princ "x == ")       ;pide el valor de x
    (setq valueX (read-line))
    (princ "y == ")       ;pide el valor de y
    (setq valueY (read-line))
    (verifyEvaluation valueX valueY)
    (if (= *isCorrect* 1)
        (evaluation poly *x* *y*)
    ;else
    (inputEval poly)
    )
)

#|Pregunta si el resultado de una operacion
lo quiere evaluar o utilizar para otra funcion|#
(defun reuseResult()
    (setq not 1)
    (terpri)
    (princ "Evaluation polynomial: ")
    (princ *result*)
    (princ " (Y/N) ?: ")
    (setq response (read))
    (case response
        (Y (inputEval *result*))
    );; (evaluation)

    (princ "Quiere realizar otra operacion (+ / - / *) con el resultado anterior (Y/N) ??: ")
    (setq response (read))
    (case response
        (Y (progn
            (inputPoly2)
            (terpri)
            (princ "sign of operation == " )
            (setq operation (read))
            (case operation
                ;;si quiere reutilizar el resultado pregunta por un segundo polinomio
                ;;y realiza la operacion con el resultado y el polinomio digitado
                (+ (progn 
                    (terpri)
                    (princ *result*)
                    (princ " + ")
                    (princ *poly2*)
                    (suma_resta *result* *poly2* #'+)
                    ))       ;; (addition)
                (- (progn
                    (terpri)
                    (princ *result*)
                    (princ " - ")
                    (princ *poly2*)
                    (suma_resta *result* *poly2* #'-)
                    ))   ;; (susbtraction)
                (* (multiplication *result* *poly2*)) ;; (mulplitication)
            )
            )
        )
        (N (progn
            ;;en caso que no quiera realizar otra operacion
            ;;se vuelven a poner en '0' todos los vectores globales
            (setq *result* (make-array 100 :fill-pointer 0))
            (setq *poly1* (make-array 100 :fill-pointer 0))
            (setq *poly2* (make-array 100 :fill-pointer 0))
            (setq not 0) 
            ))
        (n (progn ;por si el usuario digita en minuscula
            ;;en caso que no quiera realizar otra operacion
            ;;se vuelven a poner en '0' todos los vectores globales
            (setq *result* (make-array 100 :fill-pointer 0))
            (setq *poly1* (make-array 100 :fill-pointer 0))
            (setq *poly2* (make-array 100 :fill-pointer 0))
            (setq not 0) 
            ))
    )
    (if (/= not 0)
        (reuseResult)
    ;else
    )
)

#||Principal función del programa. Ejecuta todas las operaciones segun
el usuario requiera.||#
(defun main()

    (princ "------------Welcome to Polynomial Calculator------------")
    (terpri)
    (princ "------------Operacions------------")
    (terpri)
    (princ "------------Addition (+)------------")
    (terpri)
    (princ "------------Subtraction (-)------------")
    (terpri)
    (princ "------------Multiplication (*)------------")
    (terpri)
    (princ "------------Evaluation (e)------------")
    (terpri)
    
    (loop
       

        (inputPoly1)
        (princ "Evaluation polynomial: ")
        (princ *poly1*)
        (princ " (Y/N) ?:  ")
        (setq response (read))
        (case response
            (Y (inputEval *poly1*))
            (y (inputEval *poly1*))
        )
        (terpri)
        (terpri)
        (inputPoly2)
        (terpri)
        (princ "sign of operation == " )
        (setq operation (read))
        
        (case operation
            (+ (progn 
                (terpri)
                (princ *poly1*)
                (princ " + ")
                (princ *poly2*)
                (suma_resta *poly1* *poly2* #'+)
                ))       ;; (addition)
            (- (progn 
                (terpri)
                (princ *poly1*)
                (princ " - ")
                (princ *poly2*)
                (suma_resta *poly1* *poly2* #'-)
                ))   ;; (susbtraction)
            (* (multiplication *poly1* *poly2*)) ;; (mulplitication)
	    )
    
        (reuseResult)
        (terpri)
        
    );fin del loop
    
)

(main)


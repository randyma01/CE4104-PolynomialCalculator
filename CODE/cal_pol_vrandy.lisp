#||
Proyecto: Polynomial Calculator

Autores:  Gustavo Fallas
          Ricardo Chang
          Randy Martínez

Contenidos:
          1. Declaración de Funciones
          2. Invocación de Funciones
||#

#||----------Declaración de Funciones----------||#

#||Inserta un vector en una matriz.||#
(defun addPoly(vec1 polynomial)
    (setq triplets (make-array 4 :fill-pointer 0))
    (setq length_vec1 (length vec1))
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
        (when (>= index length_vec1) (return index))
    )
    (vector-push triplets polynomial)
)

#||Ejecuta la multiplicación de dos polinomios.||#
(defun multiplication(poly1 poly2)
    (setq tripletsR (make-array 100 :fill-pointer 0)) ; vector resultante

    (defvar length_poly1 (length poly1))  ; largo poly1
    (defvar length_poly2 (length poly2))  ; largo poly2

    (setq length_poly1 (- length_poly1 1)) ; largo poly1 -1
    (setq length_poly2 (- length_poly2 1)) ; largo poly2 -1

    (loop for i from 0 to length_poly1
      do (progn

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
      )
    )
    (terpri)
    (write tripletsR)
    (terpri)
)

#||Ejecuta la evaluación de un polinomios con valores dados de x, y.||#
(defun evaluation(poly x y)
    (defvar result) ; valor resultante

    (defvar length_poly (length poly))  ; largo poly

    (setq length_poly (- length_poly 1)) ; largo poly -1

    (loop for i from 0 to length_poly
        do (progn
              (setq tripletsA (elt poly1 i)) ; primer vector del poly1

              (setq numeral (elt tripletsA 0)) ; valor numerico de poly
              (setq coe_x (elt tripletsA 1))   ; grado exponente x de poly
              (setq coe_y (elt tripletsA 2))   ; grado exponente y de poly

              (setq x_exp (expt x coe_x )) ; valor de x (insertado) elevado al exp x (coe_x)
              (setq y_exp (expt y coe_y )) ; valor de y (insertado) elevado al exp y (coe_y)

              (setq result (* numeral x_exp y_exp)) ; sumatorio de la evaluación
        )
    )
    (terpri)
    (write result)
    (terpri)
)



#||Principal función del programa. Ejecuta todas las operaciones segun
el usuario requiera.||#
(defun main()
    (setq poly1 (make-array 100 :fill-pointer 0))
    (setq poly2 (make-array 100 :fill-pointer 0))
    (setq polyEval (make-array 3 :fill-pointer 0))

    (princ "poli1 == ")       ;pide el primer polinomio
    (setq p1 (read-line))
    (addPoly p1 poly1)        ;agrega los numeros_poly1 al vector poly1
    ;(princ "poli2 == ")       ;pide el segundo polinomio
    ;(setq p2 (read-line))
    ;(addPoly p2 poly2)        ;agrega los numeros_poly2 al vector poly2

    ;(princ "sign of operation == ")   ;pide el signo de la operacion
    ;(setq operation (read))

    (princ "polinomio #1: ")
    (write poly1)
    (terpri)
    ;(write operation)
    ;(terpri)
    ;(princ "polinomio #2: ")
    ;(write poly2)
    ;(terpri)
    ;(princ "=")
    ;(terpri)

    ;(case operation
		 ; (+ (format t "addition"))       ;; (addition)
		 ; (- (format t "substraction"))   ;; (susbtraction)
		 ; (* (format t "multiplication")) ;; (mulplitication)
     ; (e (format t "evaluation"))     ;; (evaluation)
	  ;)

    (evaluation poly1 1 1)

    ;(multiplication poly1 poly2)

    ;;;(princ "primer elemento del polinomio #2: ")
    ;;;(setq a (elt poly2 0))
    ;;;(write (elt a 0))
)

#||----------Invocación de Funciones----------||#

(main)

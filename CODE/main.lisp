(defun addPoly(vec1 polynomial)
    (setq triplets (make-array 4 :fill-pointer 0))
    (setq a (length vec1))
    (setq c 0)
    (loop
        (if (char= (char vec1 c)  (code-char 124))
            (progn
                (vector-push triplets polynomial)
                (setq triplets (make-array 4 :fill-pointer 0))
            )
        )
        (if ( and (char/= (char vec1 c) #\-) ( and (char/= (char vec1 c)  #\Space) (char/= (char vec1 c)  (code-char 124)) ))
                 (progn
                    (vector-push (digit-char-p (char vec1 c)) triplets)
                 )
        )
        (if (char= (char vec1 c) #\-)
            (progn
                (vector-push (-(digit-char-p (char vec1 (+ c 1)))) triplets)
               (setq c (+ c 1)) ; +1
            )
        )
        (setq c (+ c 1))
        (when (>= c a) (return c))
    )
    (vector-push triplets polynomial)
)

(defun InputOperands()
    (setq poly1 (make-array 100 :fill-pointer 0))
    (setq poly2 (make-array 100 :fill-pointer 0))
    (setq polyEval (make-array 3 :fill-pointer 0))

    (princ "poli1 == ")       ;pide el primer polinomio
    (setq p1 (read-line))
    (addPoly p1 poly1)        ;agrega los numeros_poly1 al vector poly1
    (princ "poli2 == ")       ;pide el segundo polinomio
    (setq p2 (read-line))
    (addPoly p2 poly2)        ;agrega los numeros_poly2 al vector poly2

    (princ "polinomio #1: ")
    (write poly1)
    (terpri)
    (princ "polinomio #2: ")
    (write poly2)
    (terpri)

    (multiplication poly1 poly2)

    ;;;(princ "primer elemento del polinomio #2: ")
    ;;;(setq a (elt poly2 0))
    ;;;(write (elt a 0))
)



(defun multiplication(poly1 poly2)
    (setq tripletsA (make-array 100 :fill-pointer 0))
    (setq tripletsR (make-array 100 :fill-pointer 0))
    (setq tripletsR (make-array 100 :fill-pointer 0))

    (setq length_poly1 (length poly1))
    (setq length_poly2 (length poly2))

    (setq i 0)  ;filas
    (setq j 0)  ;columnas

    (loop
      (loop

        (setq tripletsA (elt poly1 0)) ; primer vector
        (setq first_element_p1 (elt  (elt poly1 0) i))   ; 0 de poly1
        (setq second_element_p1 (elt (elt poly1 1) i))   ; 1 de poly2
        (setq third_element_p1 (elt  (elt poly1 2) i))   ; 2 de poly1

        (setq a (* first_element_p1 (elt (elt poly2 0) j)))
        (setq b (+ first_element_p1 (elt (elt poly2 1) j)))
        (setq c (+ first_element_p1 (elt (elt poly2 2) j)))

        (vector-push a tripletsR)
        (vector-push b tripletsR)
        (vector-push c tripletsR)

        (setq j (+ j 1))
        (when (>= j length_poly2) (return j))
      )

      (setq i (+ i 1))
      (when (>= i length_poly1) (return i))
    )
    (write tripletsR)
)



;;;==========================================================================;;;
(InputOperands)

(defun capa-visible-p (nombre-capa / layer) 
  (if (setq layer (tblsearch "LAYER" nombre-capa)) 
    (and 
      (= (logand (cdr (assoc 70 layer)) 1) 0) ; No congelada
      (> (cdr (assoc 62 layer)) 0) ; Encendida
    )
    nil
  )
)

(defun c:CO (/ ss p_resultado i ent entData AAA AREAE nombre-capa suma_r suma_c 
             suma_d suma_total ptEtiqueta valor_numerico offset-y texto-upper
            ) 

  (princ "\nSeleccione los textos para contabilizar demanda...")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))

  (if ss 
    (progn 
      ;; Punto donde se colocarán los textos de resultados
      (setq p_resultado (getpoint "\nIndique punto para insertar resultados: "))

      (setq i      0
            suma_r 0
            suma_c 0
            suma_d 0
      )

      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        (setq nombre-capa (cdr (assoc 8 entData)))

        (if (capa-visible-p nombre-capa) 
          (progn 
            (setq AAA (cdr (assoc 1 entData)))
            (setq AREAE (atof AAA))
            (setq valor_numerico (atoi AAA))
            (setq texto-upper (strcase AAA))

            ;; Lógica de conteo (R, C, D, T, TSC, TEC)
            (if 
              (or (= texto-upper "TSC") 
                  (= texto-upper "TEC")
                  (= texto-upper "T.E.C.")
                  (= texto-upper "T.S.C.")
              )
              (setq suma_r (1+ suma_r))
              (if (wcmatch texto-upper "*R*") 
                (setq suma_r (+ suma_r AREAE))
                (if (wcmatch texto-upper "*C*") 
                  (setq suma_c (+ suma_c AREAE))
                  (if (wcmatch texto-upper "*D*") 
                    (if (> valor_numerico 9) 
                      (setq suma_d (1+ suma_d))
                      (setq suma_d (+ suma_d AREAE))
                    )
                    (if (wcmatch texto-upper "*T*") (setq suma_r (+ suma_r AREAE)))
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )

      ;; Generar resultados de texto
      (setq suma_total (+ suma_r suma_c suma_d))
      (setq offset-y 0)

      ;; Texto de TOTAL GENERAL
      (entmakex 
        (list '(0 . "TEXT") 
              '(8 . "CAT_ALIMENTA")
              (cons 10 p_resultado)
              '(40 . 1.5)
              (cons 1 (rtos (fix suma_total) 2 0))
              '(50 . 0.0)
        )
      )
      (setq offset-y 2.5)

      ;; Totales desglosados (Solo si existen)
      (if (> suma_r 0) 
        (progn 
          (entmakex 
            (list '(0 . "TEXT") 
                  '(8 . "CAT_ALIMENTA")
                  (cons 62 1)
                  (cons 10 
                        (list (car p_resultado) (- (cadr p_resultado) offset-y) 0)
                  )
                  '(40 . 1.0)
                  (cons 1 (strcat "R: " (rtos (fix suma_r) 2 0)))
                  '(50 . 0.0)
            )
          )
          (setq offset-y (+ offset-y 2.0))
        )
      )

      (if (> suma_c 0) 
        (progn 
          (entmakex 
            (list '(0 . "TEXT") 
                  '(8 . "CAT_ALIMENTA")
                  (cons 62 3)
                  (cons 10 
                        (list (car p_resultado) (- (cadr p_resultado) offset-y) 0)
                  )
                  '(40 . 1.0)
                  (cons 1 (strcat "C: " (rtos (fix suma_c) 2 0)))
                  '(50 . 0.0)
            )
          )
          (setq offset-y (+ offset-y 2.0))
        )
      )

      (if (> suma_d 0) 
        (entmakex 
          (list '(0 . "TEXT") 
                '(8 . "CAT_ALIMENTA")
                (cons 62 4)
                (cons 10 
                      (list (car p_resultado) (- (cadr p_resultado) offset-y) 0)
                )
                '(40 . 1.0)
                (cons 1 (strcat "D: " (rtos (fix suma_d) 2 0)))
                '(50 . 0.0)
          )
        )
      )

      (princ "\nConteo finalizado sin señalización visual.")
    )
    (princ "\nNo se seleccionaron textos.")
  )
  (princ)
)
; -- Función: MLSum --
;
; USO:
;	 Suma los textos seleccionados y dibuja lineas al punto central
;	 Si la suma esta entre 16 y 18 el texto sera verde, si no sera rojo
;
; POSIBLE MEJORAS:
; Incluir un filtro para capas visibles/no congeladas
; Filtrar las capas de lectura de texto (por ejemplo, solo textos que contengan números)

(defun c:MLSum () 
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))

  (if ss 
    (progn 
      (setq p_centro (getpoint "\nIndica el punto central:"))
      (setq oper       0
            text-found nil
      )
      (setq i 0)

      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        (setq p1 (cdr (assoc 10 entData)))

        (setq AAA (cdr (assoc 1 entData)))
        (setq AREAE (atof AAA))
        (setq oper (+ AREAE oper))
        (setq text-found t)

        (command "_line" p1 p_centro "")

        (setq i (1+ i))
      )

      (if text-found 
        (progn 
          (setq ptEtiqueta (list (+ (car p_centro) 2.0) 
                                 (cadr p_centro)
                                 (caddr p_centro)
                           )
          )
          ; 3 = Verde, 1 = Rojo
          (setq color (if (and (>= oper 16) (<= oper 18)) 3 1))
          (setq textEnt (entmakex 
                          (list (cons 0 "TEXT") 
                                (cons 10 ptEtiqueta)
                                (cons 40 1.5) ; Altura del texto
                                (cons 1 (rtos oper 2 2)) ; Contenido del texto
                                (cons 50 0.0) ; Rotación
                                (cons 62 color) ; Color asignado directamente
                          )
                        )
          )
          (princ (strcat "\nSuma total: " (rtos oper 2 2)))
        )
        (alert "No se encontraron textos válidos para sumar.")
      )
    )
    (princ "\nNo se seleccionaron textos.")
  )

  (princ)
)

(princ "\nComando MLSum cargado.")
(princ "\nEscribe \"MLSum\" para ejecutar.")
(princ)
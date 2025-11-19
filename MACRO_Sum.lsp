; Usa SUM para usar esta MACRO
; Suma los textos seleccionados y dibuja lineas al punto central
;

(defun c:SUM () 
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
          (setq ptEtiqueta p_centro)
          (setq textEnt (entmakex 
                          (list (cons 0 "TEXT")  ; Tipo de entidad
                                (cons 10 ptEtiqueta) ; Punto de inserción
                                (cons 40 1) ; Altura del texto
                                (cons 1 (rtos (fix oper) 2 0)) ; Contenido del texto
                                (cons 50 0.0) ; Rotación
                          )
                        )
          )
        )
        (alert "No se encontraron textos válidos para sumar.")
      )
    )
    (princ "\nNo se seleccionaron textos.")
  )

  (princ)
)

(princ "\nComando SUM cargado.")
(princ "\nEscribe \"SUM\" para ejecutar.")
(princ)

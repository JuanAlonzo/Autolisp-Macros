(defun c:R0 (/ ss i ent entData entType)
  (prompt "\nSelecciona textos (TEXT o MTEXT) para rotar a 0°: ")
  (if (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    (progn
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq entData (entget ent))
        (setq entType (cdr (assoc 0 entData)))
        
        (cond
          ;; Para TEXT tradicional: actualiza el código DXF 50
          ((= entType "TEXT")
           (entmod (subst (cons 50 0.0) (assoc 50 entData) entData)))
          
          ;; Para MTEXT: actualiza rotación (DXF 50) y dirección del vector X (DXF 11)
          ((= entType "MTEXT")
           (setq entData (subst (cons 50 0.0) (assoc 50 entData) entData))
           (if (assoc 11 entData)
             (setq entData (subst '(11 1.0 0.0 0.0) (assoc 11 entData) entData))
           )
           (entmod entData))
        )
      )
      (princ (strcat "\nProceso terminado. Textos modificados: " (itoa (sslength ss))))
    )
    (prompt "\nNo se seleccionaron objetos de texto válidos.")
  )
  (princ)
)
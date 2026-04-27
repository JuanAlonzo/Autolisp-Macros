;; MACRO de revision

;; Sirve para verificar las esquinas y los cruces en una linea de red

(defun c:DET-CRUCES (/ ss i j ent1 ent2 obj1 obj2 int-pt p is-end1 is-end2) 
  (vl-load-com)
  (princ "\n--- DIAGNÓSTICO: Detectando Cruces y Esquinas ---")

  (command "_.-LAYER" "M" "DEBUG_CRUCES" "C" "1" "" "") ; Rojo para cruces
  (command "_.-LAYER" "M" "DEBUG_ESQUINAS" "C" "2" "" "") ; Amarillo para esquinas/T (L)

  (if (setq ss (ssget '((0 . "LINE")))) 
    (progn 
      (setq i 0)
      (while (< i (sslength ss)) 
        (setq ent1 (ssname ss i))
        (setq obj1 (vlax-ename->vla-object ent1))

        (setq j (1+ i))
        (while (< j (sslength ss)) 
          (setq ent2 (ssname ss j))
          (setq obj2 (vlax-ename->vla-object ent2))

          ;; Detectar intersección física
          (setq int-pt (vlax-invoke obj1 'IntersectWith obj2 acExtendNone))

          (if int-pt 
            (while (> (length int-pt) 0) 
              (setq p (list (car int-pt) (cadr int-pt) (caddr int-pt)))

              ;; Verificar si el punto de cruce es un extremo de alguna línea
              (setq is-end1 (or (equal p (vlax-curve-getStartPoint obj1) 1e-4) 
                                (equal p (vlax-curve-getEndPoint obj1) 1e-4)
                            )
              )
              (setq is-end2 (or (equal p (vlax-curve-getStartPoint obj2) 1e-4) 
                                (equal p (vlax-curve-getEndPoint obj2) 1e-4)
                            )
              )

              (if (and (not is-end1) (not is-end2)) 
                ;; CASO A: Cruce real en medio de ambas líneas (+)
                (entmake 
                  (list '(0 . "CIRCLE") 
                        (cons 10 p)
                        (cons 40 0.5)
                        '(8 . "DEBUG_CRUCES")
                  )
                )
                ;; CASO B: Esquina o unión en T (L)
                (entmake 
                  (list '(0 . "CIRCLE") 
                        (cons 10 p)
                        (cons 40 0.3)
                        '(8 . "DEBUG_ESQUINAS")
                  )
                )
              )

              (setq int-pt (cdddr int-pt))
            )
          )
          (setq j (1+ j))
        )
        (setq i (1+ i))
        (if (= 0 (rem i 100)) (princ (strcat "\rAnalizando líneas: " (itoa i))))
      )
      (princ "\n--- Diagnóstico finalizado. Revisa los círculos en el plano. ---")
    )
  )
  (princ)
)
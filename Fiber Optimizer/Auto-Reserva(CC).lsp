;; Macro para utilizar junto con la app Fiber Optimizer
;; Busca el texto que contenga XXm mas cercano y lo reemplaza automaticamente

;; Modo de Uso:
;; - Despues de pasar el Fiber Optimizer y verificar metrados, usar CC
;; - Seleccionar cada texto creado por la aplicacion Fiber Optimizer
;;

;; Consideraciones:
;; El texto que contiene RZ=XXm debe estar de esa forma y debe estar cerca del Text
;; donde esta el original


(defun c:CC (/ entSource dataSource ptSource ssDest i entDest dataDest ptDest dist 
             minDist closestEnt valSource
            ) 
  (princ "\n--- Macro Auto-Reserva: Búsqueda por Proximidad ---")

  (while (setq entSource (car (entsel "\nSeleccione el texto con el valor real (ej. RZ=96m): "))) 
    (setq dataSource (entget entSource))

    (if (member (cdr (assoc 0 dataSource)) '("TEXT" "MTEXT")) 
      (progn 
        (setq valSource (cdr (assoc 1 dataSource)))
        (setq ptSource (cdr (assoc 10 dataSource)))

        ;; Buscar todos los textos que contengan "XXm" en el dibujo
        (setq ssDest (ssget "_X" '((0 . "TEXT,MTEXT") (1 . "*XXm*"))))

        (if ssDest 
          (progn 
            (setq minDist 1e99) ;; Inicializa con una distancia infinita
            (setq closestEnt nil)

            ;; Iterar para encontrar el más cercano
            (setq i 0)
            (repeat (sslength ssDest) 
              (setq entDest (ssname ssDest i))
              (setq dataDest (entget entDest))
              (setq ptDest (cdr (assoc 10 dataDest)))

              (setq dist (distance ptSource ptDest))

              (if (< dist minDist) 
                (progn 
                  (setq minDist dist)
                  (setq closestEnt entDest)
                )
              )
              (setq i (1+ i))
            )

            ;; Actualizar el texto más cercano encontrado
            (if closestEnt 
              (progn 
                (setq dataDest (entget closestEnt))
                (entmod (subst (cons 1 valSource) (assoc 1 dataDest) dataDest))
                (princ 
                  (strcat "\n>>> Actualizado el XXm más cercano a " 
                          (rtos minDist 2 2)
                          " unidades."
                  )
                )
              )
            )
          )
          (princ "\nError: No se encontraron textos con 'XXm' en el dibujo.")
        )
      )
      (princ "\nEl objeto seleccionado no es un texto.")
    )
    (princ "\n------------------------------------------------")
  )
  (princ "\nMacro finalizada.")
  (princ)
)
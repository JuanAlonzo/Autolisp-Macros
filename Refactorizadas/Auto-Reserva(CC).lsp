;; ============================================================================
;; Macro: Auto-Reserva por Proximidad Segura (CC) - Refactorizada
;; Basada en: Fiber Optimizer/Auto-Reserva(CC).lsp
;;
;; Mejoras implementadas:
;; - Radio máximo de seguridad configurable (`[Radio]`, por defecto 50.0m) para evitar
;;   sobreescribir por error un texto `XXm` que esté al otro lado del plano.
;; - Resaltado visual del texto origen y línea temporal (`grdraw`) hacia el `XXm`
;;   actualizado para confirmar visualmente qué etiqueta cambió.
;; - Soporte de Undo (`vla-StartUndoMark`) por cada reemplazo.
;; ============================================================================

(vl-load-com)

(defun c:CC (/ *error* doc continuar sel entSource dataSource valSource ptSource
             ssDest i entDest dataDest ptDest dist minDist closestEnt closestPt nuevoRadio)

  (defun *error* (msg)
    (if entSource  (redraw entSource 4))
    (if closestEnt (redraw closestEnt 4))
    (redraw)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase (vl-princ-to-string msg)) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (null *cc:max-radius*) (setq *cc:max-radius* 50.0))

  (princ "\n=== AUTO-RESERVA POR PROXIMIDAD (CC) ===")
  (setq continuar T)

  (while continuar
    (initget "Radio")
    (setq sel
      (entsel
        (strcat "\nSeleccione texto con valor real (ej. RZ=96m) [Radio] (Máx: "
                (rtos *cc:max-radius* 2 1) "m) <Salir>: "
        )
      )
    )

    (cond
      ((= sel "Radio")
       (setq nuevoRadio
         (getreal
           (strcat "\nIngrese el radio máximo de búsqueda <" (rtos *cc:max-radius* 2 1) ">: ")
         )
       )
       (if (and nuevoRadio (> nuevoRadio 0.0))
         (setq *cc:max-radius* nuevoRadio)
       )
      )

      ((and (listp sel) (setq entSource (car sel)))
       (setq dataSource (entget entSource))
       (if (member (cdr (assoc 0 dataSource)) '("TEXT" "MTEXT"))
         (progn
           (redraw entSource 3)
           (setq valSource (cdr (assoc 1 dataSource))
                 ptSource  (cdr (assoc 10 dataSource))
           )

           ;; Buscar todos los textos que contengan "XXm" en el dibujo
           (if (setq ssDest (ssget "_X" '((0 . "TEXT,MTEXT") (1 . "*XXm*"))))
             (progn
               (setq minDist    1e99
                     closestEnt nil
                     closestPt  nil
                     i          0
               )
               (repeat (sslength ssDest)
                 (setq entDest  (ssname ssDest i)
                       dataDest (entget entDest)
                       ptDest   (cdr (assoc 10 dataDest))
                       dist     (distance ptSource ptDest)
                 )
                 (if (< dist minDist)
                   (setq minDist    dist
                         closestEnt entDest
                         closestPt  ptDest
                   )
                 )
                 (setq i (1+ i))
               )

               (if (and closestEnt (<= minDist *cc:max-radius*))
                 (progn
                   (vla-StartUndoMark doc)
                   (setq dataDest (entget closestEnt))
                   (entmod (subst (cons 1 valSource) (assoc 1 dataDest) dataDest))
                   (entupd closestEnt)
                   (redraw)
                   ;; Vector amarillo conectando el origen con el destino actualizado y resaltado
                   (grdraw ptSource closestPt 2 1)
                   (redraw closestEnt 3)
                   (princ
                     (strcat "\n>>> Actualizado a \"" valSource "\" (Distancia: "
                             (rtos minDist 2 2) "m)."
                     )
                   )
                   (vla-EndUndoMark doc)
                 )
                 (princ
                   (strcat "\n[!] No se encontró ningún 'XXm' dentro del radio de "
                           (rtos *cc:max-radius* 2 1) "m (el más cercano está a "
                           (rtos minDist 2 2) "m). Use opción [Radio] para ampliarlo."
                   )
                 )
               )
             )
             (princ "\n[!] No quedan textos con 'XXm' en el dibujo.")
           )
           (redraw entSource 4)
           (setq entSource nil)
         )
         (princ "\n[!] El objeto seleccionado no es un texto.")
       )
      )

      (T (setq continuar nil))
    )
  )

  (redraw)
  (princ "\nMacro CC finalizada.")
  (princ)
)

(princ "\nComando CC cargado.")
(princ)

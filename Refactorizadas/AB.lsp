;; ============================================================================
;; Macro: Alinear Bloques (AB) - Refactorizada
;; Basada en: Design_FTTH/Align Blocks (AB).lsp
;;
;; Mejoras implementadas:
;; - Reemplaza la llamada recursiva `(C:AB)` al final por un bucle `(while ...)` limpio.
;; - Agrega manejo de errores y marcas de Undo (`vla-StartUndoMark`) por iteración.
;; - Regenera visualización en tiempo real antes de preguntar por la rotación de 180°.
;; ============================================================================

(vl-load-com)

(defun c:AB (/ *error* doc continuar ss pl pl-obj i blk blk-obj pnt closest param tangent ang respuesta)

  (defun *error* (msg)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase (vl-princ-to-string msg)) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq doc       (vla-get-ActiveDocument (vlax-get-acad-object))
        continuar T
  )

  (princ "\n=== ALINEAR BLOQUES A CURVA GUÍA (AB) ===")

  (while continuar
    (princ "\n\nSeleccione los bloques a alinear (o ENTER para terminar): ")
    (if (setq ss (ssget '((0 . "INSERT"))))
      (progn
        (setq pl (car (entsel "\nSeleccione la polilínea, línea, arco o spline guía: ")))
        (if (and pl
                 (wcmatch (cdr (assoc 0 (entget pl))) "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC"))
          (progn
            (vla-StartUndoMark doc)
            (setq pl-obj (vlax-ename->vla-object pl)
                  i      0
            )
            (repeat (sslength ss)
              (setq blk     (ssname ss i)
                    blk-obj (vlax-ename->vla-object blk)
                    pnt     (cdr (assoc 10 (entget blk)))
                    closest (vlax-curve-getclosestpointto pl-obj pnt)
                    param   (vlax-curve-getparamatpoint pl-obj closest)
                    tangent (vlax-curve-getfirstderiv pl-obj param)
                    ang     (+ (atan (cadr tangent) (car tangent)) (/ pi 2.0))
              )
              (vla-put-rotation blk-obj ang)
              (vla-put-insertionpoint blk-obj (vlax-3d-point closest))
              (setq i (1+ i))
            )

            (princ (strcat "\n -> " (itoa (sslength ss)) " bloque(s) alineado(s)."))

            (initget "Si No")
            (setq respuesta (getkword "\n¿Desea invertir los bloques 180 grados? [Si/No] <No>: "))
            (if (= respuesta "Si")
              (progn
                (setq i 0)
                (repeat (sslength ss)
                  (setq blk-obj (vlax-ename->vla-object (ssname ss i)))
                  (vla-put-rotation blk-obj (+ (vla-get-rotation blk-obj) pi))
                  (setq i (1+ i))
                )
                (princ (strcat "\n -> " (itoa (sslength ss)) " bloque(s) rotado(s) 180°."))
              )
            )
            (vla-EndUndoMark doc)
          )
          (princ "\n[!] ERROR: Debe seleccionar una polilínea, línea, arco o spline válidos.")
        )
      )
      (setq continuar nil)
    )
  )

  (princ "\nComando AB finalizado.")
  (princ)
)

(princ "\nComando AB cargado.")
(princ)

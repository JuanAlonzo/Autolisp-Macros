;; ============================================================================
;; Macro: Duplicar Texto con Resaltado Visual (ZZCOPY) - Refactorizada
;; Basada en: Design_FTTH/Duplicar Texto (ZZCopy).lsp
;;
;; Mejoras implementadas:
;; - Resalta visualmente el texto de ORIGEN (`redraw entSource 3`) mientras se
;;   selecciona el texto de DESTINO.
;; - Dibuja una línea guía temporal (`grdraw`) entre el origen y el destino actualizado.
;; - Restaura siempre el resaltado incluso si el usuario cancela con ESC (`*error*`).
;; - Encapsula cada copia en un `UndoMark`.
;; ============================================================================

(vl-load-com)

(defun c:ZZCOPY (/ *error* doc entSource dataSource txtSource ptSource entDest dataDest ptDest)

  (defun *error* (msg)
    (if entSource (redraw entSource 4))
    (redraw)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase (vl-princ-to-string msg)) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\n=== TRANSFERIR VALORES DE TEXTO (ZZCOPY) ===")
  (princ "\n(Presione ENTER o ESC para salir)")

  (while (setq entSource (car (entsel "\n1. Seleccione el texto de ORIGEN <Salir>: ")))
    (setq dataSource (entget entSource))

    (if (member (cdr (assoc 0 dataSource)) '("TEXT" "MTEXT"))
      (progn
        ;; Resaltar el texto origen
        (redraw entSource 3)
        (setq txtSource (cdr (assoc 1 dataSource))
              ptSource  (cdr (assoc 10 dataSource))
        )
        (princ (strcat "\n -> Valor capturado: \"" txtSource "\""))

        (setq entDest (car (entsel "\n2. Seleccione el texto de DESTINO a reemplazar: ")))
        (if (and entDest (setq dataDest (entget entDest)))
          (if (member (cdr (assoc 0 dataDest)) '("TEXT" "MTEXT"))
            (progn
              (vla-StartUndoMark doc)
              (setq ptDest (cdr (assoc 10 dataDest)))
              (entmod (subst (cons 1 txtSource) (assoc 1 dataDest) dataDest))
              (entupd entDest)
              (redraw)
              (grdraw ptSource ptDest 3 1)
              (princ (strcat "\n>>> ¡Texto actualizado a \"" txtSource "\"!"))
              (vla-EndUndoMark doc)
            )
            (princ "\n[!] Error: El objeto de destino no es un texto.")
          )
          (princ "\nOperación de destino omitida.")
        )
        ;; Quitar resaltado del origen
        (redraw entSource 4)
        (setq entSource nil)
      )
      (princ "\n[!] Error: El objeto seleccionado no es un texto válido.")
    )
  )

  (redraw)
  (princ "\nMacro ZZCOPY finalizada.")
  (princ)
)

(princ "\nComando ZZCOPY cargado.")
(princ)

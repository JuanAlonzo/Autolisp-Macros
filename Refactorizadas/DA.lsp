;; ============================================================================
;; Macro: Distancia Acumulada (DA) - Refactorizada
;; Basada en: Design_FTTH/Distance Acumulative (DA).lsp
;;
;; Mejoras implementadas:
;; - Reemplaza la llamada recursiva `(C:DA)` por un bucle `(while ...)` controlado.
;; - Implementa realmente las opciones `[Restablecer/Finalizar]` en el prompt mediante `initget`.
;; - Dibuja vectores temporales en pantalla (`grdraw`) para visualizar el recorrido medido.
;; - Inserta el texto mediante `entmakex` (no falla con estilos de texto de altura fija).
;; ============================================================================

(vl-load-com)

(defun c:DA (/ *error* old_cmdecho continuar medir_tramo pt1 pt2 d d_rounded pt_text)

  (defun *error* (msg)
    (redraw)
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if (not (wcmatch (strcase (vl-princ-to-string msg)) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (graphscr)
  (setq old_cmdecho (getvar "CMDECHO")
        continuar   T
  )
  (setvar "CMDECHO" 0)

  (princ "\n=== DISTANCIA ACUMULADA (DA) ===")

  (while continuar
    (setq pt1 (getpoint "\nSeleccione el primer punto de medición (o ENTER para salir): "))
    (if pt1
      (progn
        (setq d           0.0
              medir_tramo T
        )
        (while medir_tramo
          (initget "Restablecer Finalizar")
          (setq pt2
            (getpoint pt1
              (strcat "\nSiguiente punto [Restablecer/Finalizar] (Acumulado: "
                      (itoa (fix (+ d 0.5))) "m) <Finalizar>: "
              )
            )
          )
          (cond
            ((= pt2 "Restablecer")
             (redraw)
             (setq d 0.0)
             (princ "\n-> Distancia restablecida a 0.")
             (setq pt1 (getpoint "\nSeleccione el nuevo primer punto: "))
             (if (not pt1) (setq medir_tramo nil))
            )
            ((or (null pt2) (= pt2 "Finalizar"))
             (setq medir_tramo nil)
            )
            ((listp pt2)
             (grdraw pt1 pt2 3 1) ; Línea temporal verde punteada
             (setq d           (+ d (distance pt1 pt2))
                   d_rounded   (fix (+ d 0.5))
                   pt1         pt2
             )
             (princ (strcat "\nDistancia acumulada: " (itoa d_rounded) " m"))
            )
          )
        )

        (redraw)
        (if (> d 0.0)
          (progn
            (setq d_rounded (fix (+ d 0.5)))
            (princ (strcat "\n*** Distancia total: " (itoa d_rounded) " unidades ***"))
            (setq pt_text (getpoint "\nSeleccione punto para insertar etiqueta (o ENTER para omitir): "))
            (if pt_text
              (progn
                (entmakex
                  (list '(0 . "TEXT")
                        (cons 10 pt_text)
                        (cons 11 pt_text)
                        '(40 . 1.0)
                        (cons 1 (strcat "DIST: " (itoa d_rounded)))
                        '(50 . 0.0)
                        '(72 . 1) ; Center
                        '(73 . 2) ; Middle
                  )
                )
                (princ "\nTexto insertado correctamente.")
              )
            )
          )
        )
      )
      (setq continuar nil)
    )
  )

  (setvar "CMDECHO" old_cmdecho)
  (princ "\nComando DA finalizado.")
  (princ)
)

(princ "\nComando DA cargado.")
(princ)

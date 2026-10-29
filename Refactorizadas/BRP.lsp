;; ============================================================================
;; Macro: BreakPoint (BRP) - Refactorizada
;; Basada en: MACRO_BreakPoint.lsp
;;
;; Mejoras implementadas:
;; - Reemplaza la llamada recursiva `(C:BRP)` por un bucle `(while ...)` limpio.
;; - Permite salir suavemente presionando ENTER, ESPACIO o ESC sin errores.
;; - Encapsula cada punto roto en su propio UndoMark para poder hacer Ctrl+Z paso a paso.
;; ============================================================================

(vl-load-com)

(defun c:BRP (/ *error* doc old_osmode old_nomutt old_cmdecho
              break_point search_radius selection_set obj_count counter selected_obj total_rotos)

  (defun *error* (msg)
    (if old_osmode  (setvar 'osmode old_osmode))
    (if old_nomutt  (setvar 'nomutt old_nomutt))
    (if old_cmdecho (setvar 'cmdecho old_cmdecho))
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase (vl-princ-to-string msg)) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq doc           (vla-get-activedocument (vlax-get-acad-object))
        old_osmode    (getvar 'osmode)
        old_nomutt    (getvar 'nomutt)
        old_cmdecho   (getvar 'cmdecho)
        search_radius 0.01
        total_rotos   0
  )

  ;; Activar OSNAP útil (Endpoint, Midpoint, Center, Node, Quadrant, Intersection, Insert, Nearest)
  (setvar 'osmode 553)
  (setvar 'cmdecho 0)

  (princ "\n=== BREAK POINT (BRP) ===")
  (princ "\nDesigne puntos para romper líneas/polilíneas/arcos (ENTER o ESC para salir).")

  (while (setq break_point (getpoint "\nDesigne el punto de ruptura <Salir>: "))
    (vla-StartUndoMark doc)
    (setvar 'nomutt 1)

    (setq selection_set
      (ssget "_C"
             (polar break_point (* 1.25 pi) search_radius)
             (polar break_point (* 0.25 pi) search_radius)
             '((0 . "LWPOLYLINE,LINE,ARC,POLYLINE,SPLINE,ELLIPSE"))
      )
    )
    (setvar 'nomutt old_nomutt)

    (if selection_set
      (progn
        (setq obj_count (sslength selection_set)
              counter   0
        )
        (repeat obj_count
          (setq selected_obj (ssname selection_set counter))
          (command "._break" selected_obj "_none" break_point "_none" "@")
          (setq counter (1+ counter))
        )
        (setq total_rotos (+ total_rotos obj_count))
        (princ (strcat "\n -> " (itoa obj_count) " objeto(s) roto(s) en este punto."))
      )
      (princ "\n [!] No se encontraron objetos en el punto seleccionado.")
    )

    (vla-EndUndoMark doc)
  )

  (setvar 'osmode old_osmode)
  (setvar 'nomutt old_nomutt)
  (setvar 'cmdecho old_cmdecho)

  (princ (strcat "\nOperación finalizada. Total objetos rotos: " (itoa total_rotos) "."))
  (princ)
)

(princ "\nComando BRP cargado.")
(princ)

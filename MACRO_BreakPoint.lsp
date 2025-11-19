; -- Funcion BreakPoint (BRP) --
;
; Autor: Desconocido
; Funcionalidades agregadas por Alonso Jaramillo
;
;
; Descripción:
;   Rompe líneas, polilíneas y arcos en un punto específico.
;   Selecciona automáticamente objetos cercanos al punto.
;
; Uso:
; 	Escribe BRP en AutoCAD

(vl-load-com)

(defun brp:error (msg /) 
  (if 
    (not 
      (member msg 
              '("Function cancelled" "quit / exit abort" "console break" "end")
      )
    )
    (princ (strcat "\nError: " msg))
  )

  (if *brp:old_cmdecho* (setvar 'cmdecho *brp:old_cmdecho*))
  (if *brp:old_nomutt* (setvar 'nomutt *brp:old_nomutt*))
  (if *brp:old_osmode* (setvar 'osmode *brp:old_osmode*))

  (if *brp:adoc* 
    (vla-endundomark *brp:adoc*)
  )

  (setq *brp:old_cmdecho* nil
        *brp:old_nomutt*  nil
        *brp:old_osmode*  nil
        *brp:adoc*        nil
  )
  (princ)
)

(defun c:BRP (/ break_point selection_set counter obj_count search_radius old_error 
              selected_obj
             ) 

  (setq old_error *error*
        *error*   brp:error
  )

  (setq *brp:adoc* (vla-get-activedocument (vlax-get-acad-object)))

  ; Save current system variables
  (setq *brp:old_osmode*  (getvar 'osmode)
        *brp:old_nomutt*  (getvar 'nomutt)
        *brp:old_cmdecho* (getvar 'cmdecho)
  )

  (vla-startundomark *brp:adoc*)

  ; Set OSNAP (553 = endpoint, midpoint, center, node, quadrant, intersection, insert, nearest)
  (setvar 'osmode 553)
  (setvar 'cmdecho 0)
  (setvar 'nomutt 1)

  (princ "\nBREAK POINT (BRP)")
  (princ "\nSeleccione el punto donde romper las lineas/polilineas/arcos...")

  (setq break_point (getpoint "\nDesigne el punto de ruptura (o ESC para cancelar): "))

  (if (not break_point) 
    (progn 
      (princ "\nOperacion cancelada: No se selecciono punto.")
      (brp:error "end")
      (exit)
    )
  )
  ; Define the search radius (in drawing units)
  (setq search_radius 0.01)

  (princ "\nBuscando objetos en el punto seleccionado...")

  ; Create a selection set of lines, polylines, arcs, splines, and ellipses near the point
  (setq selection_set (ssget "_C" 
                             (polar break_point (* 1.25 pi) search_radius)
                             (polar break_point (* 0.25 pi) search_radius)
                             '((0 . "LWPOLYLINE,LINE,ARC,POLYLINE,SPLINE,ELLIPSE"))
                      )
  )

  ; Validation: Check if any objects were found
  (if (not selection_set) 
    (progn 
      (princ "\nNo se encontraron objetos en el punto seleccionado.")
      (princ "\nIntente con un punto diferente o verifique que hay objetos en ese lugar.")
      (brp:error "end")
      (exit)
    )
  )

  ; BREAK OBJECTS IN SELECTION SET
  (setq obj_count (sslength selection_set))
  (princ (strcat "\nSe encontraron " (itoa obj_count) " objeto(s)."))
  (princ "\nRompiendo objetos...")

  (setq counter 0)

  (repeat obj_count 
    (setq selected_obj (ssname selection_set counter))

    ; Break the object at the specified point (break @ means break at a point without removing segment)
    (command "._break" selected_obj "_none" break_point "_none" "@")

    (setq counter (1+ counter))
  )

  (princ (strcat "\n" (itoa obj_count) " objeto(s) roto(s) exitosamente."))

  ; Restore original system variables
  (setvar 'osmode *brp:old_osmode*)
  (setvar 'nomutt *brp:old_nomutt*)
  (setvar 'cmdecho *brp:old_cmdecho*)

  ; End undo mark
  (vla-endundomark *brp:adoc*)

  ; Restore error function
  (setq *error* old_error)

  ; Clear temporary global variables
  (setq *brp:old_cmdecho* nil
        *brp:old_nomutt*  nil
        *brp:old_osmode*  nil
        *brp:adoc*        nil
  )

  (princ "\nOperacion completada")
  (C:BRP) ; Recursive call to allow multiple uses without reloading
  (princ)
)


; Permite romper múltiples puntos en secuencia
; (defun c:BRPM (/ continue_break) 

;   (princ "\nBREAK POINT MULTIPLE (BRPM)")
;   (princ "\nRompa multiples puntos. Presione ESC para terminar.")

;   (setq continue_break T)

;   (while continue_break 
;     ; Llamar a la función BRP
;     (if (not (c:BRP)) 
;       (setq continue_break nil)
;     )

;     ; Preguntar si desea continuar
;     (initget "Si No")
;     (if (= (getkword "\n¿Romper otro punto? [Si/No] <Si>: ") "No") 
;       (setq continue_break nil)
;     )
;   )

;   (princ "\nOperacion multiple finalizada.")
;   (princ)
; )


(princ "\n===================================")
(princ "\nComandos Break Point cargado")
(princ "\n  BRP  - Break Point")
; (princ "\n  BRPM - Break Point Multiple")
(princ "\n===================================")
(princ)

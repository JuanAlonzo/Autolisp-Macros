;; ============================================================================
;; Macro: Agregar Texto a Líneas (ADDTXTLINE) - Refactorizada
;; Basada en: Capas/Add Text To Lines (ADDTXTLINE).lsp
;;
;; Mejoras implementadas:
;; - Recuerda el último texto, tamaño y color utilizados en la sesión (`<Valor previo>`).
;; - Reemplaza `(command "._Text" ...)` por `vla-addText` (10x más rápido y nunca falla
;;   si el estilo de texto activo tiene altura fija).
;; - Añade desfase perpendicular del 40% de la altura para no pisar la línea.
;; - Soporta Undo en un solo paso (`vla-StartUndoMark`).
;; ============================================================================

(vl-load-com)

(defun addtxtline:asegurar-capa (nombre color / tbl ed)
  (if (not (setq tbl (tblobjname "LAYER" nombre)))
    (entmake
      (list '(0 . "LAYER")
            '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbLayerTableRecord")
            (cons 2 nombre)
            '(70 . 0)
            (cons 62 color)
      )
    )
    (progn
      (setq ed (entget tbl))
      (setq ed (subst (cons 62 color) (assoc 62 ed) ed))
      (entmod ed)
    )
  )
)

(defun addtxtline:leer-vertices (ent / obj endParam ptList i)
  (setq obj      (vlax-ename->vla-object ent)
        endParam (vlax-curve-getEndParam obj)
        ptList   '()
        i        0
  )
  (if (= (vla-get-ObjectName obj) "AcDbLine")
    (list (vlax-curve-getStartPoint obj) (vlax-curve-getEndPoint obj))
    (progn
      (repeat (1+ (fix endParam))
        (setq ptList (cons (vlax-curve-getPointAtParam obj i) ptList)
              i      (1+ i)
        )
      )
      (reverse ptList)
    )
  )
)

(defun c:ADDTXTLINE (/ *error* doc space input_text input_size input_color color_num
                     layer_name lines i ent pntList k p1 p2 mid ang_rad offset insPt
                     pt3d txtObj total_segments)

  (defun *error* (msg)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase (vl-princ-to-string msg)) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; Valores predeterminados de sesión
  (if (null *addtxt:text*)  (setq *addtxt:text* "1R"))
  (if (null *addtxt:size*)  (setq *addtxt:size* 1.0))
  (if (null *addtxt:color*) (setq *addtxt:color* "Verde"))

  (setq input_text
    (getstring T
      (strcat "\nIngrese el texto a agregar en cada línea <" *addtxt:text* ">: ")
    )
  )
  (if (and input_text (/= input_text ""))
    (setq *addtxt:text* input_text)
  )

  (setq input_size
    (getreal
      (strcat "\nIngrese el tamaño del texto <" (rtos *addtxt:size* 2 2) ">: ")
    )
  )
  (if (and input_size (> input_size 0.0))
    (setq *addtxt:size* input_size)
  )

  (initget "Rojo Verde Azul Amarillo Cyan Magenta Blanco")
  (setq input_color
    (getkword
      (strcat "\nSeleccione color [Rojo/Verde/Azul/Amarillo/Cyan/Magenta/Blanco] <" *addtxt:color* ">: ")
    )
  )
  (if input_color
    (setq *addtxt:color* input_color)
  )

  (setq color_num
    (cond
      ((= *addtxt:color* "Rojo")     1)
      ((= *addtxt:color* "Amarillo") 2)
      ((= *addtxt:color* "Verde")    3)
      ((= *addtxt:color* "Cyan")     4)
      ((= *addtxt:color* "Azul")     5)
      ((= *addtxt:color* "Magenta")  6)
      ((= *addtxt:color* "Blanco")   7)
      (T 3)
    )
  )

  (setq layer_name (strcat "texto_" *addtxt:color*))
  (addtxtline:asegurar-capa layer_name color_num)

  (princ "\nSeleccione las líneas o polilíneas: ")
  (if (setq lines (ssget "_:L" '((0 . "LINE,POLYLINE,LWPOLYLINE"))))
    (progn
      (setq doc   (vla-get-ActiveDocument (vlax-get-acad-object))
            space (if (= 1 (vla-get-ActiveSpace doc))
                    (vla-get-ModelSpace doc)
                    (vla-get-PaperSpace doc)
                  )
      )
      (vla-StartUndoMark doc)

      (setq i              0
            total_segments 0
            offset         (* *addtxt:size* 0.4)
      )

      (repeat (sslength lines)
        (setq ent (ssname lines i))
        (if (setq pntList (addtxtline:leer-vertices ent))
          (progn
            (setq k 0)
            (repeat (1- (length pntList))
              (setq p1      (nth k pntList)
                    p2      (nth (1+ k) pntList)
                    ang_rad (angle p1 p2)
                    mid     (list (/ (+ (car p1) (car p2)) 2.0)
                                  (/ (+ (cadr p1) (cadr p2)) 2.0)
                                  0.0
                            )
              )
              ;; Corregir ángulo para que siempre sea legible
              (if (and (> ang_rad (/ pi 2.0)) (<= ang_rad (* 1.5 pi)))
                (setq ang_rad (+ ang_rad pi))
              )
              (setq insPt  (polar mid (+ ang_rad (/ pi 2.0)) offset)
                    pt3d   (vlax-3d-point insPt)
                    txtObj (vla-addText space *addtxt:text* pt3d *addtxt:size*)
              )
              (vla-put-Layer txtObj layer_name)
              (vla-put-Alignment txtObj 13) ; BottomCenter
              (vla-put-Rotation txtObj ang_rad)
              (vla-put-TextAlignmentPoint txtObj pt3d)

              (setq total_segments (1+ total_segments)
                    k              (1+ k)
              )
            )
          )
        )
        (setq i (1+ i))
      )

      (vla-EndUndoMark doc)
      (princ
        (strcat "\n>>> Texto \"" *addtxt:text* "\" agregado en "
                (itoa total_segments) " segmento(s) en la capa '" layer_name "'."
        )
      )
    )
    (princ "\nOperación cancelada: No se seleccionaron objetos.")
  )
  (princ)
)

(princ "\nComando ADDTXTLINE cargado.")
(princ)

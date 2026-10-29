;; ============================================================================
;; Macro: Medir Líneas y Polilíneas (MIDLINE / DDD) - Refactorizada
;; Basada en:
;;   - Design_FTTH/Medir Lineas (MIDLINE).lsp
;;   - Conteos/Distancia Polilinea (DDD).lsp
;;
;; Mejoras implementadas:
;; - Aplica las mejoras de DDD en MidLine: inserción vía `vla-addText` (inmune a estilos
;;   con altura fija y mucho más rápida que `command "._Text"`).
;; - Desfase perpendicular automático (40% de la altura del texto) y alineación
;;   BottomCenter (13) para que el texto nunca se encime sobre la línea.
;; - Permite elegir entre medir por [Segmentos / Total] y conserva los atajos:
;;     MIDLINE -> Mide cada segmento en capa CAT_DIST POSTE
;;     DDD     -> Mide longitud total de polilínea con decimales reales (ej. 45.20m)
;; ============================================================================

(vl-load-com)

(defun midline:asegurar-capa (nombre color /)
  (if (not (tblsearch "LAYER" nombre))
    (entmake
      (list '(0 . "LAYER")
            '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbLayerTableRecord")
            (cons 2 nombre)
            '(70 . 0)
            (cons 62 color)
      )
    )
  )
)

(defun midline:insertar-etiqueta (space pt-medio ang-rad texto capa estilo altura / offset insPt pt3d txtObj)
  ;; Corregir ángulo para lectura humana (evita texto invertido)
  (if (and (> ang-rad (/ pi 2.0)) (<= ang-rad (* 1.5 pi)))
    (setq ang-rad (+ ang-rad pi))
  )
  ;; Desfase perpendicular (+90°) equivalente al 40% de la altura del texto
  (setq offset (* altura 0.4)
        insPt  (polar pt-medio (+ ang-rad (/ pi 2.0)) offset)
        pt3d   (vlax-3d-point insPt)
        txtObj (vla-addText space texto pt3d altura)
  )
  (vla-put-Layer txtObj capa)
  (if (tblsearch "STYLE" estilo)
    (vla-put-StyleName txtObj estilo)
  )
  (vla-put-Alignment txtObj 13) ; acAlignmentBottomCenter
  (vla-put-Rotation txtObj ang-rad)
  (vla-put-TextAlignmentPoint txtObj pt3d)
  txtObj
)

(defun midline:leer-vertices (ent / obj endParam ptList i)
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

(defun midline:procesar (modo-defecto / *error* doc space target_layer target_style
                         th ss i ent obj pntList k p1 p2 dist_val mid ang_rad
                         len mparam deriv disttot)

  (defun *error* (msg)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase (vl-princ-to-string msg)) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq doc   (vla-get-ActiveDocument (vlax-get-acad-object))
        space (if (= 1 (vla-get-ActiveSpace doc))
                (vla-get-ModelSpace doc)
                (vla-get-PaperSpace doc)
              )
  )

  (setq target_layer "CAT_DIST POSTE")
  (midline:asegurar-capa target_layer 3)

  (setq target_style
    (if (tblsearch "STYLE" "Style-CHAR_FAST_FONT")
      "Style-CHAR_FAST_FONT"
      (getvar "TEXTSTYLE")
    )
  )

  (setq th 1.5)
  (princ (strcat "\nSeleccione líneas o polilíneas a medir [" modo-defecto "]: "))

  (if (setq ss (ssget "_:L" '((0 . "LINE,POLYLINE,LWPOLYLINE"))))
    (progn
      (vla-StartUndoMark doc)
      (setq disttot 0.0
            i       0
      )
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent)
        )
        (if (= modo-defecto "TOTAL")
          ;; Modo longitud total a lo largo de la curva (DDD mejorado)
          (if (vlax-curve-getEndParam obj)
            (progn
              (setq len     (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj))
                    mid     (vlax-curve-getPointAtDist obj (/ len 2.0))
                    mparam  (vlax-curve-getParamAtPoint obj mid)
                    deriv   (vlax-curve-getFirstDeriv obj mparam)
                    ang_rad (angle '(0.0 0.0 0.0) deriv)
                    disttot (+ disttot len)
              )
              (midline:insertar-etiqueta
                space mid ang_rad (strcat (rtos len 2 2) "m") target_layer target_style (getvar "TEXTSIZE")
              )
            )
          )
          ;; Modo por segmento individual (MIDLINE mejorado con desfase y vla-addText)
          (if (setq pntList (midline:leer-vertices ent))
            (progn
              (setq k 0)
              (repeat (1- (length pntList))
                (setq p1       (nth k pntList)
                      p2       (nth (1+ k) pntList)
                      dist_val (distance p1 p2)
                      ang_rad  (angle p1 p2)
                      mid      (list (/ (+ (car p1) (car p2)) 2.0)
                                     (/ (+ (cadr p1) (cadr p2)) 2.0)
                                     0.0
                               )
                      disttot  (+ disttot dist_val)
                )
                (midline:insertar-etiqueta
                  space mid ang_rad (rtos dist_val 2 0) target_layer target_style th
                )
                (setq k (1+ k))
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (vla-EndUndoMark doc)
      (princ (strcat "\nDistancia total medida: " (rtos disttot 2 2) " m"))
    )
    (princ "\nOperación cancelada: No se seleccionaron objetos.")
  )
  (princ)
)

(defun c:MidLine () (midline:procesar "SEGMENTOS"))
(defun c:DDD     () (midline:procesar "TOTAL"))

(princ "\nComandos MIDLINE (por segmentos) y DDD (longitud total) cargados.")
(princ)

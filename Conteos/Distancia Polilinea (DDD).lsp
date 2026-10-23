(defun c:DDD (/ doc space ss i ent obj len mid mparam deriv ang th offset insPt pt 
              txtObj
             ) 
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq space (if (= 1 (vla-get-ActiveSpace doc)) 
                (vla-get-ModelSpace doc)
                (vla-get-PaperSpace doc)
              )
  )

  ;; Crea la capa "DIST POSTE" si no existe (ByLayer)
  (if (not (tblsearch "LAYER" "DIST POSTE")) 
    (entmake 
      (list 
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        '(2 . "DIST POSTE")
        '(70 . 0)
      )
    )
  )

  ;; Altura activa y distancia de separación (desfase respecto a la polilínea)
  (setq th (getvar "TEXTSIZE"))
  (setq offset (* th 0.4)) ; Separación equivalente al 40% de la altura del texto

  ;; Selección de polilíneas
  (princ "\nSeleccione las polilíneas: ")
  (if (setq ss (ssget "_:L" '((0 . "*POLYLINE")))) 
    (repeat (setq i (sslength ss)) 
      (setq ent (ssname ss (setq i (1- i))))
      (setq obj (vlax-ename->vla-object ent))

      (if (vlax-curve-getEndParam obj) 
        (progn 
          ;; Longitud total y punto medio
          (setq len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
          (setq mid (vlax-curve-getPointAtDist obj (/ len 2.0)))
          (setq mparam (vlax-curve-getParamAtPoint obj mid))

          ;; Dirección tangente en el punto medio
          (setq deriv (vlax-curve-getFirstDeriv obj mparam))
          (setq ang (angle '(0.0 0.0 0.0) deriv))

          ;; Corrección para lectura humana (evita texto invertido)
          (if (and (> ang (/ pi 2.0)) (<= ang (* 1.5 pi))) 
            (setq ang (+ ang pi))
          )

          ;; Desfase perpendicular hacia arriba del texto (+90°) para no encimarse
          (setq insPt (polar mid (+ ang (/ pi 2.0)) offset))
          (setq pt (vlax-3d-point insPt))

          ;; Inserción del texto
          (setq txtObj (vla-addText space (strcat (rtos len 2 0) ".00m") pt th))

          ;; Propiedades y alineación: Bottom Center (13) separado de la línea
          (vla-put-Layer txtObj "DIST POSTE")
          (vla-put-Alignment txtObj 13) ; acAlignmentBottomCenter
          (vla-put-Rotation txtObj ang)
          (vla-put-TextAlignmentPoint txtObj pt)
        )
      )
    )
    (princ "\nNo se seleccionaron polilíneas válidas.")
  )
  (princ "\nProceso finalizado.")
  (princ)
)

(princ "\nComando DISTPL cargado.")
(princ "\nEscriba DISTPL en la línea de comandos para ejecutar.")
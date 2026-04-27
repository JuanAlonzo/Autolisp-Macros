; -- Funcion MedirLineas (MLINE) --
;
; Funcionalidad mejorada de la macro original MLINEA.
;
; Notas:
;	 Mide la longitud de cada segmento de lineas y polilineas seleccionadas.
;	 Coloca el valor de la longitud en el punto medio de cada segmento.
;	 El texto se orienta según el ángulo del segmento.
;	 Crea una capa llamada "dist" para los textos de distancia.
;
; Funciones adicionales:
;	Verifica si la capa "CAT_DIST POSTE" existe; si no, crea una capa "dist".
;	Verifica si el estilo de texto "Style-CHAR_FAST_FONT" existe; si
;	si no, usa el estilo de texto actual.
;

(vl-load-com)

(defun c:MidLine (/ disttot lines ctr len ent pntList ptCntr fpoint epoint dist_val 
                  ang_rad ang txt_ins target_layer target_style
                 ) 
  (setvar "luprec" 2)

  (if (tblsearch "LAYER" "CAT_DIST POSTE") 
    (progn 
      (setq target_layer "CAT_DIST POSTE")
      (princ "\nUsando capa existente: CAT_DIST POSTE")
    )
    (progn 
      (Create_Layer "dist" 3)
      (setq target_layer "dist")
      (princ "\nCapa CAT_DIST POSTE no encontrada. Usando capa: dist")
    )
  )

  (if (tblsearch "STYLE" "Style-CHAR_FAST_FONT") 
    (progn 
      (setq target_style "Style-CHAR_FAST_FONT")
      (princ "\nUsando estilo de texto: Style-CHAR_FAST_FONT")
    )
    (progn 
      (setq target_style (getvar "TEXTSTYLE"))
      (princ 
        (strcat "\nEstilo Style-CHAR_FAST_FONT no encontrado. Usando estilo actual: " 
                target_style
        )
      )
    )
  )

  (setq disttot 0)
  (setq lines (ssget (list (cons 0 "LINE,POLYLINE,LWPOLYLINE"))))
  (setq ctr 0)
  (if (/= lines nil) 
    (progn 
      (setq len (sslength lines))
      (repeat len 
        (setq ent (ssname lines ctr))

        (if (and ent (setq pntList (ReadPline ent))) 
          (progn 
            (setq ptCntr 0)
            (repeat (1- (length pntList)) 
              (setq fpoint (nth ptCntr pntList))
              (setq epoint (nth (1+ ptCntr) pntList))
              (setq dist_val (distance fpoint epoint))
              (setq ang_rad (angle fpoint epoint))
              (setq ang (* (/ ang_rad pi) 180))
              (if (and (>= ang 90) (<= ang 270)) 
                (setq ang (rtos (+ ang 180)))
                (setq ang (rtos ang))
              )
              (setq txt_ins (MidPoint fpoint epoint))
              (command "._Text" 
                       "s"
                       target_style
                       "j"
                       "bc"
                       txt_ins
                       "1.5"
                       ang
                       (rtos dist_val 2 0)
              )

              (command "._Change" (entlast) "" "p" "la" target_layer "")
              (setq disttot (+ disttot dist_val))
              (setq ptCntr (1+ ptCntr))
            )
          )
        )
        (setq ctr (1+ ctr))
      )
      (princ (strcat "\nDistancia total: " (rtos disttot 2 2) " unidades"))
    )
    (princ "\nOperación cancelada: No se seleccionaron objetos.")
  )
  (princ)
)

(princ "\nComando MIDLINE cargado.")
(princ "\nEscribe \"MIDLINE\" para medir cada segmento de la linea.")
(princ)

(defun MidPoint (midp_fpo midp_spo / midp_mpo) 
  (setq midp_mpo (list (/ (+ (car midp_fpo) (car midp_spo)) 2) 
                       (/ (+ (cadr midp_fpo) (cadr midp_spo)) 2)
                 )
  )
)

(defun Create_Layer (lay_layn lay_laycol /) 
  (if (= (tblsearch "Layer" lay_layn) nil) 
    (command "._Layer" "n" lay_layn "c" lay_laycol lay_layn "")
    (command "._Layer" "t" lay_layn "on" lay_layn "c" lay_laycol lay_layn "")
  )
  (princ)
)

(defun ReadPline (imp_Ent / glb_obj glb_PntCnt returnPTList ptCntr glb_oName 
                  glb_OClosed glb_EnDetails big_Point3d end_Point3d glb_2dDist 
                  old_Point cur_Point3d
                 ) 
  (setq glb_obj (vlax-ename->vla-object imp_Ent))
  (setq glb_PntCnt (vlax-curve-getEndParam glb_obj))
  (setq returnPTList '())
  (setq ptCntr 1)
  (setq glb_oName (vlax-get-property glb_obj 'ObjectName))
  (setq glb_OClosed nil)
  (if (= glb_oName "AcDbLine") 
    (progn 
      (setq glb_EnDetails (entget imp_Ent))
      (setq big_Point3d (cdr (assoc 10 glb_EnDetails)))
      (setq end_Point3d (cdr (assoc 11 glb_EnDetails)))
      (setq returnPTList (append returnPTList (list big_Point3d)))
      (setq returnPTList (append returnPTList (list end_Point3d)))
    )
    (progn 
      (setq glb_OClosed (vlax-curve-isClosed glb_obj))
      (setq glb_2dDist 0)
      (setq old_Point nil)
      (repeat (1+ (fix glb_PntCnt)) 
        (setq cur_Point3d (vlax-curve-getPointAtParam glb_obj (1- ptCntr)))
        (setq returnPTList (append returnPTList (list cur_Point3d)))
        (setq ptCntr (1+ ptCntr))
      )
    )
  )
  (setq return returnPTList)
)
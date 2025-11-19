; -- Macro AgregarTextoALineas (ADDTXTLINE) --
;
; Autor: Alonso Jaramillo
;
; Uso: 
;	Agrega texto en el punto medio de cada segmento de líneas o polilíneas seleccionadas.
;	El texto se orienta según el ángulo del segmento para mejorar la legibilidad.
;	Se puede modificar el tamaño y color del texto.
;
; Utilidad:
;	Útil para anotar segmentos de líneas o polilíneas en planos u otros dibujos técnicos.
;	Facilita la identificación y documentación de elementos lineales en el dibujo.
;	El texto se coloca en una capa específica según el color seleccionado para mejor organización.
;

(vl-load-com)

(defun c:ADDTXTLINE (/ user_text text_color text_size lines ctr len ent pntList 
                     ptCntr fpoint epoint ang_rad ang txt_ins layer_name
                    ) 

  (setvar "luprec" 2)

  (setq user_text (getstring T "\nIngrese el texto a agregar en cada linea: "))

  (if (or (= user_text nil) (= user_text "")) 
    (progn 
      (princ "\nOperación cancelada: No se ingreso texto.")
      (exit)
    )
  )

  (setq text_size (getreal "\nIngrese el tamaño del texto <1.0>: "))

  (if (= text_size nil) 
    (setq text_size 1)
  )

  (if (<= text_size 0) 
    (progn 
      (princ "\nError: El tamaño debe ser mayor que 0. Usando tamaño predeterminado 1")
      (setq text_size 1)
    )
  )

  (initget "Rojo Verde Azul Amarillo Cyan Magenta Blanco")
  (setq text_color (getkword "\nSeleccione el color del texto [Rojo/Verde/Azul/Amarillo/Cyan/Magenta/Blanco] <Verde>: "))

  (if (= text_color nil) 
    (setq text_color "Verde")
  )

  (setq color_num (cond 
                    ((= text_color "Rojo") 1)
                    ((= text_color "Verde") 3)
                    ((= text_color "Azul") 5)
                    ((= text_color "Amarillo") 2)
                    ((= text_color "Cyan") 4)
                    ((= text_color "Magenta") 6)
                    ((= text_color "Blanco") 7)
                    (T 3) ; Verde por defecto
                  )
  )

  (setq layer_name (strcat "texto_" text_color))
  (Create_Layer layer_name color_num)

  (princ "\nSeleccione las lineas/polilineas: ")
  (setq lines (ssget (list (cons 0 "LINE,POLYLINE,LWPOLYLINE"))))
  (setq ctr 0)

  (if (/= lines nil) 
    (progn 
      (setq len (sslength lines))
      (setq total_segments 0)

      (repeat len 
        (setq ent (ssname lines ctr))

        (if (and ent (setq pntList (ReadPline ent))) 
          (progn 
            (setq ptCntr 0)
            (repeat (1- (length pntList)) 
              (setq fpoint (nth ptCntr pntList))
              (setq epoint (nth (1+ ptCntr) pntList))

              ; Calcula el ángulo en radianes y luego lo convierte a grados
              (setq ang_rad (angle fpoint epoint))
              (setq ang (* (/ ang_rad pi) 180))

              ; Ajusta el ángulo para que el texto sea legible
              (if (and (>= ang 90) (<= ang 270)) 
                (setq ang (rtos (+ ang 180)))
                (setq ang (rtos ang))
              )

              (setq txt_ins (MidPoint fpoint epoint))

              (command "._Text" "j" "bc" txt_ins text_size ang user_text)

              (command "._Change" (entlast) "" "p" "la" layer_name "")

              (setq total_segments (1+ total_segments))
              (setq ptCntr (1+ ptCntr))
            )
          )
        )
        (setq ctr (1+ ctr))
      )
      (princ 
        (strcat "\nTexto agregado \"" 
                user_text
                "\" en "
                (itoa total_segments)
                " segmento(s)."
        )
      )
    )
    (princ "\nOperacion cancelada: No se seleccionaron objetos.")
  )
  (princ)
)

(princ "\nComando ADDTXTLINE cargado.")
(princ "\nEscribe \"ADDTXTLINE\" para agregar texto a lineas/polilineas.")
(princ)

; FUNCIONES AUXILIARES
;
; Calcula el punto medio entre dos puntos 2D
;
(defun MidPoint (midp_fpo midp_spo / midp_mpo) 
  (setq midp_mpo (list 
                   (/ (+ (car midp_fpo) (car midp_spo)) 2)
                   (/ (+ (cadr midp_fpo) (cadr midp_spo)) 2)
                 )
  )
)

; Crea una capa si no existe, o la activa y cambia su color si ya existe
;
(defun Create_Layer (lay_layn lay_laycol /) 
  (if (= (tblsearch "Layer" lay_layn) nil) 
    (command "._Layer" "n" lay_layn "c" lay_laycol lay_layn "")
    (command "._Layer" "t" lay_layn "on" lay_layn "c" lay_laycol lay_layn "")
  )
  (princ)
)

; Lee los puntos de una línea o polilínea y devuelve una lista de puntos 3D
;
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

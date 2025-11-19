; -- Funcion AlinearBloques (AB) --
;
; Autor: Alonso Jaramillo
;
; USO:
;	Alinea bloques a lo largo de una polilinea guia.
;	Los bloques se rotan para seguir la tangente de la polilinea.
;	Adicionalmente, se ofrece la opcion de rotar los bloques 180 grados.
; 
; Notas:
; - Selecciona los bloques a alinear y luego la polilinea guia.
; - La polilinea puede ser una linea, arco, spline o polilinea.
; - Asegurate de que los bloques tengan un punto de insercion adecuado.
; 

(defun c:AB (/ ss pl pl-obj i blk pnt param tangent ang new-pnt) 
  (princ "\nSeleccione los bloques a alinear: ")
  (setq ss (ssget '((0 . "INSERT"))))

  (if ss 
    (progn 
      (princ "\nSeleccione la polilinea guia: ")
      (setq pl (car (entsel)))

      (if 
        (and pl 
             (or (= (cdr (assoc 0 (entget pl))) "LWPOLYLINE") 
                 (= (cdr (assoc 0 (entget pl))) "POLYLINE")
                 (= (cdr (assoc 0 (entget pl))) "SPLINE")
                 (= (cdr (assoc 0 (entget pl))) "LINE")
                 (= (cdr (assoc 0 (entget pl))) "ARC")
             )
        )
        (progn 
          (setq pl-obj (vlax-ename->vla-object pl))

          (setq i 0)
          (repeat (sslength ss) 
            (setq blk (ssname ss i))
            (setq pnt (cdr (assoc 10 (entget blk))))

            (setq param (vlax-curve-getparamatpoint pl-obj 
                                                    (vlax-curve-getclosestpointto pl-obj 
                                                                                  pnt
                                                    )
                        )
            )

            (setq tangent (vlax-curve-getfirstderiv pl-obj param))

            ;; Calcular el ángulo de rotación y agregar 90 grados (pi/2 radianes)
            (setq ang (+ (atan (cadr tangent) (car tangent)) (/ pi 2)))

            (setq new-pnt (vlax-curve-getpointatparam pl-obj param))

            (vla-put-rotation (vlax-ename->vla-object blk) ang)
            (vla-put-insertionpoint (vlax-ename->vla-object blk) 
                                    (vlax-3d-point new-pnt)
            )

            (setq i (1+ i))
          )
          (princ 
            (strcat "\n" 
                    (itoa (sslength ss))
                    " bloques alineados con la polilinea."
            )
          )

          (initget "Si No")
          (setq respuesta (getkword "\n¿Desea rotar los bloques 180 grados? [Si/No] <No>: "))

          (if (= respuesta "Si") 
            (progn 
              (setq i 0)
              (repeat (sslength ss) 
                (setq blk (ssname ss i))
                (setq blk-obj (vlax-ename->vla-object blk))
                (setq ang-actual (vla-get-rotation blk-obj))
                (vla-put-rotation blk-obj (+ ang-actual pi))
                (setq i (1+ i))
              )
              (princ 
                (strcat "\n" (itoa (sslength ss)) " bloques rotados 180 grados.")
              )
            )
            (princ "\nNo se rotaron los bloques.")
          )
        )
        (princ "\nERROR: Debe seleccionar una polilinea, linea, arco o spline validos.")
      )
    )
    (princ "\nNo se seleccionaron bloques.")
  )
  (princ "\nOperacion completada")
  ; Recursive call to allow multiple uses without reloading
  (C:AB)
  (princ)
)

(princ "\nComando AB cargado.")
(princ "\nEscribe \"AB\" para ejecutar la funcion AlinearBloques.")

; -- Función: PolErase --
;
; USO:
;	 Borra objetos usando una polilínea como límite de selección.
;	 Permite tres modos: Ventana, Captura o Recorte.
;

(vl-load-com)

(defun c:PolErase (/ pol mode vrt lyr pto ssx ssv ssc n ent is_interior old_error 
                   old_cmdecho old_highlight old_ucsfollow
                  ) 

  ; Auxiliar function to get DXF code value
  (defun dxf (code ent) 
    (cdr (assoc code (entget ent)))
  )

  ; Save original system variables
  (setq old_error *error*)
  (setq old_cmdecho   (getvar "cmdecho")
        old_highlight (getvar "highlight")
        old_ucsfollow (getvar "ucsfollow")
  )

  ; Config system variables for the operation
  (setvar "cmdecho" 0)
  (setvar "ucsfollow" 0)

  ; Initialize undo and UCS
  (command "_undo" "_begin")
  (command "_ucs" "_world")

  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort"))) 
      (princ (strcat "\nError: " msg))
    )
    (command "_ucs" "_prev")
    (command "_undo" "_end")
    (setvar "ucsfollow" old_ucsfollow)
    (setvar "highlight" old_highlight)
    (setvar "cmdecho" old_cmdecho)
    (setq *error* old_error)
    (princ)
  )

  (princ "\nPOLERASE")
  (princ "\nSeleccione una polilinea como limite de borrado...")

  (while 
    (not 
      (or 
        (and 
          (setq pol (car (entsel "\nDesigne la polilinea de borde (o ESC para cancelar): ")))
          (= (dxf 0 pol) "LWPOLYLINE")
        )
        (progn 
          (princ "\nOperacion cancelada.")
          (setq pol nil)
          (exit)
        )
      )
    )
    (if pol 
      (princ "\nERROR: Debe seleccionar una polilinea (LWPOLYLINE). Intente nuevamente.")
    )
  )

  (setq vrt (LstVrt2LWPol pol))
  (setq lyr (append '((-4 . "<or")) 
                    (mapcar (function (lambda (n) (cons 8 n))) (LstLyrUtlAct))
                    '((-4 . "or>"))
            )
  )

  (princ "\n\nModos de borrado disponibles:")
  (princ "\n  - Ventana: Borra solo objetos completamente dentro de la polilinea")
  (princ "\n  - Captura: Borra objetos que crucen o esten dentro de la polilinea")
  (princ "\n  - Recorte: Borra y recorta objetos segun el lado seleccionado")

  (initget "Ventana Captura Recorte")
  (setq mode (getkword "\nSeleccione el modo [Ventana/Captura/Recorte] <Recorte>: "))

  (if (= mode nil) 
    (setq mode "Recorte")
  )

  (princ (strcat "\nModo seleccionado: " mode))

  (initget 1)
  (setq pto (getpoint "\nDesigne un punto en el lado a borrar/recortar: "))

  (if (not pto) 
    (progn 
      (princ "\nERROR: No se designo un punto valido.")
      (*error* "Function cancelled")
      (exit)
    )
  )
  (command "_point" pto)

  (setq ssx (ssget "_x" lyr))
  (setq ssv (ssget "_wp" vrt))
  (setq ssc (ssget "_cp" vrt))

  ; Remove the polyline from the selections
  (if ssx (setq ssx (ssdel pol ssx)))
  (if ssc (setq ssc (ssdel pol ssc)))

  (setq is_interior nil)
  (setq n -1)

  ; Verify if the point is inside the polyline
  (while (setq ent (ssname ssc (setq n (1+ n)))) 
    (if (= (dxf 5 (entlast)) (dxf 5 ent)) 
      (setq is_interior T)
    )
  )

  (princ "\nProcesando borrado...")

  (cond 
    ((and (or (= mode "Ventana") (= mode "Recorte")) is_interior)
     (if ssv 
       (progn 
         (command "_erase" ssv "")
         (princ 
           (strcat "\n" 
                   (itoa (sslength ssv))
                   " objeto(s) borrado(s) [Modo: "
                   mode
                   " - Interior]"
           )
         )
       )
       (princ "\nNo hay objetos para borrar dentro de la polilinea.")
     )
    )

    ((or (= mode "Ventana") (= mode "Recorte"))
     (if (and ssx ssc) 
       (progn 
         (command "_erase" ssx "_remove" ssc "")
         (princ (strcat "\nObjetos borrados [Modo: " mode " - Exterior]"))
       )
       (princ "\nNo hay objetos para borrar fuera de la polilinea.")
     )
    )

    ((and (= mode "Captura") is_interior)
     (if ssc 
       (progn 
         (command "_erase" ssc "")
         (princ 
           (strcat "\n" 
                   (itoa (sslength ssc))
                   " objeto(s) borrado(s) [Modo: Captura - Interior]"
           )
         )
       )
       (princ "\nNo hay objetos para borrar.")
     )
    )

    ((= mode "Captura")
     (if ssx 
       (progn 
         (command "_erase" ssx)
         (if ssv 
           (command "_remove" ssv "")
           (command "")
         )
         (princ "\nObjetos borrados [Modo: Captura - Exterior]")
       )
       (princ "\nNo hay objetos para borrar.")
     )
    )
  )

  (if (= mode "Recorte") 
    (progn 
      (princ "\nAplicando recorte...")

      (if (findfile "extrim.lsp") 
        (progn 
          (load "extrim")
          (if (not (member 'etrim '())) 
            (etrim pol pto)
            (princ "\nAdvertencia: La funcion etrim no esta disponible.")
          )
        )
        (princ "\nAdvertencia: No se encontro el archivo extrim.lsp. Recorte cancelado.")
      )
    )
  )

  (command "_ucs" "_prev")
  (command "_undo" "_end")
  (setvar "ucsfollow" old_ucsfollow)
  (setvar "highlight" old_highlight)
  (setvar "cmdecho" old_cmdecho)
  (setq *error* old_error)

  (princ "\nOperacion completada")
  (princ)
)

(princ "\nComando POLERASE cargado.")
(princ "\nEscribe \"POLERASE\" para ejecutar.")

; Get vertex list from LWPOLYLINE
(defun LstVrt2LWPol (pol / ass lst) 
  (foreach ass (entget pol) 
    (if (= (car ass) 10) 
      (setq lst (cons (cdr ass) lst))
    )
  )
  (reverse lst)
)

; Get list of used and activated layers
(defun LstLyrUtlAct (/ lyr lst) 
  (setq lyr (tblnext "layer" T))
  (while lyr 
    (if 
      (and 
        (= (logand (cdr (assoc 70 lyr)) 1) 0)
        (> (cdr (assoc 62 lyr)) 0)
      )
      (setq lst (cons (cdr (assoc 2 lyr)) lst))
    )
    (setq lyr (tblnext "layer"))
  )
  (acad_strlsort lst)
)

(princ)

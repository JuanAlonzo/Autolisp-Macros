;; ============================================================================
;; Macro: PolErase (Borrado y Recorte por Polilínea) - Refactorizada
;; Basada en: MACRO_PolErase.lsp
;;
;; Mejoras implementadas:
;; - Elimina la creación de entidades `POINT` residuales (`(command "_point" pto)`)
;;   que quedaban ensuciando el dibujo cuando el clic era exterior.
;; - Implementa el algoritmo matemático Ray-Casting (`polerase:punto-en-poligono-p`)
;;   para determinar instantáneamente si el punto elegido es interior o exterior.
;; ============================================================================

(vl-load-com)

;; Determina matemáticamente si un punto 2D está dentro de un polígono (Ray-Casting)
(defun polerase:punto-en-poligono-p (pt vrt / x y inside p1 p2 x1 y1 x2 y2 intersect)
  (setq x      (car pt)
        y      (cadr pt)
        inside nil
        p1     (last vrt)
  )
  (foreach p2 vrt
    (setq x1 (car p1)
          y1 (cadr p1)
          x2 (car p2)
          y2 (cadr p2)
    )
    (if (and (/= (> y1 y) (> y2 y))
             (< x (+ (* (/ (- x2 x1) (- y2 y1)) (- y y1)) x1)))
      (setq inside (not inside))
    )
    (setq p1 p2)
  )
  inside
)

(defun polerase:vertices-lwpol (pol / ass lst)
  (foreach ass (entget pol)
    (if (= (car ass) 10)
      (setq lst (cons (cdr ass) lst))
    )
  )
  (reverse lst)
)

(defun polerase:capas-activas (/ lyr lst)
  (setq lyr (tblnext "layer" T))
  (while lyr
    (if (and (= (logand (cdr (assoc 70 lyr)) 1) 0)
             (> (cdr (assoc 62 lyr)) 0))
      (setq lst (cons (cdr (assoc 2 lyr)) lst))
    )
    (setq lyr (tblnext "layer"))
  )
  (acad_strlsort lst)
)

(defun c:PolErase (/ *error* pol mode vrt lyr pto ssx ssv ssc is_interior
                   old_cmdecho old_highlight old_ucsfollow)

  (defun *error* (msg)
    (if (not (wcmatch (strcase (vl-princ-to-string msg)) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (command "_ucs" "_prev")
    (command "_undo" "_end")
    (if old_ucsfollow (setvar "ucsfollow" old_ucsfollow))
    (if old_highlight (setvar "highlight" old_highlight))
    (if old_cmdecho   (setvar "cmdecho" old_cmdecho))
    (princ)
  )

  (setq old_cmdecho   (getvar "cmdecho")
        old_highlight (getvar "highlight")
        old_ucsfollow (getvar "ucsfollow")
  )

  (setvar "cmdecho" 0)
  (setvar "ucsfollow" 0)

  (command "_undo" "_begin")
  (command "_ucs" "_world")

  (princ "\n=== POLERASE ===")
  (princ "\nSeleccione una polilínea cerrada como límite de borrado...")

  (while
    (not
      (or
        (and
          (setq pol (car (entsel "\nDesigne la polilínea de borde (o ESC para cancelar): ")))
          (= (cdr (assoc 0 (entget pol))) "LWPOLYLINE")
        )
        (progn
          (princ "\nOperación cancelada.")
          (setq pol nil)
          (*error* "Cancel")
          (exit)
        )
      )
    )
    (if pol
      (princ "\n[!] ERROR: Debe seleccionar una polilínea (LWPOLYLINE). Intente nuevamente.")
    )
  )

  (setq vrt (polerase:vertices-lwpol pol))
  (setq lyr
    (append '((-4 . "<or"))
            (mapcar (function (lambda (n) (cons 8 n))) (polerase:capas-activas))
            '((-4 . "or>"))
    )
  )

  (initget "Ventana Captura Recorte")
  (setq mode (getkword "\nSeleccione el modo [Ventana/Captura/Recorte] <Recorte>: "))
  (if (null mode) (setq mode "Recorte"))

  (initget 1)
  (setq pto (getpoint "\nDesigne un punto en el lado a borrar/recortar: "))

  ;; Verificación matemática sin crear entidades POINT residuales
  (setq is_interior (polerase:punto-en-poligono-p pto vrt))

  (setq ssx (ssget "_x" lyr)
        ssv (ssget "_wp" vrt)
        ssc (ssget "_cp" vrt)
  )

  ;; Excluir la polilínea de borde de las selecciones
  (if ssx (setq ssx (ssdel pol ssx)))
  (if ssc (setq ssc (ssdel pol ssc)))

  (princ (strcat "\nProcesando borrado (" (if is_interior "Interior" "Exterior") ")..."))

  (cond
    ((and (or (= mode "Ventana") (= mode "Recorte")) is_interior)
     (if ssv
       (progn
         (command "_erase" ssv "")
         (princ (strcat "\n" (itoa (sslength ssv)) " objeto(s) borrado(s) [Modo: " mode " - Interior]"))
       )
       (princ "\nNo hay objetos para borrar dentro de la polilínea.")
     )
    )

    ((or (= mode "Ventana") (= mode "Recorte"))
     (if (and ssx ssc)
       (progn
         (command "_erase" ssx "_remove" ssc "")
         (princ (strcat "\nObjetos borrados [Modo: " mode " - Exterior]"))
       )
       (princ "\nNo hay objetos para borrar fuera de la polilínea.")
     )
    )

    ((and (= mode "Captura") is_interior)
     (if ssc
       (progn
         (command "_erase" ssc "")
         (princ (strcat "\n" (itoa (sslength ssc)) " objeto(s) borrado(s) [Modo: Captura - Interior]"))
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
          (if etrim
            (etrim pol pto)
            (princ "\nAdvertencia: La función etrim no está disponible.")
          )
        )
        (princ "\nAdvertencia: No se encontró extrim.lsp. Recorte omitido.")
      )
    )
  )

  (command "_ucs" "_prev")
  (command "_undo" "_end")
  (setvar "ucsfollow" old_ucsfollow)
  (setvar "highlight" old_highlight)
  (setvar "cmdecho" old_cmdecho)

  (princ "\nOperación completada.")
  (princ)
)

(princ "\nComando POLERASE cargado.")
(princ)

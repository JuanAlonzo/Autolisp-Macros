; Seleccionar dentro de una polilínea cerrada
; Seleccionar una polilínea cerrada y se seleccionarán todos los objetos dentro de ella

(defun C:ICS (/ en ss lst ssall tmp head) 
  (vl-load-com)
  (if 
    (and (setq en (car (entsel "\nSelect contorno: "))) 
         (wcmatch (cdr (assoc 0 (entget en))) "*POLYLINE")
    )
    (progn 
      (setq lst (ACET-GEOM-OBJECT-POINT-LIST en 1e-3))
      (setq lst (mapcar '(lambda (x) (list (car x) (cadr x))) lst))
      (while lst 
        (setq head (car lst)
              tmp  (cons head tmp)
              lst  (vl-remove-if 
                     '(lambda (pt) (equal pt head 1e-6))
                     (cdr lst)
                   )
        )
      )
      (setq lst (reverse tmp))
      (ACET-SS-ZOOM-EXTENTS (ACET-LIST-TO-SS (list en)))
      (command "_.Zoom" "0.95x")
      (if (setq ss (ssget "_CP" lst)) 
        (sssetfirst ss ss)
      )
    )
  )
)
(princ "\nMacro ICS cargado.")
(princ "\nEscribe \"ICS\" para seleccionar dentro de una polilínea cerrada.")
(princ)
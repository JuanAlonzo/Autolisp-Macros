; -- Funcion AddDemand (AD) --
;
; Autor: Alonso Jaramillo
;
; USO:
;	Agrega un texto "1R" debajo de cada texto seleccionado
;	Crea una capa llamada "MagentaLayer" si no existe
;	Coloca el nuevo texto en la capa "MagentaLayer" con color magenta
;
; UTILIDAD:
;	Facilita la adición de demandas en planos de AutoCAD
;	Permite una rápida identificación de los elementos textuales en el dibujo.
;

(defun c:AddDemand () 
  (vl-load-com)
  (if (not (tblsearch "layer" "MagentaLayer")) 
    (progn 
      (command "_.-layer" "_make" "MagentaLayer" "_color" "magenta" "" "")
    )
  )
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss 
    (progn 
      (setq i 0)
      (setq n (sslength ss))
      (while (< i n) 
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        (setq txtPos (cdr (assoc 10 entData))) ; text position
        (setq txtRot (cdr (assoc 50 entData))) ; text rotation
        (setq txtHeight (cdr (assoc 40 entData))) ; text height
        (setq newPos (list (car txtPos) 
                           (- (cadr txtPos) (* 1.5 txtHeight))
                           (caddr txtPos)
                     )
        ) ; New position below the text
        (entmake 
          (list 
            (cons 0 "TEXT")
            (cons 8 "MagentaLayer") ; Magenta layer
            (cons 10 newPos) ; New position
            (cons 1 "1R") ; Text content
            (cons 40 txtHeight) ; Text height
            (cons 50 txtRot) ; Text rotation
            (cons 7 "Standard") ; Text style
          )
        )
        (setq i (1+ i))
      )
    )
    (princ "\nNo se seleccionaron textos.")
  )
  (princ)
)

(princ "\nCommand ADDDEMAND loaded.")
(princ "\nType ADDDEMAND to add demand text.")
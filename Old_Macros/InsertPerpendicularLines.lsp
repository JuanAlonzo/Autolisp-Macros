(defun c:insertPerpendicularLines ()
  (setq linePt (getpoint "\nSelect a point on the line: "))

  (setq perpendicularPt (polar linePt (+ (angle linePt (getpoint linePt "\nSelect the direction for the perpendicular line: ")) (/ pi 2)) 5.0))

  (setq perpendicularLine (entmakex (list '(0 . "LINE") (cons 10 linePt) (cons 11 perpendicularPt) (cons 62 1))))

  (command "_undo" "_begin")

  (command "_pline" linePt perpendicularPt "")

  (setq pline (entlast)) ; Obtener el identificador de la polilínea creada
  (entdel pline) ; Eliminar la polilínea

  (command "_undo" "_end")

  (c:insertPerpendicularLines) ; Llamada recursiva para permitir la inserción de más líneas

  (princ)
)

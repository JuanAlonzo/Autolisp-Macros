;Polyline/circle select - www.cadstudio.cz - www.cadforum.cz
;(use the WPS command or 'WPS inside an object selection prompt)

(defun C:WPS ( / i elist at cmde cen rad p1 impl)
 (setq cmde (getvar "cmdecho"))
 (setvar "cmdecho" 0)
 (setq i 0 elist (entget (car (entsel "\nPick a bounding circle or polyline: ")))) 
 (setvar "OSMODE" (boole 7 (getvar "OSMODE") 16384))
 (if (zerop (getvar "CMDACTIVE")) (progn (setq impl T)(command "_select")))
 (command "_wp") ; or _CP
 (if (= (cdr(assoc 0 elist)) "CIRCLE")
  (progn
  (setq cen (cdr (assoc 10 elist))
        rad (cdr (assoc 40 elist)) 
  )
  (repeat 90 ; 360/4  0.06981317=4*pi/180
   (setq p1 (polar cen (* i 0.06981317) rad)  i (1+ i))
;   (command "_POINT" (trans p1 0 1))
   (command (trans p1 0 1))
  )); else
  (repeat (length elist) 
   (setq at (nth i elist) i (1+ i))
;   (if (= (car at) 10) (command (cdr at)))
   (if (= (car at) 10) (command (trans (cdr at) 0 1)))
  )
 );if CIRCLE
 (command "")
 (setvar "OSMODE" (boole 2 (getvar "OSMODE") 16384))
 (setvar "cmdecho" cmde)
 (if impl (progn (command "")(sssetfirst nil (ssget "_P"))))
 (princ)
)

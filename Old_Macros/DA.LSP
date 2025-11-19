;********************************* DA **********************************
;Running distance calculator by Mike Grisez


(defun c:da (/ pt1 pt2 d)
  (graphscr)
  (setvar "CMDECHO" 0)
  (setq pt1 (getpoint "Pick the First Point "))(terpri)
  (setq pt2 (getpoint "Next Point " pt1))(terpri)
  (setq d (distance pt1 pt2))
  (prompt "Running Distance: ")(prompt (rtos d 2 4))      
    (while
    (setq pt1 (getpoint "  Next Point: " pt2)) (terpri)
    (setq d (+ (distance pt1 pt2) d))
    (prompt "Running Distance: ")(prompt (rtos d 2 4))
    (setq pt2 pt1)
  )
  (prompt "Total Distance: ")(prompt (rtos d 4 4))
  (princ)
) 
    
    

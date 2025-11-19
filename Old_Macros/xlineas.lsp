(defun c:xl()
        ;
	;Este programa fue realizado por Rafael Rodriguez
	;
  ;(setvar "osmode"  4) 
  (setvar "clayer" "CAT_ALIMENTA")
    (PROMPT "\nEste programa dibuja n línas desde un punto") 
    (SETQ pt1 (GETPOINT "\nPrimer punto: ")) 
    (SETQ pt2 (GETPOINT "\nSegundo punto: ")) 
    (COMMAND "_line" pt1 pt2 "") 
    (SETQ pt3 (GETPOINT "\nTercer punto: ")) 
    (COMMAND "_line" pt1 pt3 "") 
    (SETQ pt4 (GETPOINT "\nCuarto punto: ")) 
    (COMMAND "_line" pt1 pt4 "")
    (SETQ pt5 (GETPOINT "\nQuinto punto: ")) 
    (COMMAND "_line" pt1 pt5 "") 
    (SETQ pt6 (GETPOINT "\nSexto punto: ")) 
    (COMMAND "_line" pt1 pt6 "") 
    (SETQ pt7 (GETPOINT "\nSeptimo punto: ")) 
    (COMMAND "_line" pt1 pt7 "")
    (SETQ pt8 (GETPOINT "\nQuinto punto: ")) 
    (COMMAND "_line" pt1 pt8 "") 
    (SETQ pt9 (GETPOINT "\nSexto punto: ")) 
    (COMMAND "_line" pt1 pt9 "") 
    (SETQ pt10 (GETPOINT "\nSexto punto: ")) 
    (COMMAND "_line" pt1 pt10 "") 
    (SETQ pt11 (GETPOINT "\nseptimo punto: ")) 
    (COMMAND "_line" pt1 pt11 "") 
    (princ)
;;;0 NONe 
;;;1 ENDpoint 
;;;2 MIDpoint 
;;;4 CENter 
;;;8 NODe 
;;;16 QUAdrant 
;;;32 INTersection 
;;;64 INSertion 
;;;128 PERpendicular 
;;;256 TANgent 
;;;512 NEArest 
;;;1024 QUIck 
;;;2048 APParent Intersection 
;;;4096 EXTension 
;;;8192 PARallel 

)
  
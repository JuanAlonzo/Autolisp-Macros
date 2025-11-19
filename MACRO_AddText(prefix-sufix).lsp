; -- Function AddText (ADDTXT) --
;
; ADD TEXT PREFIX OR SUFFIX TO EXISTING TEXT
;
; USE:
;   1. Load the routine in AutoCAD.
;   2. Type ADDTXT in the command line.
;   3. Choose whether to add a prefix or suffix.
;   4. Enter the text to add.

(defun C:ADDTXT (/ rep str *str ss txt sn vl e) 
  (vl-load-com)
  (initget "Prefix Suffix")
  (setq rep (cond 
              ((getkword "\nWhere to add Text [Prefix/Suffix]<Suffix>:"))
              ("Suffix")
            )
  )
  (or (setq *str (getenv "Insert-Text")) (setq *str "Here"))
  (if (= "" (setq str (getstring (strcat "\nEnter " rep " Text \"" *str "\": ")))) 
    (setq str *str)
  )
  (setenv "Insert-Text" str)
  (setq ss (ssget "_:L" '((0 . "*TEXT"))))
  (foreach txt (vl-remove-if 'listp (mapcar 'cadr (ssnamex SS))) 
    (setq vl (vlax-ename->vla-object txt))
    (setq e (entget txt))
    (cond 
      ((= rep "Prefix")
       (vla-put-textstring vl (strcat str (cdr (assoc 1 e))))
      )
      ((= rep "Suffix")
       (vla-put-textstring vl (strcat (cdr (assoc 1 e)) str))
      )
    )
  )
  (princ)
)

(princ "\nCommand ADDTXT loaded.")
(princ "\nType ADDTXT to add prefix or suffix to existing text.")
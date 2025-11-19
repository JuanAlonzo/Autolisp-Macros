(defun C:ICS (/ en ss lst ssall tmp head)
 ;;InSide Contour Select
 ;;!!!! [b][color="Red"]REQUIRED EXPRESS TOOLS[/color][/b]
 (vl-load-com)
 (if (and (setq en (car (entsel "\nSelect contour: ")))
          (wcmatch (cdr (assoc 0 (entget en))) "*POLYLINE")
     ) ;_ end of and
   (progn
     (setq lst (ACET-GEOM-OBJECT-POINT-LIST en 1e-3))
     (setq lst (mapcar '(lambda (x) (list (car x) (cadr x))) lst))
     (while lst
       (setq head (car lst)
             tmp  (cons head tmp)
             lst  (vl-remove-if
                    '(lambda (pt) (equal pt head 1e-6))
                    (cdr lst)
                  ) ;_ end of vl-remove-if
       ) ;_ end of setq
     ) ;_ end of while
     (setq lst (reverse tmp)) ;_ end of setq
     (ACET-SS-ZOOM-EXTENTS (ACET-LIST-TO-SS (list en)))
     (command "_.Zoom" "0.95x")      
     (if (setq ss (ssget "_CP" lst))
       (sssetfirst ss ss)
     ) ;_ end of if
   ) ;_ end of progn
 ) ;_ end of if
) ;_ end of defun 
(princ "\nType ICS") 
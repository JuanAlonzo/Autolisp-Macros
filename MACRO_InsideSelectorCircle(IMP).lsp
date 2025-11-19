(defun c:SWC (/ _pac add ss i temp i2) 
  ;; Select Within Curve
  ;; Alan J. Thompson, 03.31.11

  (vl-load-com)

  (defun _pac (e / l v d lst) 
    (setq d (- 
              (setq v (/ 
                        (setq l (vlax-curve-getDistAtParam e 
                                                           (vlax-curve-getEndParam e)
                                )
                        )
                        100.
                      )
              )
            )
    )
    (while (< (setq d (+ d v)) l) 
      (setq lst (cons (vlax-curve-getPointAtDist e d) lst))
    )
  )

  (princ "\nSelect closed curves to select object(s) within: ")
  (if 
    (setq add (ssadd)
          ss  (ssget 
                '((-4 . "<OR")
                  (0 . "CIRCLE,ELLIPSE")
                  (-4 . "<AND")
                  (0 . "*POLYLINE")
                  (-4 . "&=")
                  (70 . 1)
                  (-4 . "AND>")
                  (-4 . "OR>")
                 )
              )
    )
    (progn 
      (repeat (setq i (sslength ss)) 
        (if (setq temp (ssget "_WP" (_pac (ssname ss (setq i (1- i)))))) 
          (repeat (setq i2 (sslength temp)) 
            (ssadd (ssname temp (setq i2 (1- i2))) add)
          )
        )
      )
      (sssetfirst nil add)
      (ssget "_I")
    )
  )
  (princ)
)
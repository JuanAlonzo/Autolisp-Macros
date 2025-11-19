(defun c:xls () 
  (alert "Selecciona los textos a EXPORTAR")
  (if (= 1 1) 
    (progn 
      (setq i     0
            l_x   nil
            l_y   nil
            l_pos nil
            lista nil
      )
      (prompt "\n.......................")
      (setq group (ssget (list (cons 0 "TEXT"))))

      (if (/= group nil) 
        (progn (setq ruta (getvar "DWGPREFIX")) 
               (setq ruta (getfiled "COMANDO:XLS - Escriba el Nombre del Archivo" 
                                    ruta
                                    "xls"
                                    1
                          )
               )
               (if (= ruta nil) 
                 (setq ruta (strcat (getvar "DWGPREFIX") 
                                    (substr (getvar "DWGNAME") 
                                            1
                                            (- (strlen (getvar "DWGNAME")) 3)
                                    )
                                    "xls"
                            )
                 )
               )
               (setq f (open ruta "w"))
               (repeat (sslength group) 
                 (setq lista (append lista (list i)))
                 (setq i (+ i 1))
               )
               (setq i 0)
               (while (/= lista nil) 
                 (foreach temp lista 
                   (setq e_ana (ssname group temp))
                   (setq e_dat (entget e_ana))
                   (setq l_y (append l_y (list (cadr (cdr (assoc 10 e_dat))))))
                 )
                 (setq y_max (apply 'max l_y))
                 (setq i 0)
                 (foreach temp lista 
                   (setq e_ana (ssname group temp))
                   (setq e_dat (entget e_ana))
                   (setq yy (cadr (cdr (assoc 10 e_dat))))
                   (if (equal y_max yy 0.5) 
                     (setq l_pos (append l_pos (list temp)))
                   )
                 )
                 (setq l_x nil)
                 (foreach temp l_pos 
                   (setq e_ana (ssname group temp))
                   (setq e_dat (entget e_ana))
                   (setq l_x (append l_x (list (car (cdr (assoc 10 e_dat))))))
                 )
                 (setq nl_pos nil)
                 (setq l_xx (vl-sort l_x '<))
                 (foreach temp l_xx 
                   (setq nl_pos (append nl_pos (list (vl-position temp l_x))))
                 )
                 (setq linea "")
                 (foreach temp nl_pos 
                   (setq e_cad (cdr 
                                 (assoc 1 (entget (ssname group (nth temp l_pos))))
                               )
                   )
                   (setq linea (strcat linea e_cad "\t"))
                 )
                 (write-line linea f)
                 (foreach temp nl_pos 
                   (setq lista (vl-remove (nth temp l_pos) lista))
                 )
                 (setq i      0
                       l_y    nil
                       l_x    nil
                       l_pos  nil
                       nl_pos nil
                 )
               )
               (close f)
               (setq e_cad nil
                     group nil
                     linea nil
                     l_xx  nil
                     l_yy  nil
                     yy    nil
                     ruta  nil
                     i     nil
                     f     nil
               )
        )
        (alert "No existen textos en la seleccion")
      )
    )
  )
  (princ)
)
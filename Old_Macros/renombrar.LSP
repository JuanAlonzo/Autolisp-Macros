;;;rutina para renombrar bloques
(defun c:renombrar
   (/ name_old name_new)
   (setq name_old nil)
   (setq
      name_old
   (cdr
      (assoc
         2
         (entget
       (car
          (entsel"\nDesignar Bloque a Renombrar: ")
       )
         )
      )
   )
   )
   (setq
      name_new
   (getstring
      (strcat
         "\n Nombre actual del bloque...< "
         (strcase name_old)
         " >   Entrar nuevo nombre de bloque: "
      )
   )
   )
   (command"_.rename""_b"name_old name_new)
   (alert
      (strcat
    "Bloque nombre_____"(strcase name_old)
    "\nRenombrado"
    "\nNombre_____"(strcase name_new)
    "\n____________________________    "
    "\n\n\nLuis Ramirez  © 2012   "
      )
   )
   (prin1)
)
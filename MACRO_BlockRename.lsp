; -- Funcion RenombrarBloques (RB) --
;
; Autor: Luis Ramirez
; Rutina para renombrar bloques
;
; USO:
;	Selecciona un bloque y asigna un nuevo nombre
;	El nombre del bloque se puede cambiar a través de la línea de comandos.
;

(defun c:renombrar (/ name_old name_new) 
  (setq name_old nil)
  (setq name_old (cdr 
                   (assoc 
                     2
                     (entget 
                       (car 
                         (entsel "\nDesignar Bloque a Renombrar: ")
                       )
                     )
                   )
                 )
  )
  (setq name_new (getstring 
                   (strcat 
                     "\n Nombre actual del bloque...< "
                     (strcase name_old)
                     " >   Entrar nuevo nombre de bloque: "
                   )
                 )
  )
  (command "_.rename" "_b" name_old name_new)
  (alert 
    (strcat 
      "Nombre original:"
      (strcase name_old)
      "\nRenombrado a:"
      "\nNuevo nombre:"
      (strcase name_new)
      "\n____________________________    "
      "\n\n\nLuis Ramirez  © 2012   "
    )
  )
  (prin1)
)
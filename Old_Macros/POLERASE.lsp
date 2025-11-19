 ;
 ; -- Orden PolErase
 ; Borra objetos por ventana/captura interior/exterior,
 ;   o borra y recorta interior/exterior.
 ;   ©2004/9 scaner, Spain
 ; Notas:
 ;   Utiliza la función etrim de la orden extrim.
 ;
(defun c:PolErase (/ pol typ vrt lyr pto ssx ssv ssc n ent ky1 ky2)
  (defun dxf (ass ent) (cdr (assoc ass (entget ent))))
  (setq old_error *error*)
  (setq old_cmdecho   (getvar "cmdecho")
        old_highlight (getvar "highlight")
        old_ucsfollow (getvar "ucsfollow")
  )
  (setvar "cmdecho" 0)
  (setvar "ucsfollow" 0)
  (command "_undo" "_begin")
  (command "_ucs" "_world")
  (while (not (and (setq pol (car (entsel "\nDesigne la polilínea de borde: ")))
                   (= (dxf 0 pol) "LWPOLYLINE")
              )
         )
  )
  (setq vrt (LstVrt2LWPol pol)
        lyr (append '((-4 . "<or"))
                    (mapcar (function (lambda (n) (cons 8 n))) (LstLyrUtlAct))
                    '((-4 . "or>"))
            )
  )
  (initget "V C R")
  (if (not (setq ky1 (getkword "\nBorrar por Ventana, Captura o <borrar y Recortar>: ")))
    (setq ky1 "R")
  )
  (initget 1)
  (command "_point" (setq pto (getpoint "\nDesigne el lado a borrar/recortar: ")))
  (setq ssx (ssdel pol (ssget "_x" lyr))
        ssv (ssget "_wp" vrt)
        ssc (ssdel pol (ssget "_cp" vrt))
        n   -1
  )
  (while (setq ent (ssname ssc (setq n (1+ n))))
    (if (= (dxf 5 (entlast)) (dxf 5 ent))
      (setq ky2 T) ;interior
    )
  )
  (cond ((and (or (= ky1 "V") (= ky1 "R")) ky2) (command "_erase" ssv ""))
        ((or (= ky1 "V") (= ky1 "R")) (command "_erase" ssx "_remove" ssc ""))
        ((and (= ky1 "C") ky2) (command "_erase" ssc ""))
        ((= ky1 "C")
         (command "_erase" ssx)
         (if ssv
           (command "_remove" ssv "")
           (command "")
         )
        )
  )
  (cond ((= ky1 "R") (load "extrim") (etrim pol pto)))
  (command "_ucs" "_prev")
  (command "_undo" "_end")
  (setvar "ucsfollow" old_ucsfollow)
  (setvar "highlight" old_highlight)
  (setvar "cmdecho" old_cmdecho)
  (setq *error* old_error)
  (princ)
)
(prompt "\nNuevo comando PolErase definido en AutoCAD - ©2004/9 scaner, Spain.")
 ;
 ; -- Función *error*
 ; Tratamiento de errores
 ;
(defun *error* (msg)
  (command "_ucs" "_prev")
  (command "_undo" "_end")
  (setvar "ucsfollow" old_ucsfollow)
  (setvar "highlight" old_highlight)
  (setvar "cmdecho" old_cmdecho)
  (setq *error* old_error)
)
 ;
 ; -- Función LstVrt2LWPol
 ; Forma una lista con los vértices (x,y) de una polilínea optimizada.
 ;   ©2004/7 scaner, Spain
 ; Argumentos [Tipo]:
 ;   pol = Nombre de entidad (LWPOLYLINE) [ENAME]
 ; Retorna [Tipo]:
 ;   > Lista de coordenadas de vértices (x,y) [LIST]
 ; Notas:
 ;   Ninguna
 ;
(defun LstVrt2LWPol (pol / ass lst)
  (foreach ass (entget pol)
    (if (= (car ass) 10)
      (setq lst (cons (cdr ass) lst))
    )
  )
  (reverse lst)
)
 ;
 ; -- Función LstLyrUtlAct
 ; Forma una lista con las capas utilizadas y activadas.
 ;   ©2004/7 scaner, Spain
 ; Argumentos [Tipo]:
 ;   Ninguno
 ; Retorna [Tipo]:
 ;   > Lista de nombres de capas [LIST]
 ; Notas:
 ;   Las capas se colocan por orden alfabético.
 ;
(defun LstLyrUtlAct (/ lyr lst)
  (setq lyr (tblnext "layer" T))
  (while lyr
    (if (and (= (logand (cdr (assoc 70 lyr)) 1) 0) (> (cdr (assoc 62 lyr)) 0))
      (setq lst (cons (cdr (assoc 2 lyr)) lst))
    )
    (setq lyr (tblnext "layer"))
  )
  (acad_strlsort lst)
)
(princ)
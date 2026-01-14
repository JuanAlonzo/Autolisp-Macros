; MACRO_CrearCapa.lsp
;
; Descripción: 
; Crea una nueva capa en AutoCAD con nombre, color y grosor especificados por el usuario.
;
; Uso: 
; Ejecutar el comando 'CrearCapa' en AutoCAD.
;


(defun c:CrearCapa (/ nc col gr gr_mm) 
  (setvar "cmdecho" 0)

  (setq nc (getstring "\nNombre de la capa: "))
  (if (= nc "") 
    (progn (princ "\nError: El nombre no puede estar vacío.") (exit))
  )

  (setq col (acad_colordlg 7))

  (if col 
    (progn 
      (setq gr (getint "\nGrosor (ej: 9, 15, 30, 50): "))

      (setq gr_mm (/ (float gr) 100.0))

      (command "_-LAYER" "_M" nc "_C" col nc "_LW" gr_mm nc "")

      (princ 
        (strcat "\nCapa '" 
                nc
                "' creada | Color: "
                (itoa col)
                " | Grosor: "
                (rtos gr_mm 2 2)
                "mm"
        )
      )
    )
    (princ "\nCancelado por el usuario.")
  )

  (setvar "cmdecho" 1)
  (princ)
)
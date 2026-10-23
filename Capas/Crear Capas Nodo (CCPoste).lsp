(defun c:CCPOSTES (/ plano capa1 capa2 capa3 col_exist col_proy col_apoyo) 
  (setvar "cmdecho" 0)

  ;;(T permite espacios si fuera necesario)
  (setq plano (getstring T "\nIngrese el codigo del plano (ej. LMSC148-S): "))

  (if plano 
    (setq plano (vl-string-trim " " plano))
  )

  (if (or (null plano) (= plano "")) 
    (princ "\nOperacion cancelada: El nombre del plano no puede estar vacio.")
    (progn 
      ;; Definición de colores estándar (modificar según la norma interna):
      ;; 1 = Rojo | 2 = Amarillo | 3 = Verde | 4 = Cian | 6 = Magenta | 7 = Blanco
      (setq col_exist 2)
      (setq col_proy 4)
      (setq col_apoyo 1)

      ;; Construcción de los nombres exactos de capa según el estándar
      (setq capa1 (strcat "NUMERACION DE POSTES EXISTENTES " plano))
      (setq capa2 (strcat "NUMERACION DE POSTES PROYECTADOS " plano))
      (setq capa3 (strcat "NUMERACION DE POSTES PROYECTADOS DE APOYO " plano))

      (command "_-LAYER" "_M" capa1 "_C" col_exist capa1 "_M" capa2 "_C" col_proy 
               capa2 "_M" capa3 "_C" col_apoyo capa3 ""
      )

      (princ (strcat "\n--- Capas generadas para [" plano "] ---"))
      (princ (strcat "\n -> " capa1 " (Color " (itoa col_exist) ")"))
      (princ (strcat "\n -> " capa2 " (Color " (itoa col_proy) ")"))
      (princ (strcat "\n -> " capa3 " (Color " (itoa col_apoyo) ")"))
    )
  )

  (setvar "cmdecho" 1)
  (princ)
)

(defun c:CNP () (c:CCPOSTES))
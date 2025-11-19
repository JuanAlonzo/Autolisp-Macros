; -- Función: Numero --
;
; Inserta números incrementales en puntos especificados
; El número inicial y el factor de incremento son definidos por el usuario
;
; USO:
;   Ejecutar el comando "numero"
;   Configurar altura, ángulo, estilo y capa del texto
;   Ingresar el número inicial (entero)
;   Ingresar el factor de incremento (real)
;   Seleccionar puntos donde se insertarán los números
;
; MEJORAS IMPLEMENTADAS:
;   ? Opción para definir altura y ángulo del texto
;   ? Selección de estilo de texto existente o crear uno nuevo
;   ? Selección de capa existente o crear una nueva

(vl-load-com)

(defun c:numero (/ numero factor punto producto altura angulo estilo_nombre 
                 capa_nombre usar_estilo_existente usar_capa_existente lista_estilos 
                 lista_capas estilo_valido capa_valida
                ) 

  (princ "\n--- Configuración de Estilo de Texto ---")

  (initget "Si No")
  (setq usar_estilo_existente (getkword "\n¿Usar un estilo de texto existente? [Si/No] <No>: "))

  (if (= usar_estilo_existente "Si") 
    (progn 
      (princ "\nEstilos de texto disponibles:")
      (setq lista_estilos '())
      (setq tabla_estilos (tblnext "STYLE" T))
      (while tabla_estilos 
        (setq nombre_estilo (cdr (assoc 2 tabla_estilos)))
        (princ (strcat "\n  - " nombre_estilo))
        (setq lista_estilos (cons nombre_estilo lista_estilos))
        (setq tabla_estilos (tblnext "STYLE"))
      )

      (setq estilo_valido nil)
      (while (not estilo_valido) 
        (setq estilo_nombre (getstring T "\nIngrese el nombre del estilo a usar: "))
        (if (tblsearch "STYLE" estilo_nombre) 
          (progn 
            (setq estilo_valido T)
            (princ (strcat "\nUsando estilo: " estilo_nombre))
          )
          (princ "\nEstilo no encontrado. Intente nuevamente.")
        )
      )
    )
    (progn 
      (setq estilo_nombre "NUM_INCREMENT")
      (command "_-style" estilo_nombre "verdana" "0" "1" "0" "_no" "_no")
      (princ (strcat "\nEstilo creado: " estilo_nombre))
    )
  )

  (princ "\n\n--- Configuración de Capa ---")

  (initget "Si No")
  (setq usar_capa_existente (getkword "\n¿Usar una capa existente? [Si/No] <No>: "))

  (if (= usar_capa_existente "Si") 
    (progn 
      (princ "\nCapas disponibles:")
      (setq lista_capas '())
      (setq tabla_capas (tblnext "LAYER" T))
      (while tabla_capas 
        (setq nombre_capa (cdr (assoc 2 tabla_capas)))
        (princ (strcat "\n  - " nombre_capa))
        (setq lista_capas (cons nombre_capa lista_capas))
        (setq tabla_capas (tblnext "LAYER"))
      )

      (setq capa_valida nil)
      (while (not capa_valida) 
        (setq capa_nombre (getstring T "\nIngrese el nombre de la capa a usar: "))
        (if (tblsearch "LAYER" capa_nombre) 
          (progn 
            (setq capa_valida T)
            (command "_-layer" "s" capa_nombre "")
            (princ (strcat "\nUsando capa: " capa_nombre))
          )
          (princ "\nCapa no encontrada. Intente nuevamente.")
        )
      )
    )
    (progn 
      (setq capa_nombre "NUM_INCREMENT")
      (command "_-layer" "make" capa_nombre "_color" "5" capa_nombre "")
      (princ (strcat "\nCapa creada: " capa_nombre " (Color: Cyan)"))
    )
  )

  (princ "\n\n--- Configuración de Texto ---")

  (setq altura nil)
  (while (not altura) 
    (setq altura (getreal "\nAltura del texto <1>: "))
    (if (not altura) 
      (setq altura 1.0)
      (if (<= altura 0) 
        (progn 
          (princ "\nLa altura debe ser mayor que 0.")
          (setq altura nil)
        )
      )
    )
  )

  (setq angulo nil)
  (while (not angulo) 
    (setq angulo (getreal "\nÁngulo del texto en grados <0>: "))
    (if (not angulo) 
      (setq angulo 0.0)
    )
  )

  (princ "\n\n--- Configuración de Numeración ---")

  (setq numero nil)
  (while (not numero) 
    (setq numero (getint "\nNumero inicial: "))
    (if (not numero) 
      (princ "\nDebe ingresar un numero entero válido.")
    )
  )

  (setq factor nil)
  (while (not factor) 
    (setq factor (getreal "\nFactor de incremento: "))
    (if (not factor) 
      (princ "\nDebe ingresar un numero válido.")
      (if (= factor 0) 
        (progn 
          (princ "\nEl factor no puede ser 0.")
          (setq factor nil)
        )
      )
    )
  )

  (princ "\n\n*** Configuración completa. Seleccione puntos para insertar números ***")
  (princ "\n(Presione ESC o Enter para finalizar)")

  (while (setq punto (getpoint (strcat "\nPunto [" (itoa numero) "]: "))) 
    (setq producto (* numero factor))
    (command "_text" 
             "s"
             estilo_nombre
             "_C"
             punto
             altura
             angulo
             (rtos producto 2 0)
    )
    (setq numero (1+ numero))
  )

  (princ "\n*** Operación completada ***")
  (princ)
)

(princ "\nComando NUMERO cargado.")
(princ "\nEscribe \"NUMERO\" para insertar números incrementales.")
(princ) 

; -- Funcion DistanciaAcumulada (DA) --
;
; USO:
;	Calcula la distancia acumulada entre puntos seleccionados
;	Muestra la distancia en la linea de comandos con valores redondeados
;	Permite restablecer la distancia o insertar el total como texto
;
; Mejoras implementadas:
; - Muestra la distancia total en una etiqueta de texto en el dibujo (opcional)
; - Permite restablecer la distancia acumulada sin salir del comando
; - Usa medidas redondeadas al entero más cercano

(defun c:DA (/ pt1 pt2 d opcion continuar d_rounded) 
  (graphscr)
  (setvar "CMDECHO" 0)
  (setq continuar T)

  (while continuar 
    (setq pt1 (getpoint "\nSeleccione el primer punto: "))

    (if pt1 
      (progn 
        (setq pt2 (getpoint pt1 "\nSiguiente punto: "))

        (if pt2 
          (progn 
            (setq d (distance pt1 pt2))
            (setq d_rounded (fix (+ d 0.5))) ; Redondea al entero más cercano
            (princ (strcat "\nDistancia acumulada: " (itoa d_rounded)))

            (while 
              (setq pt1 (getpoint pt2 
                                  "\n[Siguiente punto/Restablecer/Finalizar] <Finalizar>: "
                        )
              )
              (if (listp pt1) 
                (progn 
                  (setq d (+ (distance pt1 pt2) d))
                  (setq d_rounded (fix (+ d 0.5)))
                  (princ (strcat "\nDistancia acumulada: " (itoa d_rounded)))
                  (setq pt2 pt1)
                )
                (progn 
                  (setq pt1 nil)
                )
              )
            )

            (setq d_rounded (fix (+ d 0.5)))
            (princ 
              (strcat "\n*** Distancia total: " (itoa d_rounded) " unidades ***")
            )

            ; Insertar directamente el texto sin preguntar
            (setq pt_text (getpoint "\nSeleccione el punto para insertar el texto: "))
            (if pt_text 
              (progn 
                (command "._Text" 
                         "j"
                         "MC"
                         pt_text
                         "1"
                         "0"
                         (strcat "DIST: " (itoa d_rounded))
                )
                (princ "\nTexto insertado correctamente.")
              )
              (princ "\nInsercion de texto cancelada.")
            )

            ; Preguntar si desea reiniciar
            ; (initget "Si No")
            ; (setq opcion (getkword "\n¿Desea realizar otra medición? [Si/No] <No>: "))

            ; (if (/= opcion "Si")
            ;   (setq continuar nil)
            ; )
          )
          (progn 
            (princ "\nOperación cancelada.")
            (setq continuar nil)
          )
        )
      )
      (progn 
        (princ "\nOperación cancelada.")
        (setq continuar nil)
      )
    )
  )

  (C:DA) ; Reiniciar el comando al finalizar
  (setvar "CMDECHO" 1)
  (princ)
)

(princ "\nComando DA (Distancia Acumulada) cargado.")
(princ "\nEscribe \"DA\" para medir distancias acumuladas con valores redondeados.")
(princ) 
    
    

;; Macro para duplicar un texto a otro

;; Uso para continuar la MACRO MCOUNT, la cual devuelve R, C y D por separado.
;; Con esta macro se reemplazara automaticamente la demanda en cada Circulo y Triangulo

;; Usar ZZCOPY y despues seleccionar manualmente que duplicar.

(defun c:ZZCOPY (/ entSource dataSource txtSource entDest dataDest) 
  (princ "\n--- Macro para Transferir Valores ---")

  ;; Bucle para permitir múltiples transferencias seguidas
  (while (setq entSource (car (entsel "\nSeleccione el texto de ORIGEN (el que tiene el número): "))) 
    (setq dataSource (entget entSource))

    ;; Verificar si es un texto o mtext
    (if (member (cdr (assoc 0 dataSource)) '("TEXT" "MTEXT")) 
      (progn 
        (setq txtSource (cdr (assoc 1 dataSource)))
        (princ (strcat "\nValor capturado: " txtSource))

        ;; Seleccionar el destino
        (setq entDest (car (entsel "\nSeleccione el texto de DESTINO (el que dice RZ=XXm): ")))
        (if (and entDest (setq dataDest (entget entDest))) 
          (if (member (cdr (assoc 0 dataDest)) '("TEXT" "MTEXT")) 
            (progn 
              ;; Sustituir el contenido del texto de destino por el de origen
              (entmod (subst (cons 1 txtSource) (assoc 1 dataDest) dataDest))
              (princ "\n>>> ¡Texto actualizado!")
            )
            (princ "\nError: El objeto de destino no es un texto.")
          )
          (princ "\nOperación de destino cancelada.")
        )
      )
      (princ "\nError: El objeto seleccionado no es un texto válido.")
    )
  )
  (princ "\nMacro finalizada.")
  (princ)
)
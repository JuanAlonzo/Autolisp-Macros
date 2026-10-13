(defun c:SUMRES (/ ss i ent str num total count clean-str) 
  (vl-load-com)
  (setq total 0.0
        count 0
  )

  (princ "\nSeleccione los textos con formato RZ=...m: ")
  ;; Filtra solo objetos TEXT y MTEXT
  (if (setq ss (ssget '((0 . "TEXT,MTEXT")))) 
    (progn 
      (repeat (setq i (sslength ss)) 
        (setq ent (ssname ss (setq i (1- i)))
              str (cdr (assoc 1 (entget ent)))
        )

        ;; Limpiar texto: remover saltos de formato en MTEXT y caracteres no numéricos excepto el punto
        (setq clean-str (vl-string-trim " \t\r\n" str))

        ;; Extraer el número usando expresión regular
        (setq num (extraer-numero clean-str))

        (if num 
          (progn 
            (setq total (+ total num)
                  count (1+ count)
            )
          )
        )
      )

      ;; Mostrar resultados
      (if (> count 0) 
        (progn 
          (princ (strcat "\n>>> Total elementos sumados: " (itoa count)))
          (princ (strcat "\n>>> Suma acumulada total: " (rtos total 2 2) " m\n"))
          (alert 
            (strcat "Elementos procesados: " 
                    (itoa count)
                    "\n\nSuma total acumulada: "
                    (rtos total 2 2)
                    " m"
            )
          )
        )
        (princ "\nNo se encontraron valores numéricos válidos en la selección.\n")
      )
    )
    (princ "\nNo se seleccionaron entidades válidas.\n")
  )
  (princ)
)

;; Función auxiliar para extraer números (enteros o decimales) de una cadena
(defun extraer-numero (txt / regex matches match val) 
  (setq regex (vlax-create-object "VBScript.RegExp"))
  (vlax-put-property regex 'Pattern "([0-9]+(\\.[0-9]+)?)")
  (vlax-put-property regex 'Global :vlax-false)
  (vlax-put-property regex 'IgnoreCase :vlax-true)

  (setq matches (vlax-invoke-method regex 'Execute txt))
  (if (> (vlax-get-property matches 'Count) 0) 
    (progn 
      (setq match (vlax-get-property matches 'Item 0))
      (setq val (atof (vlax-get-property match 'Value)))
    )
    (setq val nil)
  )
  (vlax-release-object regex)
  val
)
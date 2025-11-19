; -- Función: Suma Textos --
;
; Esta macro esta basada en la macro original "SumaText(2.0).lsp"
;
; USO:
;	 Este programa suma los números señalados en texto
;	 Coloca el resultado en un punto indicado por el usuario.
;
; Mejoras implementadas:
; 	Mejor manejo de variables locales
; 	Altura promedio en base a los textos seleccionados
;	Filtro capas, resumen y mejor feedback al cancelar
;

(defun capa-visible-p (nombre-capa / layer) 
  (if (setq layer (tblsearch "LAYER" nombre-capa)) 
    (and 
      (= (logand (cdr (assoc 70 layer)) 1) 0)
      (> (cdr (assoc 62 layer)) 0)
    )
    nil
  )
)

(defun normalizar-numero (texto / texto-normalizado) 
  (setq texto-normalizado (vl-string-subst "." "," texto))
  texto-normalizado
)

(defun es-numerico-p (texto / texto-norm valor) 
  (setq texto-norm (normalizar-numero texto))
  (setq valor (atof texto-norm))
  (or (/= valor 0.0) 
      (wcmatch texto-norm "*0*")
  )
)

(defun c:ST (/ ss num ent entdata oper text-found tipo-ent texto-original texto-norm 
             valor nombre-capa suma-alturas contador-alturas altura-promedio ptA 
             textos-procesados textos-ignorados textos-capa-oculta
            ) 

  (setq oper 0.0)
  (setq text-found nil)
  (setq suma-alturas 0.0)
  (setq contador-alturas 0)
  (setq textos-procesados 0)
  (setq textos-ignorados 0)
  (setq textos-capa-oculta 0)

  (if (setq ss (ssget)) 
    (progn 
      (princ "\nAnalizando textos seleccionados...\n")
      (setq num 0)

      (repeat (sslength ss) 
        (setq ent (ssname ss num))
        (setq entdata (entget ent))
        (setq tipo-ent (cdr (assoc 0 entdata)))

        (if (or (eq tipo-ent "TEXT") (eq tipo-ent "MTEXT")) 
          (progn 
            (setq nombre-capa (cdr (assoc 8 entdata)))

            (if (capa-visible-p nombre-capa) 
              (progn 
                (setq texto-original (cdr (assoc 1 entdata)))

                (if (es-numerico-p texto-original) 
                  (progn 
                    (setq texto-norm (normalizar-numero texto-original))
                    (setq valor (atof texto-norm))
                    (setq oper (+ valor oper))
                    (setq text-found t)
                    (setq textos-procesados (1+ textos-procesados))

                    (if (setq altura (cdr (assoc 40 entdata))) 
                      (progn 
                        (setq suma-alturas (+ suma-alturas altura))
                        (setq contador-alturas (1+ contador-alturas))
                      )
                    )
                  )
                  (progn 
                    (setq textos-ignorados (1+ textos-ignorados))
                    (princ 
                      (strcat "  Ignorado (no numerico): \"" texto-original "\"\n")
                    )
                  )
                )
              )
              (progn 
                (setq textos-capa-oculta (1+ textos-capa-oculta))
              )
            )
          )
        )
        (setq num (1+ num))
      )

      (princ (strcat "Textos procesados: " (itoa textos-procesados) "\n"))
      (princ 
        (strcat "Textos ignorados (no numericos): " (itoa textos-ignorados) "\n")
      )
      (princ 
        (strcat "Textos en capas ocultas/congeladas: " 
                (itoa textos-capa-oculta)
                "\n"
        )
      )
      (princ (strcat "TOTAL SUMA: " (rtos oper 2 2) "\n"))
    )
    (princ "\nOperacion cancelada: No se seleccionaron objetos.\n")
  )

  (if text-found 
    (progn 
      (if (> contador-alturas 0) 
        (setq altura-promedio (/ suma-alturas contador-alturas))
        (setq altura-promedio 1.5) ; Default height
      )

      (setq ptA (getpoint "\nPunto de insercion del resultado (ESC para cancelar): "))

      (if ptA 
        (progn 
          (terpri)
          (command "_TEXT" ptA altura-promedio "" (rtos oper 2 2))
          (princ "\nTexto de resultado insertado correctamente.\n")
        )
        (princ "\nOperacion cancelada por el usuario.\n")
      )
    )
    (if ss 
      (alert "No se encontraron textos numericos validos para sumar.")
    )
  )
  (princ)
)

(princ "\nComando ST cargado.")
(princ "\nEscribe \"ST\" para ejecutar la suma de textos numericos.")
(princ)

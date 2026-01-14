; -- Función: MCOUNT --
;
; Funcionalidad mejorada de la macro original XLINEA.
; Autor: Alonso Jaramillo
;
; USO:
; 	Suma los textos seleccionados y dibuja lineas al punto central
; 	Incluye filtro de capas visibles/no congeladas
;	Coloca el resultado en un nuevo texto en el punto central
;	Coloca textos adicionales con las sumas de R, C y D debajo del total
;   Crea la capa CAT_ALIMENTA si no existe
;   Textos especiales TSC, TEC, T.E.C., T.S.C. cuentan como 1R
;

(defun capa-visible-p (nombre-capa / layer) 
  (if (setq layer (tblsearch "LAYER" nombre-capa)) 
    (and 
      (= (logand (cdr (assoc 70 layer)) 1) 0) ; No congelada
      (> (cdr (assoc 62 layer)) 0) ; Encendida (LayerOn)
    )
    nil
  )
)

(defun verificar-crear-capa (nombre-capa color / layer-data) 
  ; Verifica si la capa existe, si no existe la crea
  (if (not (tblsearch "LAYER" nombre-capa)) 
    (progn 
      (setq layer-data (list (cons 0 "LAYER") 
                             (cons 100 "AcDbSymbolTableRecord")
                             (cons 100 "AcDbLayerTableRecord")
                             (cons 2 nombre-capa)
                             (cons 70 0)
                             (cons 62 color)
                       )
      )
      (entmake layer-data)
      (princ (strcat "\nCapa '" nombre-capa "' creada con color " (itoa color)))
    )
    (princ (strcat "\nCapa '" nombre-capa "' ya existe"))
  )
)

(defun c:MCOUNT (/ ss p_centro oper text-found i ent entData p1 AAA AREAE nombre-capa 
                 suma_r suma_c suma_d suma_total ptEtiqueta textEnt ptTextoR ptTextoC 
                 ptTextoD textos-procesados textos-omitidos valor_numerico offset-y 
                 texto-upper
                ) 

  (setq ss (ssget '((0 . "TEXT,MTEXT"))))

  (if ss 
    (progn 
      ; Verificar o crear la capa CAT_ALIMENTA
      (verificar-crear-capa "CAT_ALIMENTA" 30)

      (setq p_centro (getpoint "\nIndica el punto central:"))
      (setq oper       0
            text-found nil
      )
      (setq i 0)

      (setq textos-procesados 0)
      (setq textos-omitidos 0)

      (setq suma_r 0)
      (setq suma_c 0)
      (setq suma_d 0)

      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        (setq p1 (cdr (assoc 10 entData)))

        (setq nombre-capa (cdr (assoc 8 entData)))

        (if (capa-visible-p nombre-capa) 
          (progn 
            (setq AAA (cdr (assoc 1 entData)))
            (setq AREAE (atof AAA))
            (setq text-found t)
            (setq textos-procesados (1+ textos-procesados))

            ; Extraer valor numérico del texto
            (setq valor_numerico (atoi AAA))
            (setq texto-upper (strcase AAA))

            ; Verificar textos especiales TSC, TEC, T.E.C., T.S.C. - cuentan como 1R
            (if 
              (or (= texto-upper "TSC") 
                  (= texto-upper "TEC")
                  (= texto-upper "T.E.C.")
                  (= texto-upper "T.S.C.")
              )
              (setq suma_r (+ suma_r 1))
              ; Si no es texto especial, aplicar lógica normal
              (if (wcmatch texto-upper "*R*") 
                (setq suma_r (+ suma_r AREAE))
                (if (wcmatch texto-upper "*C*") 
                  (setq suma_c (+ suma_c AREAE))
                  (if (wcmatch texto-upper "*D*") 
                    (progn 
                      ; Lógica especial para D: si >9 cuenta como 1, si <=9 cuenta el número
                      (if (> valor_numerico 9) 
                        (setq suma_d (+ suma_d 1))
                        (setq suma_d (+ suma_d AREAE))
                      )
                    )
                    ; Si contiene T (pero no es texto especial), sumarlo a R
                    (if (wcmatch texto-upper "*T*") 
                      (setq suma_r (+ suma_r AREAE))
                    )
                  )
                )
              )
            )

            (command "._layer" "s" "CAT_ALIMENTA" "")
            (command "_line" p1 p_centro "")
          )
          (progn 
            (setq textos-omitidos (1+ textos-omitidos))
          )
        )

        (setq i (1+ i))
      )

      (if text-found 
        (progn 
          (setq suma_total (+ suma_r suma_c suma_d))

          (setq ptEtiqueta p_centro)
          (setq offset-y 0) ; Variable para controlar el desplazamiento vertical

          ; Insertar TOTAL siempre en capa CAT_ALIMENTA
          (setq textEnt (entmakex 
                          (list (cons 0 "TEXT") 
                                (cons 8 "CAT_ALIMENTA") ; Capa
                                (cons 10 ptEtiqueta)
                                (cons 40 1) ; Text height
                                (cons 1 (rtos (fix suma_total) 2 0)) ; Text content
                                (cons 50 0.0) ; Rotation
                          )
                        )
          )
          (setq offset-y (+ offset-y 2))

          ;; Agregar texto con la suma de R solo si es mayor que 0
          (if (> suma_r 0) 
            (progn 
              (setq ptTextoR (list (car ptEtiqueta) 
                                   (- (cadr ptEtiqueta) offset-y)
                                   0
                             )
              )
              (entmakex 
                (list (cons 0 "TEXT") 
                      (cons 8 "CAT_ALIMENTA") ; Capa
                      (cons 10 ptTextoR)
                      (cons 40 1)
                      (cons 1 (strcat "Total R: " (rtos (fix suma_r) 2 0)))
                      (cons 50 0.0)
                      (cons 62 1) ; Color rojo
                )
              )
              (setq offset-y (+ offset-y 2))
            )
          )

          ;; Agregar texto con la suma de C solo si es mayor que 0
          (if (> suma_c 0) 
            (progn 
              (setq ptTextoC (list (car ptEtiqueta) 
                                   (- (cadr ptEtiqueta) offset-y)
                                   0
                             )
              )
              (entmakex 
                (list (cons 0 "TEXT") 
                      (cons 8 "CAT_ALIMENTA") ; Capa
                      (cons 10 ptTextoC)
                      (cons 40 1)
                      (cons 1 (strcat "Total C: " (rtos (fix suma_c) 2 0)))
                      (cons 50 0.0)
                      (cons 62 3) ; Color verde
                )
              )
              (setq offset-y (+ offset-y 2))
            )
          )

          ;; Agregar texto con la suma de D solo si es mayor que 0
          (if (> suma_d 0) 
            (progn 
              (setq ptTextoD (list (car ptEtiqueta) 
                                   (- (cadr ptEtiqueta) offset-y)
                                   0
                             )
              )
              (entmakex 
                (list (cons 0 "TEXT") 
                      (cons 8 "CAT_ALIMENTA") ; Capa
                      (cons 10 ptTextoD)
                      (cons 40 1)
                      (cons 1 (strcat "Total D: " (rtos (fix suma_d) 2 0)))
                      (cons 50 0.0)
                      (cons 62 4) ; Color cyan
                )
              )
              (setq offset-y (+ offset-y 2))
            )
          )
        )
        (alert "No se encontraron textos válidos para sumar.")
      )
    )
    (princ "\nNo se seleccionaron textos.")
  )

  (princ)
)

(princ "\nComando MCOUNT cargado.")
(princ "\nEscribe \"MCOUNT\" para ejecutar.")
(princ)

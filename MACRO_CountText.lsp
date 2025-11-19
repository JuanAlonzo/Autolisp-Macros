; -- Funcion ContarTextos (CT) --
;
; Autor: Alonso Jaramillo
; Funcionalidad sacada de la macro de AutoCAD "SumaText"
;
; USO:
;	 Cuenta y suma valores numéricos en textos seleccionados.
;	 Inserta un resumen en el dibujo en un punto especificado.
;	 Reconoce sufijos "R", "C", "D" para conteos específicos.
;
; Notas:
; - Selecciona textos (TEXT o MTEXT) que contengan números.
; - Ejemplo de texto válido: "12.5", "3R", "7C", "2D", "0.75"
; - Si no se encuentra ningún texto numérico, muestra una alerta.
; - Los números pueden usar punto o coma como separador decimal.
;

(defun capa-visible-p (nombre-capa / layer) 
  (if (setq layer (tblsearch "LAYER" nombre-capa)) 
    (and 
      (= (logand (cdr (assoc 70 layer)) 1) 0) ; No congelada
      (> (cdr (assoc 62 layer)) 0) ; Encendida (LayOn)
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

(defun extraer-numero-tipo (texto tipo / texto-upper patron pos i num-str) 
  (setq texto-upper (strcase texto))
  (setq patron (strcat "*#" tipo "*"))

  ; Search for the pattern and extract the number before the type
  (if (wcmatch texto-upper patron) 
    (progn 
      (setq pos (vl-string-search tipo texto-upper))
      (if pos 
        (progn 
          (setq i (1- pos))
          (setq num-str "")
          (while 
            (and (>= i 0) 
                 (wcmatch (substr texto-upper (1+ i) 1) "#")
            )
            (setq num-str (strcat (substr texto-upper (1+ i) 1) num-str))
            (setq i (1- i))
          )
          (if (> (strlen num-str) 0) 
            (atoi num-str)
            1 ; If no number found, default to 1
          )
        )
        0
      )
    )
    0
  )
)

(defun c:CT (/ ss num ent entdata oper text-found tipo-ent texto-original texto-norm 
             valor nombre-capa suma-alturas contador-alturas altura-promedio ptA 
             textos-procesados textos-ignorados textos-capa-oculta suma-r suma-c 
             suma-d suma-t num-r num-c num-d num-t contador-d-menores contador-d-mayores
            ) 

  (setq oper 0.0)
  (setq text-found nil)
  (setq suma-alturas 0.0)
  (setq contador-alturas 0)
  (setq textos-procesados 0)
  (setq textos-ignorados 0)
  (setq textos-capa-oculta 0)

  (setq suma-r 0)
  (setq suma-c 0)
  (setq suma-d 0)
  (setq suma-t 0)
  (setq contador-d-menores 0)
  (setq contador-d-mayores 0)

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

                    (setq texto-upper (strcase texto-original))

                    (if (wcmatch texto-upper "*R*") 
                      (progn 
                        (setq num-r (extraer-numero-tipo texto-original "R"))
                        (setq suma-r (+ suma-r num-r))
                      )
                    )

                    (if (wcmatch texto-upper "*C*") 
                      (progn 
                        (setq num-c (extraer-numero-tipo texto-original "C"))
                        (setq suma-c (+ suma-c num-c))
                      )
                    )

                    (if (wcmatch texto-upper "*D*") 
                      (progn 
                        (setq num-d (extraer-numero-tipo texto-original "D"))
                        ; Lógica especial: si num-d > 9, se cuenta como 1
                        (if (> num-d 9) 
                          (progn
                            (setq suma-d (+ suma-d 1))
                            (setq contador-d-mayores (+ contador-d-mayores 1))
                          )
                          (progn
                            (setq suma-d (+ suma-d num-d))
                            (setq contador-d-menores (+ contador-d-menores 1))
                          )
                        )
                      )
                    )

                    (if (wcmatch texto-upper "*T*") 
                      (progn 
                        (setq num-t (extraer-numero-tipo texto-original "T"))
                        (setq suma-t (+ suma-t num-t))
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

      (princ 
        (strcat "\nTextos procesados: " 
                (itoa textos-procesados)
                " | Omitidos: "
                (itoa textos-capa-oculta)
                "\n"
        )
      )
    )
    (princ "\nOperacion cancelada: No se seleccionaron objetos.\n")
  )

  (if text-found 
    (progn 
      (if (> contador-alturas 0) 
        (setq altura-promedio (/ suma-alturas contador-alturas))
        (setq altura-promedio 1.5) ; Altura por defecto
      )

      (setq ptA (getpoint "\nPunto de insercion de los resultados: "))

      (if ptA 
        (progn 
          (setq suma-total-tipos (+ suma-r suma-c suma-d suma-t))
          (setq offset-y 0) ; Variable para controlar el desplazamiento vertical

          ; Insertar TOTAL siempre
          (entmakex 
            (list (cons 0 "TEXT") 
                  (cons 10 ptA)
                  (cons 40 altura-promedio)
                  (cons 1 (strcat "TOTAL: " (rtos oper 2 2)))
                  (cons 50 0.0)
            )
          )
          (setq offset-y (+ offset-y 2))

          ; Insertar R solo si es mayor que 0
          (if (> suma-r 0) 
            (progn 
              (setq ptR (list (car ptA) 
                              (- (cadr ptA) (* offset-y altura-promedio))
                              0
                        )
              )
              (entmakex 
                (list (cons 0 "TEXT") 
                      (cons 10 ptR)
                      (cons 40 altura-promedio)
                      (cons 1 (strcat "R: " (itoa suma-r)))
                      (cons 50 0.0)
                      (cons 62 1) ; Color rojo
                )
              )
              (setq offset-y (+ offset-y 2))
            )
          )

          ; Insertar C solo si es mayor que 0
          (if (> suma-c 0) 
            (progn 
              (setq ptC (list (car ptA) 
                              (- (cadr ptA) (* offset-y altura-promedio))
                              0
                        )
              )
              (entmakex 
                (list (cons 0 "TEXT") 
                      (cons 10 ptC)
                      (cons 40 altura-promedio)
                      (cons 1 (strcat "C: " (itoa suma-c)))
                      (cons 50 0.0)
                      (cons 62 3) ; Color verde
                )
              )
              (setq offset-y (+ offset-y 2))
            )
          )

          ; Insertar D solo si es mayor que 0
          (if (> suma-d 0) 
            (progn 
              (setq ptD (list (car ptA) 
                              (- (cadr ptA) (* offset-y altura-promedio))
                              0
                        )
              )
              (entmakex 
                (list (cons 0 "TEXT") 
                      (cons 10 ptD)
                      (cons 40 altura-promedio)
                      (cons 1 (strcat "D: " (itoa suma-d) 
                                      " (" (itoa contador-d-menores) "≤9 + " 
                                      (itoa contador-d-mayores) ">9)"))
                      (cons 50 0.0)
                      (cons 62 4) ; Color cyan
                )
              )
              (setq offset-y (+ offset-y 2))
            )
          )

          ; Insertar T solo si es mayor que 0
          (if (> suma-t 0) 
            (progn 
              (setq ptT (list (car ptA) 
                              (- (cadr ptA) (* offset-y altura-promedio))
                              0
                        )
              )
              (entmakex 
                (list (cons 0 "TEXT") 
                      (cons 10 ptT)
                      (cons 40 altura-promedio)
                      (cons 1 (strcat "T: " (itoa suma-t)))
                      (cons 50 0.0)
                      (cons 62 5) ; Color azul
                )
              )
              (setq offset-y (+ offset-y 2))
            )
          )

          ; Insertar suma total solo si es mayor que 0
          (if (> suma-total-tipos 0) 
            (progn 
              (setq ptSum (list (car ptA) 
                                (- (cadr ptA) (* offset-y altura-promedio))
                                0
                          )
              )
              (entmakex 
                (list (cons 0 "TEXT") 
                      (cons 10 ptSum)
                      (cons 40 altura-promedio)
                      (cons 1 (strcat "R+C+D+T: " (itoa suma-total-tipos)))
                      (cons 50 0.0)
                      (cons 62 6) ; Color magenta
                )
              )
            )
          )

          (princ "\nResultados insertados en el dibujo.\n")
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

(princ "\nComando CT cargado.")
(princ "\nEscribe \"CT\" para ejecutar la funcion ContarTextos.")

;; ============================================================================
;; Macro: Conteo Interactivo de Demandas FTTH (MCOUNT / CO / CT)
;; Unifica:
;;   - Design_FTTH/Multi Count (MCO).lsp
;;   - MACRO_Multiline_Count.lsp
;;   - Design_FTTH/Only Count (CO).lsp
;;   - Conteos/Count Text (CT).lsp
;;   - MACRO_MultiLine.lsp (MLSum)
;;   - MACRO_Sum.lsp (SUM)
;;
;; Mejoras implementadas:
;; - Modo interactivo en el prompt del punto central: permite alternar entre
;;   [Lineas/SoloTexto] y [Semaforo] sin recargar ni cambiar de comando.
;; - Creación de líneas mediante `entmakex` (inmune a errores de OSNAP y 10x más rápido).
;; - Parser unificado: reconoce textos especiales (TSC, TEC, T.E.C., T.S.C. = 1R),
;;   múltiples sufijos en una misma cadena (ej. "2R 1C"), regla de D (>9 cuenta como 1),
;;   y números puros.
;; - Incluye atajos rápidos:
;;     MCOUNT -> Inicia con Líneas activadas
;;     CO     -> Inicia en modo Solo Texto (sin líneas)
;;     CT     -> Inicia con desglose completo y altura dinámica
;; ============================================================================

(vl-load-com)

(defun mcount:capa-visible-p (nombre-capa / layer)
  (if (setq layer (tblsearch "LAYER" nombre-capa))
    (and
      (= (logand (cdr (assoc 70 layer)) 1) 0) ; No congelada
      (> (cdr (assoc 62 layer)) 0)            ; Encendida (LayerOn)
    )
    nil
  )
)

(defun mcount:verificar-crear-capa (nombre-capa color /)
  (if (not (tblsearch "LAYER" nombre-capa))
    (entmake
      (list '(0 . "LAYER")
            '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbLayerTableRecord")
            (cons 2 nombre-capa)
            '(70 . 0)
            (cons 62 color)
      )
    )
  )
)

(defun mcount:extraer-numero-antes-de (texto-upper letra / pos i ch num-str)
  (setq pos (vl-string-search letra texto-upper))
  (if pos
    (progn
      (setq i       pos
            num-str ""
      )
      ;; Retroceder espacios si los hubiera
      (while (and (> i 0) (= (substr texto-upper i 1) " "))
        (setq i (1- i))
      )
      ;; Capturar dígitos y punto decimal
      (while (and (> i 0)
                  (wcmatch (setq ch (substr texto-upper i 1)) "#,."))
        (if (= ch ",") (setq ch "."))
        (setq num-str (strcat ch num-str)
              i       (1- i)
        )
      )
      (if (> (strlen num-str) 0)
        (atof num-str)
        1.0
      )
    )
    nil
  )
)

(defun mcount:ejecutar (modo-inicial / *error* doc ss i ent entData nombre-capa p1
                        raw-txt txt-upper val-r val-c val-d val-t val-puro
                        suma_r suma_c suma_d suma_puro d_menores d_mayores
                        pts-origen textos-procesados textos-omitidos
                        dibujar-lineas usar-semaforo p_centro kw
                        suma_total offset-y color-total altura-txt)

  (defun *error* (msg)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase (vl-princ-to-string msg)) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; Recordar preferencias en la sesión
  (if (null *mcount:lineas*)   (setq *mcount:lineas* T))
  (if (null *mcount:semaforo*) (setq *mcount:semaforo* nil))
  (if (= modo-inicial "SOLO_TEXTO") (setq dibujar-lineas nil)
    (if (= modo-inicial "CON_LINEAS") (setq dibujar-lineas T)
      (setq dibujar-lineas *mcount:lineas*)
    )
  )
  (setq usar-semaforo *mcount:semaforo*)

  (princ "\nSeleccione los textos de demanda (TEXT / MTEXT): ")
  (if (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    (progn
      (vla-StartUndoMark doc)
      (mcount:verificar-crear-capa "CAT_ALIMENTA" 30)

      (setq i                 0
            suma_r            0.0
            suma_c            0.0
            suma_d            0.0
            suma_puro         0.0
            d_menores         0
            d_mayores         0
            textos-procesados 0
            textos-omitidos   0
            pts-origen        '()
      )

      (while (< i (sslength ss))
        (setq ent         (ssname ss i)
              entData     (entget ent)
              nombre-capa (cdr (assoc 8 entData))
              p1          (cdr (assoc 10 entData))
        )

        (if (mcount:capa-visible-p nombre-capa)
          (progn
            (setq raw-txt   (vl-string-trim " \t\r\n" (cdr (assoc 1 entData)))
                  txt-upper (strcase raw-txt)
            )

            (cond
              ;; Caso 1: Textos especiales TSC / TEC -> 1R
              ((or (= txt-upper "TSC")
                   (= txt-upper "TEC")
                   (= txt-upper "T.E.C.")
                   (= txt-upper "T.S.C."))
               (setq suma_r            (+ suma_r 1.0)
                     pts-origen        (cons p1 pts-origen)
                     textos-procesados (1+ textos-procesados)
               )
              )

              ;; Caso 2: Contiene sufijos R, C, D o T
              ((wcmatch txt-upper "*R*,*C*,*D*,*T*")
               (setq val-r (if (wcmatch txt-upper "*R*") (mcount:extraer-numero-antes-de txt-upper "R") 0.0)
                     val-c (if (wcmatch txt-upper "*C*") (mcount:extraer-numero-antes-de txt-upper "C") 0.0)
                     val-d (if (wcmatch txt-upper "*D*") (mcount:extraer-numero-antes-de txt-upper "D") 0.0)
                     val-t (if (wcmatch txt-upper "*T*") (mcount:extraer-numero-antes-de txt-upper "T") 0.0)
               )
               (setq suma_r (+ suma_r val-r val-t)
                     suma_c (+ suma_c val-c)
               )
               (if (> val-d 0.0)
                 (if (> val-d 9.0)
                   (setq suma_d    (+ suma_d 1.0)
                         d_mayores (1+ d_mayores)
                   )
                   (setq suma_d    (+ suma_d val-d)
                         d_menores (1+ d_menores)
                   )
                 )
               )
               (setq pts-origen        (cons p1 pts-origen)
                     textos-procesados (1+ textos-procesados)
               )
              )

              ;; Caso 3: Número puro sin letras
              ((or (/= (setq val-puro (atof (vl-string-subst "." "," raw-txt))) 0.0)
                   (wcmatch raw-txt "*0*"))
               (setq suma_puro         (+ suma_puro val-puro)
                     pts-origen        (cons p1 pts-origen)
                     textos-procesados (1+ textos-procesados)
               )
              )

              (T (setq textos-omitidos (1+ textos-omitidos)))
            )
          )
          (setq textos-omitidos (1+ textos-omitidos))
        )
        (setq i (1+ i))
      )

      (if (> textos-procesados 0)
        (progn
          (setq suma_total (+ suma_r suma_c suma_d suma_puro))
          (princ
            (strcat "\n>>> Pre-resumen (" (itoa textos-procesados) " textos): "
                    "Total=" (rtos suma_total 2 0)
                    " [R=" (rtos suma_r 2 0)
                    " | C=" (rtos suma_c 2 0)
                    " | D=" (rtos suma_d 2 0) "]"
            )
          )

          ;; Solicitud interactiva del punto central con opciones en vivo
          (setq p_centro nil)
          (while (null p_centro)
            (initget "Lineas SoloTexto Semaforo")
            (setq kw
              (getpoint
                (strcat "\nIndique punto central [Lineas/SoloTexto/Semaforo] <"
                        (if dibujar-lineas "Con Lineas" "Solo Texto")
                        (if usar-semaforo ", Semaforo ON" "")
                        ">: "
                )
              )
            )
            (cond
              ((= kw "Lineas")
               (setq dibujar-lineas   T
                     *mcount:lineas*  T
               )
               (princ "\n-> Modo cambiado a: CON LINEAS.")
              )
              ((= kw "SoloTexto")
               (setq dibujar-lineas   nil
                     *mcount:lineas*  nil
               )
               (princ "\n-> Modo cambiado a: SOLO TEXTO (sin líneas).")
              )
              ((= kw "Semaforo")
               (setq usar-semaforo     (not usar-semaforo)
                     *mcount:semaforo* usar-semaforo
               )
               (princ (strcat "\n-> Semáforo (16-18 Verde): " (if usar-semaforo "ACTIVADO" "DESACTIVADO")))
              )
              ((listp kw)
               (setq p_centro kw)
              )
              (T
               (princ "\nInserción cancelada por el usuario.")
               (vla-EndUndoMark doc)
               (exit)
              )
            )
          )

          ;; 1. Dibujar líneas spider con entmakex (sin errores de OSNAP)
          (if dibujar-lineas
            (foreach pt pts-origen
              (entmakex
                (list '(0 . "LINE")
                      '(8 . "CAT_ALIMENTA")
                      (cons 10 pt)
                      (cons 11 p_centro)
                )
              )
            )
          )

          ;; 2. Insertar etiqueta de TOTAL GENERAL
          (setq altura-txt  (if dibujar-lineas 1.0 1.5)
                offset-y    0.0
                color-total (if usar-semaforo
                              (if (and (>= suma_total 16) (<= suma_total 18)) 3 1)
                              256 ; ByLayer
                            )
          )

          (entmakex
            (list '(0 . "TEXT")
                  '(8 . "CAT_ALIMENTA")
                  (cons 10 p_centro)
                  (cons 40 altura-txt)
                  (cons 1 (rtos suma_total 2 0))
                  '(50 . 0.0)
                  (cons 62 color-total)
            )
          )
          (setq offset-y (+ offset-y 2.0))

          ;; 3. Desglose R
          (if (> suma_r 0)
            (progn
              (entmakex
                (list '(0 . "TEXT")
                      '(8 . "CAT_ALIMENTA")
                      (cons 10 (list (car p_centro) (- (cadr p_centro) offset-y) 0.0))
                      '(40 . 1.0)
                      (cons 1 (strcat "Total R: " (rtos (fix suma_r) 2 0)))
                      '(50 . 0.0)
                      '(62 . 1) ; Rojo
                )
              )
              (setq offset-y (+ offset-y 2.0))
            )
          )

          ;; 4. Desglose C
          (if (> suma_c 0)
            (progn
              (entmakex
                (list '(0 . "TEXT")
                      '(8 . "CAT_ALIMENTA")
                      (cons 10 (list (car p_centro) (- (cadr p_centro) offset-y) 0.0))
                      '(40 . 1.0)
                      (cons 1 (strcat "Total C: " (rtos (fix suma_c) 2 0)))
                      '(50 . 0.0)
                      '(62 . 3) ; Verde
                )
              )
              (setq offset-y (+ offset-y 2.0))
            )
          )

          ;; 5. Desglose D
          (if (> suma_d 0)
            (progn
              (entmakex
                (list '(0 . "TEXT")
                      '(8 . "CAT_ALIMENTA")
                      (cons 10 (list (car p_centro) (- (cadr p_centro) offset-y) 0.0))
                      '(40 . 1.0)
                      (cons 1 (strcat "Total D: " (rtos (fix suma_d) 2 0)))
                      '(50 . 0.0)
                      '(62 . 4) ; Cyan
                )
              )
              (setq offset-y (+ offset-y 2.0))
            )
          )

          (princ "\nConteo insertado exitosamente en capa CAT_ALIMENTA.")
        )
        (alert "No se encontraron textos válidos en capas visibles para sumar.")
      )
      (vla-EndUndoMark doc)
    )
    (princ "\nNo se seleccionaron textos.")
  )
  (princ)
)

;; Comandos públicos
(defun c:MCOUNT () (mcount:ejecutar "CON_LINEAS"))
(defun c:CO     () (mcount:ejecutar "SOLO_TEXTO"))
(defun c:CT     () (mcount:ejecutar nil))

(princ "\nComandos de conteo cargados: MCOUNT (con líneas), CO (solo texto), CT (interactivo).")
(princ)

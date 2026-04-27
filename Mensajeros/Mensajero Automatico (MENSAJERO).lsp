; mensajeroAuto.lsp
;
; Descripción:
; Automatiza la inserción de bloques de cable mensajero a lo largo de una polilínea guía, 
; ajustando la longitud del cable y gestionando capas.
; Uso:
; Ejecutar el comando 'MENSAJERO' en AutoCAD.
;


(defun c:MENSAJERO (/ *error* doc modelSpace nombreBloque capaDestino capaGuia gap 
                    gapTotal propsAdmitidas guiaEnt guiaObj coords i coordList ptList 
                    ssRed lineaObj p1 p2 midPt ang distReal distFinal bloqueObj props 
                    prop nombreProp minPt maxPt centerPt oldLayer lastEnt newEnt
                   ) 
  (vl-load-com)

  ;; --- CONFIGURACION ---
  (setq nombreBloque "MensajeroP")
  (setq capaDestino "CABLE_MENSAJERO_P")
  (setq capaGuia "CABLE_GUIA")
  (setq propsAdmitidas '("Distance" "Distancia" "Distance1" "Distancia1" "Length" 
                         "Longitud"
                        )
  )
  (setq gap 1.5) ;; Espacio a reducir por lado
  (setq gapTotal (* gap 2.0))

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-ModelSpace doc))

  ;; Guardamos capa actual para restaurarla luego
  (setq oldLayer (getvar "CLAYER"))

  ;; --- MANEJO DE ERRORES ---
  (defun *error* (msg) 
    (if oldLayer (setvar "CLAYER" oldLayer))
    (vla-EndUndoMark doc)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")) 
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (vla-StartUndoMark doc)

  ;; --- PASO 1: GESTION DE CAPA GUIA ---
  (if (not (tblsearch "LAYER" capaGuia)) 
    (progn 
      (command "-layer" "m" capaGuia "c" "5" "" "lw" "0.30" "" "") ;; Crear y Color Azul
      (princ (strcat "\nCapa " capaGuia " creada."))
    )
  )

  ;; --- PASO 2: INTERACCION DE USUARIO (UX MEJORADA) ---
  (setq guiaEnt nil)

  ;; Preguntamos: Seleccionar o Enter
  (setq guiaEnt (car 
                  (entsel 
                    (strcat "\nSeleccione Polilinea Guia o presione [ENTER] para DIBUJAR en " 
                            capaGuia
                            ": "
                    )
                  )
                )
  )

  ;; CONDICIONAL: Si el usuario dio ENTER (guiaEnt es nil), activamos modo DIBUJO
  (if (not guiaEnt) 
    (progn 
      (princ "\n--- MODO DIBUJO ACTIVADO ---")
      (setvar "CLAYER" capaGuia) ;; Cambiamos a la capa guia

      (setq lastEnt (entlast)) ;; Guardamos el ultimo objeto antes de dibujar

      ;; Activamos comando PLINE y esperamos a que el usuario termine (while cmdactive)
      (command "_.PLINE")
      (while (> (getvar "CMDACTIVE") 0) (command pause))

      ;; Verificamos si se creo algo nuevo
      (if (not (equal lastEnt (entlast))) 
        (setq guiaEnt (entlast)) ;; Capturamos la nueva polilinea
        (princ "\nNo se dibujó nada. Cancelando.")
      )
    )
  )

  ;; --- PASO 3: PROCESAMIENTO (Si tenemos una guia valida) ---
  (if guiaEnt 
    (progn 
      ;; Validar que sea Polilinea (sea seleccionada o dibujada)
      (if (= (cdr (assoc 0 (entget guiaEnt))) "LWPOLYLINE") 
        (progn 
          ;; Obtener Coordenadas para Fence
          (setq guiaObj (vlax-ename->vla-object guiaEnt))
          (setq coords (vlax-get guiaObj 'Coordinates))

          (setq ptList '())
          (setq i 0)
          (while (< i (length coords)) 
            (setq ptList (cons (list (nth i coords) (nth (1+ i) coords) 0.0) 
                               ptList
                         )
            )
            (setq i (+ i 2))
          )
          (setq ptList (reverse ptList))

          ;; ESCANEO FENCE
          (princ "\nEscaneando intersecciones...")
          (setq ssRed (ssget "_F" ptList '((0 . "LINE"))))

          (if ssRed 
            (progn 
              (setq i 0)
              (repeat (sslength ssRed) 
                (setq lineaObj (vlax-ename->vla-object (ssname ssRed i)))

                ;; --- LOGICA DE GEOMETRIA (V13) ---
                (setq p1 (vlax-safearray->list 
                           (vlax-variant-value (vla-get-StartPoint lineaObj))
                         )
                )
                (setq p2 (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint lineaObj))))
                (setq p1 (list (car p1) (cadr p1) 0.0)) ;; Aplanar Z
                (setq p2 (list (car p2) (cadr p2) 0.0))

                (setq distReal (distance p1 p2))
                (setq ang (angle p1 p2))
                (setq midPt (list (/ (+ (car p1) (car p2)) 2.0) 
                                  (/ (+ (cadr p1) (cadr p2)) 2.0)
                                  0.0
                            )
                )

                ;; Recorte
                (if (> distReal gapTotal) 
                  (setq distFinal (- distReal gapTotal))
                  (setq distFinal distReal)
                )

                ;; Insertar
                (setq bloqueObj (vla-InsertBlock modelSpace 
                                                 (vlax-3d-point midPt)
                                                 nombreBloque
                                                 1
                                                 1
                                                 1
                                                 0.0
                                )
                )
                (vla-put-Layer bloqueObj capaDestino)

                ;; Propiedades y Centrado
                (if bloqueObj 
                  (progn 
                    (setq props (vlax-variant-value (vla-GetDynamicBlockProperties bloqueObj)))
                    (foreach prop (vlax-safearray->list props) 
                      (setq nombreProp (vla-get-PropertyName prop))
                      (if (member nombreProp propsAdmitidas) 
                        (vla-put-Value prop distFinal)
                      )
                    )
                    (vla-Update bloqueObj)

                    ;; Re-centrar (Bounding Box)
                    (vla-GetBoundingBox bloqueObj 'minPt 'maxPt)
                    (setq minPt (vlax-safearray->list minPt))
                    (setq maxPt (vlax-safearray->list maxPt))
                    (setq centerPt (list 
                                     (/ (+ (car minPt) (car maxPt)) 2.0)
                                     (/ (+ (cadr minPt) (cadr maxPt)) 2.0)
                                     0.0
                                   )
                    )
                    (vla-Move bloqueObj 
                              (vlax-3d-point centerPt)
                              (vlax-3d-point midPt)
                    )
                    (vla-Rotate bloqueObj (vlax-3d-point midPt) ang)
                  )
                )
                (setq i (1+ i))
              )
              (princ (strcat "\nExito! Se sembraron " (itoa i) " cables."))
            )
            (princ "\nLa guia no cruza ninguna linea valida.")
          )
        )
        (alert "El objeto seleccionado no es una Polilinea.")
      )
    )
  )

  ;; Restaurar capa original
  (setvar "CLAYER" oldLayer)
  (vla-EndUndoMark doc)
  (princ)
)
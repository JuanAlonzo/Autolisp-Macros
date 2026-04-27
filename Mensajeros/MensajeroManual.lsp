; mensajeroManual.lsp
;
; Descripción:
; Inserta bloques de cable mensajero en líneas seleccionadas, ajustando la longitud del cable
; y gestionando capas.
;
; Uso:
; Ejecutar el comando 'SembrarMensajero' en AutoCAD.
; 



(defun c:SembrarMensajero (/ *error* doc modelSpace ss i lineaObj p1 p2 midPt ang 
                           distReal distFinal gap bloqueObj props prop nombreProp 
                           minPt maxPt centerPt propsAdmitidas
                          ) 
  (vl-load-com)

  ;; --- CONFIGURACION ---
  (setq nombreBloque "MensajeroP")
  (setq capaDestino "CABLE_MENSAJERO_P")

  ;; LISTA DE ALIAS: Agrega aqui cualquier nombre que pueda tener la propiedad
  (setq propsAdmitidas '("Distance" "Distancia" "Distance1" "Distancia1" "Length" 
                         "Longitud"
                        )
  )

  ;; ESTETICA: Espacio a reducir por lado
  (setq gap 1.5)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-ModelSpace doc))

  (princ "\nSeleccione las lineas de red (Cualquier capa): ")
  (setq ss (ssget '((0 . "LINE"))))

  (if ss 
    (progn 
      (vla-StartUndoMark doc)
      (setq i 0)

      (repeat (sslength ss) 
        (setq lineaObj (vlax-ename->vla-object (ssname ss i)))

        ;; 1. OBTENER DATOS (Aplanado a Z=0)
        (setq p1 (vlax-safearray->list (vlax-variant-value (vla-get-StartPoint lineaObj))))
        (setq p2 (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint lineaObj))))
        (setq p1 (list (car p1) (cadr p1) 0.0))
        (setq p2 (list (car p2) (cadr p2) 0.0))

        ;; Calculos
        (setq distReal (distance p1 p2))
        (setq ang (angle p1 p2))
        (setq midPt (list (/ (+ (car p1) (car p2)) 2.0) 
                          (/ (+ (cadr p1) (cadr p2)) 2.0)
                          0.0
                    )
        )

        ;; 2. CALCULO DE RECORTE
        (if (> distReal (* gap 2.0)) 
          (setq distFinal (- distReal (* gap 2.0)))
          (setq distFinal distReal)
        )

        ;; 3. INSERTAR BLOQUE
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

        ;; 4. APLICAR DISTANCIA (BUSQUEDA INTELIGENTE)
        (if bloqueObj 
          (progn 
            (setq props (vlax-variant-value (vla-GetDynamicBlockProperties bloqueObj)))

            ;; Iteramos por todas las propiedades del bloque
            (foreach prop (vlax-safearray->list props) 
              (setq nombreProp (vla-get-PropertyName prop))

              ;; VERIFICAMOS SI EL NOMBRE ESTA EN NUESTRA LISTA DE ADMITIDOS
              (if (member nombreProp propsAdmitidas) 
                (progn 
                  (vla-put-Value prop distFinal)
                  ;; (princ (strcat "\nPropiedad encontrada: " nombreProp)) ;; Debug opcional
                )
              )
            )

            (vla-Update bloqueObj)

            ;; 5. RE-CENTRAR
            (vla-GetBoundingBox bloqueObj 'minPt 'maxPt)
            (setq minPt (vlax-safearray->list minPt))
            (setq maxPt (vlax-safearray->list maxPt))
            (setq centerPt (list (/ (+ (car minPt) (car maxPt)) 2.0) 
                                 (/ (+ (cadr minPt) (cadr maxPt)) 2.0)
                                 0.0
                           )
            )
            (vla-Move bloqueObj (vlax-3d-point centerPt) (vlax-3d-point midPt))

            ;; 6. ROTAR
            (vla-Rotate bloqueObj (vlax-3d-point midPt) ang)
          )
        )
        (setq i (1+ i))
      )

      (vla-EndUndoMark doc)
      (princ (strcat "\nProceso finalizado. " (itoa i) " cables colocados."))
    )
    (princ "\nNo seleccionó ninguna linea.")
  )
  (princ)
)
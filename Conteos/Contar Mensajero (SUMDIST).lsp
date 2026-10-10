(defun c:SUMDIST (/ ss ss-validos doc bloqueObj props nombreReal distNeta distConGap 
                  propsAdmitidas i valProp cantBloques gap gapPorBloque
                 ) 
  (vl-load-com)

  (setq nombreBloque "MensajeroP")
  (setq gap 1.5)
  (setq gapPorBloque (* gap 2.0))

  (setq propsAdmitidas '("DISTANCE" "DISTANCIA" "DISTANCE1" "DISTANCIA1" "LENGTH" 
                         "LONGITUD"
                        )
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (princ 
    (strcat "\nSeleccione bloques '" 
            nombreBloque
            "' [o presione ENTER para seleccionar TODOS en el dibujo]: "
    )
  )
  (setq ss (ssget '((0 . "INSERT"))))

  (if (not ss) 
    (setq ss (ssget "X" '((0 . "INSERT"))))
  )

  (if ss 
    (progn 
      (setq distNeta 0.0)
      (setq ss-validos (ssadd))

      (repeat (setq i (sslength ss)) 
        (setq bloqueObj (vlax-ename->vla-object (ssname ss (setq i (1- i)))))

        (setq nombreReal (vlax-get-property bloqueObj 
                                            (if 
                                              (vlax-property-available-p bloqueObj 
                                                                         'EffectiveName
                                              )
                                              'EffectiveName
                                              'Name
                                            )
                         )
        )

        (if (= (strcase nombreReal) (strcase nombreBloque)) 
          (progn 
            (ssadd (vlax-vla-object->ename bloqueObj) ss-validos)

            (if (= (vla-get-IsDynamicBlock bloqueObj) :vlax-true) 
              (progn 
                (setq props (vlax-safearray->list 
                              (vlax-variant-value 
                                (vla-GetDynamicBlockProperties bloqueObj)
                              )
                            )
                )
                (foreach prop props 
                  (if (member (strcase (vla-get-PropertyName prop)) propsAdmitidas) 
                    (progn 
                      (setq valProp (vlax-variant-value (vla-get-Value prop)))
                      (if (numberp valProp) 
                        (setq distNeta (+ distNeta valProp))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      (if (> (sslength ss-validos) 0) 
        (progn 
          (setq cantBloques (sslength ss-validos))
          (setq distConGap (+ distNeta (* cantBloques gapPorBloque)))

          (sssetfirst nil ss-validos)

          (princ 
            (strcat "\n=============================================" 
                    "\n           RESUMEN DE MENSAJEROS             "
                    "\n============================================="
                    "\n Bloques encontrados      : "
                    (itoa cantBloques)
                    "\n Suma Neta (Distance)     : "
                    (rtos distNeta 2 2)
                    " m"
                    "\n Suma con Gap (+3.0m/und) : "
                    (rtos distConGap 2 2)
                    " m"
                    "\n============================================="
            )
          )

          (alert 
            (strcat "REPORTE DE MENSAJEROS (" 
                    nombreBloque
                    ")"
                    "\n\nBloques contabilizados: "
                    (itoa cantBloques)
                    "\n-------------------------------------------"
                    "\n• Longitud Neta (Distance): "
                    (rtos distNeta 2 2)
                    " m"
                    "\n• Longitud con Gap (+ "
                    (rtos gapPorBloque 2 1)
                    "m c/u): "
                    (rtos distConGap 2 2)
                    " m"
            )
          )
        )
        (princ (strcat "\nNo se encontraron bloques llamados '" nombreBloque "'."))
      )
    )
    (princ "\nNo se encontraron bloques en el dibujo.")
  )
  (princ)
)
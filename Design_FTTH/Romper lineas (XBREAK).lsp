;; Macro XBREAK (Reemplazo de funcion BA de YQARCH)

;; Se recomienda tener encendida solo la capa CAT_LINEA DE RED EXISTENTE

;; Recomendaciones:
;; - Explotar toda la linea de red para que todo el tramo sean solo lineas
;; - verificar que cruces y esquinas esten correctamente unidas

;; Una vez verificado lo anterior,usar la macro XBREAK
;; Iterará sobre cada tramo buscando donde cortar
;; Al finalizar verificar usando MIDLINES


(defun c:XBREAK (/ cruces_encontrados contador_seguridad) 
  (vl-load-com)
  (princ "\n--- XBREAK: Limpieza Iterativa de Red ---")

  (setq contador_seguridad 0)
  (setq cruces_encontrados T)

  ;; Iteramos mientras sigan apareciendo cruces (Máximo 20 pasadas para evitar bucles infinitos)
  (while (and cruces_encontrados (< contador_seguridad 20)) 
    (princ 
      (strcat "\nIteración de limpieza #" (itoa (1+ contador_seguridad)) "...")
    )

    ;; Ejecutamos la función de un solo pase de corte
    (if (not (XBREAK_SUB_PROC)) 
      (setq cruces_encontrados nil)
      (setq contador_seguridad (1+ contador_seguridad))
    )
  )

  (if (>= contador_seguridad 20) 
    (princ "\nFinalizado: Se alcanzó el límite de seguridad (revisa si hay líneas duplicadas).")
    (princ "\n--- ¡Proceso Completado! No se detectan más cruces. ---")
  )
  (princ)
)

;; Sub-proceso que busca y realiza UN corte por cada intersección encontrada
(defun XBREAK_SUB_PROC (/ ss i j ent1 ent2 obj1 obj2 int-pt p ss-at-point k 
                        ent-to-break corto_algo
                       ) 
  (setq corto_algo nil)
  (setvar "cmdecho" 0)

  (if (setq ss (ssget "X" '((0 . "LINE") (8 . "CAT_LINEA DE RED EXISTENTE"))))  ;; Filtra por tu capa para ir más rápido
    (progn 
      (setq i 0)
      (while (< i (sslength ss)) 
        (setq ent1 (ssname ss i))
        (if (entget ent1)  ;; Verificar que la entidad aún existe
          (progn 
            (setq obj1 (vlax-ename->vla-object ent1))
            (setq j (1+ i))
            (while (< j (sslength ss)) 
              (setq ent2 (ssname ss j))
              (if (and (entget ent2) (/= ent1 ent2)) 
                (progn 
                  (setq obj2 (vlax-ename->vla-object ent2))
                  (setq int-pt (vlax-invoke obj1 'IntersectWith obj2 acExtendNone))

                  (if int-pt 
                    (while (> (length int-pt) 0) 
                      (setq p (list (car int-pt) (cadr int-pt) (caddr int-pt)))

                      ;; Capturamos TODO lo que pase por este punto
                      (setq ss-at-point (ssget "_C" 
                                               (list (- (car p) 0.001) 
                                                     (- (cadr p) 0.001)
                                               )
                                               (list (+ (car p) 0.001) 
                                                     (+ (cadr p) 0.001)
                                               )
                                               '((0 . "LINE"))
                                        )
                      )

                      (if ss-at-point 
                        (repeat (setq k (sslength ss-at-point)) 
                          (setq ent-to-break (ssname ss-at-point (setq k (1- k))))

                          ;; Solo rompemos si NO es un extremo
                          (if 
                            (and (entget ent-to-break) 
                                 (not 
                                   (or 
                                     (equal p 
                                            (vlax-curve-getStartPoint ent-to-break)
                                            1e-4
                                     )
                                     (equal p 
                                            (vlax-curve-getEndPoint ent-to-break)
                                            1e-4
                                     )
                                   )
                                 )
                            )
                            (progn 
                              (vl-cmdf "._break" ent-to-break "_none" p "_none" "@")
                              (setq corto_algo T) ;; Marcamos que hubo un cambio
                            )
                          )
                        )
                      )
                      (setq int-pt (cdddr int-pt))
                    )
                  )
                )
              )
              (setq j (1+ j))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  corto_algo ;; Devuelve T si cortó algo, forzando otra iteración de la macro principal
)
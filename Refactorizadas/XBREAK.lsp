;; ============================================================================
;; Macro: XBREAK (Refactorizada)
;; Basada en: Design_FTTH/Romper lineas (XBREAK).lsp y romperlinea.lsp
;;
;; Mejoras implementadas:
;; - Unifica XBREAK y rompetepe en un único comando (XBREAK).
;; - Filtra automáticamente usando SOLO la capa activa actual (CLAYER).
;; - Agrupa toda la operación en un bloque Undo (vla-StartUndoMark).
;; - Reporta el total de cortes realizados y restaura variables de sistema si hay error.
;; ============================================================================

(vl-load-com)

(defun c:XBREAK (/ *error* doc old_cmdecho old_osmode capa_activa
                 cruces_encontrados contador_seguridad cortes_iteracion total_cortes)

  (defun *error* (msg)
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase (vl-princ-to-string msg)) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nError en XBREAK: " msg))
    )
    (princ)
  )

  (setq doc         (vla-get-ActiveDocument (vlax-get-acad-object))
        old_cmdecho (getvar "CMDECHO")
        old_osmode  (getvar "OSMODE")
        capa_activa (getvar "CLAYER")
  )

  (vla-StartUndoMark doc)
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  (princ "\n=======================================================")
  (princ "\n--- XBREAK: Limpieza Iterativa de Red (Capa Activa) ---")
  (princ (strcat "\nCapa activa filtrada: [" capa_activa "]"))
  (princ "\n=======================================================")

  (setq contador_seguridad 0
        total_cortes       0
        cruces_encontrados T
  )

  ;; Iteramos mientras sigan apareciendo cruces (máximo 20 pasadas de seguridad)
  (while (and cruces_encontrados (< contador_seguridad 20))
    (princ (strcat "\nIteración de limpieza #" (itoa (1+ contador_seguridad)) "..."))
    (setq cortes_iteracion (XBREAK_SUB_PROC_ACTIVA capa_activa))
    (if (> cortes_iteracion 0)
      (progn
        (setq total_cortes       (+ total_cortes cortes_iteracion)
              contador_seguridad (1+ contador_seguridad)
        )
        (princ (strcat " (" (itoa cortes_iteracion) " corte(s) realizados)"))
      )
      (setq cruces_encontrados nil)
    )
  )

  (if (>= contador_seguridad 20)
    (princ "\n[!] Finalizado: Se alcanzó el límite de 20 pasadas (revisa si hay líneas duplicadas/superpuestas).")
    (princ (strcat "\n--- ¡Proceso Completado! Total de cortes realizados en '" capa_activa "': " (itoa total_cortes) " ---"))
  )

  (setvar "OSMODE" old_osmode)
  (setvar "CMDECHO" old_cmdecho)
  (vla-EndUndoMark doc)
  (princ)
)

;; Sub-proceso que busca y realiza cortes por cada intersección en la capa activa
(defun XBREAK_SUB_PROC_ACTIVA (target_layer / ss i j ent1 ent2 obj1 obj2 int-pt p
                               ss-at-point k ent-to-break cant_cortes)
  (setq cant_cortes 0)

  (if (setq ss (ssget "_X" (list '(0 . "LINE") (cons 8 target_layer))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent1 (ssname ss i))
        (if (entget ent1)
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

                      ;; Capturar solo líneas de la capa activa que pasen por el punto de cruce
                      (setq ss-at-point
                        (ssget "_C"
                               (list (- (car p) 0.001) (- (cadr p) 0.001))
                               (list (+ (car p) 0.001) (+ (cadr p) 0.001))
                               (list '(0 . "LINE") (cons 8 target_layer))
                        )
                      )

                      (if ss-at-point
                        (repeat (setq k (sslength ss-at-point))
                          (setq ent-to-break (ssname ss-at-point (setq k (1- k))))

                          ;; Solo rompemos si NO es ya un extremo de la línea
                          (if (and (entget ent-to-break)
                                   (not (or (equal p (vlax-curve-getStartPoint ent-to-break) 1e-4)
                                            (equal p (vlax-curve-getEndPoint ent-to-break) 1e-4))))
                            (progn
                              (vl-cmdf "._break" ent-to-break "_none" p "_none" "@")
                              (setq cant_cortes (1+ cant_cortes))
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
    (princ (strcat "\nNo se encontraron objetos LINE en la capa activa '" target_layer "'."))
  )
  cant_cortes
)

(princ "\nComando XBREAK cargado (filtra automáticamente por la capa activa).")
(princ)

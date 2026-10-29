;; ============================================================================
;; Macro: Crear Capas Nodo Oficial (CCPOSTES / CNP / CAPA)
;; Unifica:
;;   - Capas/Crear Capas Nodo (CCPoste).lsp
;;   - Capas/Crear Capa Numeracion.lsp
;;
;; Mejoras implementadas:
;; - Deja la versión oficial estándar (separada por espacio y colores 2, 4 y 1).
;; - Sugiere automáticamente el nombre del archivo DWG actual como valor por defecto
;;   al presionar ENTER.
;; - Crea o actualiza las capas mediante entmake/tblobjname sin cambiar la capa activa
;;   del usuario accidentalmente.
;; ============================================================================

(vl-load-com)

(defun ccpostes:crear-o-actualizar-capa (nombre color / tbl ed)
  (if (not (setq tbl (tblobjname "LAYER" nombre)))
    (entmake
      (list '(0 . "LAYER")
            '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbLayerTableRecord")
            (cons 2 nombre)
            '(70 . 0)
            (cons 62 color)
      )
    )
    (progn
      (setq ed (entget tbl))
      (setq ed (subst (cons 62 color) (assoc 62 ed) ed))
      (setq ed (subst (cons 70 (logand (cdr (assoc 70 ed)) (~ 1))) (assoc 70 ed) ed))
      (entmod ed)
    )
  )
)

(defun c:CCPOSTES (/ dwg_default plano capa1 capa2 capa3 col_exist col_proy col_apoyo)
  (setq dwg_default (vl-filename-base (getvar "DWGNAME")))

  (setq plano
    (getstring T
      (strcat "\nIngrese el código del plano <" dwg_default ">: ")
    )
  )

  (if plano
    (setq plano (vl-string-trim " " plano))
  )

  (if (or (null plano) (= plano ""))
    (setq plano dwg_default)
  )

  ;; Colores estándar oficiales:
  ;; 2 = Amarillo (Existentes) | 4 = Cian (Proyectados) | 1 = Rojo (Apoyo)
  (setq col_exist 2
        col_proy  4
        col_apoyo 1
  )

  ;; Nombres exactos según estándar oficial
  (setq capa1 (strcat "NUMERACION DE POSTES EXISTENTES " plano)
        capa2 (strcat "NUMERACION DE POSTES PROYECTADOS " plano)
        capa3 (strcat "NUMERACION DE POSTES PROYECTADOS DE APOYO " plano)
  )

  (ccpostes:crear-o-actualizar-capa capa1 col_exist)
  (ccpostes:crear-o-actualizar-capa capa2 col_proy)
  (ccpostes:crear-o-actualizar-capa capa3 col_apoyo)

  (princ (strcat "\n--- Capas generadas para [" plano "] ---"))
  (princ (strcat "\n -> " capa1 " (Color " (itoa col_exist) " - Amarillo)"))
  (princ (strcat "\n -> " capa2 " (Color " (itoa col_proy) " - Cian)"))
  (princ (strcat "\n -> " capa3 " (Color " (itoa col_apoyo) " - Rojo)"))
  (princ)
)

;; Alias de compatibilidad
(defun c:CNP  () (c:CCPOSTES))
(defun c:CAPA () (c:CCPOSTES))

(princ "\nComandos CCPOSTES, CNP y CAPA cargados.")
(princ)

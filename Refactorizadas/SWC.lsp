;; ============================================================================
;; Macro: Selector Unificado Dentro de Curvas (SWC / ICS / WPS)
;; Unifica:
;;   - MACRO_InsideSelector(IMP).lsp (ICS)
;;   - MACRO_InsideSelectorCircle(IMP).lsp (SWC)
;;   - MACRO_InsideSelector_GLOBAL(IMP).lsp (WPS)
;;
;; Mejoras implementadas:
;; - Soporta múltiples contornos simultáneos (Círculos, Elipses, Polilíneas con o sin arcos).
;; - No depende de Express Tools (ACET-*).
;; - Permite elegir entre modo [Captura (CP) / Ventana (WP)] recordando la última elección.
;; - Excluye los propios bordes de contorno de la selección resultante.
;; ============================================================================

(vl-load-com)

(defun swc:muestrear-curva (e segmentos / len paso d lst pt)
  (setq len  (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e))
        paso (/ len (float segmentos))
        d    0.0
        lst  '()
  )
  (repeat segmentos
    (if (setq pt (vlax-curve-getPointAtDist e d))
      (setq lst (cons (trans pt 0 1) lst))
    )
    (setq d (+ d paso))
  )
  (reverse lst)
)

(defun c:SWC (/ modo kw ss add i ent pts temp i2 e-sub)
  (if (null *swc:modo*) (setq *swc:modo* "Captura"))

  (initget "Ventana Captura")
  (if (setq kw (getkword (strcat "\nModo de selección interior [Ventana/Captura] <" *swc:modo* ">: ")))
    (setq *swc:modo* kw)
  )
  (setq modo (if (= *swc:modo* "Ventana") "_WP" "_CP"))

  (princ "\nSeleccione polilíneas cerradas, círculos o elipses de borde: ")
  (if (setq ss (ssget '((0 . "CIRCLE,ELLIPSE,*POLYLINE,SPLINE"))))
    (progn
      (setq add (ssadd))
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq pts (swc:muestrear-curva ent 120))
        (if (and pts (setq temp (ssget modo pts)))
          (repeat (setq i2 (sslength temp))
            (setq e-sub (ssname temp (setq i2 (1- i2))))
            ;; No incluir el propio contorno seleccionado
            (if (not (ssmemb e-sub ss))
              (ssadd e-sub add)
            )
          )
        )
      )
      (if (> (sslength add) 0)
        (progn
          (sssetfirst nil add)
          (princ (strcat "\n>>> " (itoa (sslength add)) " objeto(s) seleccionado(s) [" *swc:modo* "]."))
        )
        (princ "\nNo se encontraron objetos dentro del contorno.")
      )
    )
    (princ "\nNo se seleccionaron contornos válidos.")
  )
  (princ)
)

;; Alias compatibles con los comandos anteriores
(defun c:ICS () (c:SWC))
(defun c:WPS () (c:SWC))

(princ "\nSelector unificado cargado. Comandos disponibles: SWC, ICS, WPS.")
(princ)

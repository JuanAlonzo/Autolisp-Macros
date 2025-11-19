;********************************

(defun c:st (/ ss num ent oper text-found)
  ; "st" es "Suma Textos"
  ; Este programa suma los números señalados en texto
  ; Los textos hechos con la orden MTEXT hay que explotarlos previamente para sumarlos.
  ; Los textos con decimales deben estar separados con punto y no con coma (,) para que la rutina los considere
  ; Autor: Manuel monroy pagnon, arquitecto. mmonroy@mundofree.com   Hecha el 23 de enero de 2002

  (setq oper 0)
  (setq text-found nil)

  (if (setq ss (ssget))
    (progn
      (setq num 0)
      (repeat (sslength ss)
        (setq ent (ssname ss num))
        (if (or (eq (cdr (assoc 0 (entget ent))) "TEXT")
                (eq (cdr (assoc 0 (entget ent))) "MTEXT"))
          (progn
            (setq AAA (cdr (assoc 1 (entget ent))))
            (setq AREAE (atof AAA))
            (setq oper (+ AREAE oper))
            (setq text-found t)
          )
        )
        (setq num (1+ num))
      )
    )
  )

  (if text-found
    (progn
      (setq ptA (getpoint "Pinchar Punto inserción del rótulo "))
      (terpri)
      (command "_TEXT" ptA 1.5 "" (strcat "" (rtos oper 2 2) ""))
    )
    (alert "No se encontraron textos válidos para sumar.")
  )
)

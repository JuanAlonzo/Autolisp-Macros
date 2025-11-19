;********************************

(defun c:st ( / conj num ent ) ; "st" es "Suma Textos"
;Este programa suma los numeros señalados en texto
;Los textos hechos con la orden MTEXT hay que explotarlos previamente para sumarlos.
;Los textos con decimales deben estar separados con punto y no con coma (,) para que la rutina los considere
;Autor: Manuel monroy pagnon, arquitecto. mmonroy@mundofree.com   Hecha el 23 de enero de 2002

    (setq lista nil)
    (setq oper 0)


    (if (setq conj (ssget)) ; seleccionamos los textos
        (progn

        
        (setq num 0 )

        (while
	(setq ent (ssname conj num))
	(setq AAA (cdr (assoc 1 (entget ent))))
        (setq AREAE (ATOF AAA))
        (setq num  (1+ num))

       	(setq oper (+ AREAE oper))

) ;Parentesis de While
) ;Parentesis de progn
) ;Parentesis de if

(setq ptA (getpoint "Pinchar Punto inserción del rótulo "))(terpri)
(COMMAND "_TEXT" ptA "" "" (STRCAT "" (RTOS OPER 2 2) "" ))

)
; NUMERA.LSP
; Rotula automáticamente números consecutivos en el dibujo.

; Versión 1.2: febrero de 2001: Acepta incremento y origen en números no enteros. Gestiona número de decimales
;
; Versión 2.0: Acepta prefijos y sofijos
; Versión 2.1: Permite que los prefijos y sufijos contengan espacios en blanco 
;
; Futuras versiones: 
;	- Seleccionando un texto ya existente en el dibujo que contenga un número,
;	  rotula a partir del siguiente valor.
;	- Permite rotular automáticamente en forma de matriz o línea
;
;
; Julio Pablo
; Visita Las Quimbambas en http://pagina.de/lasquimbambas
;

(princ "\nnumera 2.1: Introduce automáticamente números consecutivos en el dibujo, en los puntos que indiquemos\n")

(defun c:numera(/ contador incremento frasecitainformativa opciondeturno)

 (setq contador 1.00)
 (setq incremento 1.00)
 (setq opciondeturno "")
 (if (null alturatexto) (setq alturatexto (getvar "TEXTSIZE")))
 (if (null prefijo) (setq prefijo ""))
 (if (null sufijo) (setq sufijo ""))
 (if (null ndecimales) (setq ndecimales 0))
 (setq opciondeturno "")

 (while opciondeturno
  (initget "Altura Origen Incremento Prefijo Sufijo Decimales")
  (setq rotulodeturno (strcat prefijo (rtos contador 2 ndecimales) sufijo))
  (setq 
	frasecitainformativa (strcat "\nAltura/Origen/Incremento/Prefijo/Sufijo/Decimales/<punto \"" rotulodeturno "\">:")
  ) 
  (setq puntodeturno (getpoint frasecitainformativa))
  (setq opciondeturno 

   (cond
    ( ;si el usuario pulsa A
     (eq puntodeturno "Altura")
     (setq nuevovalor (getdist (strcat "\n(Si el estilo vigente no tiene definida altura=0, este valor no será efectivo). Altura de texto <" (rtos alturatexto) ">:")))
     (if (> nuevovalor 0) (setq alturatexto nuevovalor))
     (princ "\naltura:")(princ alturatexto)
   )  

    ( ;si el usuario pulsa O
     (eq puntodeturno "Origen")
     (setq nuevovalor 
       (getreal 
        (strcat "\nIntroduce el valor inicial <" (rtos contador) ">:")
       );getreal
     );setq
     (setq contador nuevovalor)
     (princ "\nOrigen:")(princ contador)
     (setq opciondeturno "")
    )  

    ( ; si el usuario pulsa I
     (eq puntodeturno "Incremento")
     (setq nuevovalor 
       (getreal 
        (strcat "\nIntroduce el incremento <" (rtos incremento) ">:")
       );getreal
     );setq
     (setq incremento nuevovalor)
     (princ "\nOrigen:")(princ incremento)
     (setq opciondeturno "")
    )  

    ( ; si el usuario pulsa P
     (eq puntodeturno "Prefijo")
     (setq prefijo (getstring T "\nIntroduce el prefijo:"))
    )  

    ( ; si el usuario pulsa S
     (eq puntodeturno "Sufijo")
     (setq sufijo (getstring T "\nIntroduce el sufijo:"))
    )  

    ( ; si el usuario pulsa D
     (eq puntodeturno "Decimales")
     (setq nuevovalor (getint (strcat "\nPrecisión. Cifras decimales <" (itoa ndecimales) ">:")))
     (if (>= nuevovalor 0) (setq ndecimales nuevovalor))
     (princ "\nCifras decimales:")(princ ndecimales)
     (setq opciondeturno "")

    )  

    ( ;si la respuesta del usuario es marcar un punto...
     (eq (type puntodeturno) 'LIST) 
     (command "_text" "_mc" puntodeturno alturatexto "" rotulodeturno)
     (setq contador (+ contador incremento))
    );[opción]

    (; si el usuario pulsa [intro]
     T nil
    )

   );cond 

  );setq opciondeturno
 ) ;while

 (princ "\nMás órdenes como ésta en http://pagina.de/juliopablo\n")
 (prin1)
);defun





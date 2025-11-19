;       AREAROTULO.LSP 2.2: Seleccionando un punto, busca su contorno cerrado,
;			    calcula su área y coloca un rótulo con una sola 
;                           pulsación.
;
;	Versión 1.2b: Funciona en cualquier idioma
;	Versión 2.0, permite:
;		Elegir el prefijo y el sufijo
; 		Indicar redondeo y número de decimales
;	Versión 2.1, el redondeo se define en función a parte
;	Versión 2.2, prefijo y sufijo admiten espacios en blanco.
;
;	Futuras versionas:
;		Podrá optar entre descontar islas o tomarlas en cuenta
;
;       Orden:AREAR
;
;       Julio Pablo. Marzo, 2001.
;	Correo electrónico: 	 estuvoaqui@altavista.net
;	Sitio web:		 http://pagina.de/lasquimbambas
;

; Esta orden comparte ciertas variables con SUMAREA.LSP para que, cuando cambiemos el valor de redondeo, 
; altura de texto y/o cifras decimales en una tambien afecte a la otra para seguir un criterio lógico 
; de rotulación.


(princ "\nCargado AREAROTULO 2.2: Rotula el área de un contorno cerrado.\nPara que esta orden funcione, el estilo vigente debe tener altura=0. ")

(defun redondea (valorbruto valorredondeo / cociente resto)
 (if (/= 0 valorredondeo)
  (progn
   (setq cociente (fix (/ valorbruto valorredondeo)))
   (setq resto (- valorbruto (* valorredondeo cociente)))
   (if (> resto (/ valorredondeo 2))
    (setq redondeada (+ valorbruto (- valorredondeo resto)))
    (setq redondeada (- valorbruto resto))
   ); if
  );progn
  ; si el redondeo es cero, devuelve el valor bruto sin redondear
  (setq redondeada valorbruto)
 );if redondeo distinto de 0
 (eval redondeada)
);defun redondea

(defun c:arear ()

 (if (null redondeo) (setq redondeo 0.05))
 (if (null ndecimales) (setq ndecimales (getvar "luprec")))
 (if (null prefijo) (setq prefijo "SUP.:"))
 (if (null sufijo) (setq sufijo "m2"))
 (if (null alturatexto) (setq alturatexto (getvar "TEXTSIZE")))
 (setq opciondeturno "")

 (setq menueco (getvar "cmdecho"))
 (setvar "cmdecho" 0)

; Se muestran los valores actuales de las variables
 (princ "\nAREAR: Rotula áreas de contornos cerrados. Altura=") 
 (princ alturatexto)
 (princ " decimales=")
 (princ ndecimales)
 (princ " redondeo=")
 (princ redondeo)
 (princ " prefijo:\"")
 (princ prefijo)
 (princ "\" sufijo:\"")
 (princ sufijo)
 (princ "\".")

; Selección de las distintas opciones: meollo
 (while opciondeturno
  (setq frasecitainformativa "\nAltura del texto/Decimales/Redondeo/Prefijo/Sufijo/<punto interior>")
  (initget "Altura Decimales Redondeo Prefijo Sufijo")

  (setq puntodeturno (getpoint frasecitainformativa))
  (setq opciondeturno
   (cond 

    ( ;si la respuesta del usuario es Altura
     (eq puntodeturno "Altura") 
     (setq nuevovalor (getdist (strcat "\n(Si el estilo vigente no tiene definida altura=0, este valor no será efectivo).\n Altura de texto (0 para usar valor del estilo actual) <" (rtos alturatexto) ">:")))
     (if (> nuevovalor 0) (setq alturatexto nuevovalor))
     (IF (= nuevovalor 0) (setq alturatexto (getvar "TEXTSIZE")))
     (princ "\naltura:")(princ alturatexto)
     (setq opciondeturno "")
    );[opción Altura]

    ( ;si la respuesta del usuario es Decimales
     (eq puntodeturno "Decimales") 
     (setq nuevovalor (getint (strcat "\nCifras decimales <" (itoa ndecimales) ">:")))
     (if (>= nuevovalor 0) (setq ndecimales nuevovalor))
     (princ "\nCifras decimales:")(princ ndecimales)
     (setq opciondeturno "")
    );[opción Decimales]

    ( ;si la respuesta del usuario es Redondeo
     (eq puntodeturno "Redondeo") 
     (setq nuevovalor (getreal (strcat "\nValor de redondeo <" (rtos redondeo) ">:")))
     (if (>= nuevovalor 0) (setq redondeo nuevovalor))
     (princ "\nredondeo:")(princ redondeo)
     (setq opciondeturno "")
    );[opción Redondeo]

    ( ;si la respuesta del usuario es Prefijo
     (eq puntodeturno "Prefijo") 
     (setq prefijo (getstring T (strcat "\nPrefijo (actual:\"" prefijo "\"):")))
     (princ "\nPrefijo:\"")(princ prefijo)(princ "\"")
    );[opción Prefijo]

    ( ;si la respuesta del usuario es Sufijo
     (eq puntodeturno "Sufijo") 
     (setq sufijo (getstring T (strcat "\nSufijo (actual:\"" sufijo "\"):")))
     (princ "\nSufijo:\"")(princ sufijo)(princ "\"")
    );[opción Sufijo]

    ( ;si la respuesta del usuario es marcar un punto...
     (eq (type puntodeturno) 'LIST) 
      (command "_-boundary" "_a" "_i" "_n" "_n" "_x" puntodeturno "")
      (command "_area" "_o" "_l")
      (command "_erase" "_l" "")
      (setq superficie (getvar "area"))
      
      (setq rotulo (strcat prefijo (rtos (redondea superficie redondeo) 2 ndecimales) sufijo))
      (command "_text" "_c" puntodeturno alturatexto "" rotulo)
      (setq opciondeturno "")
     );[opción: un punto]

     (; si el usuario pulsa [intro]
     T nil
    )

   ); cond
  ); setq opciondeturno
 );while

 (setvar "cmdecho" menueco)
 (princ "\nOrden perpetrada por Julio Pablo. Gracias por utilizarla\n")
 (prin1)
)
;-------------


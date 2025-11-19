; Obtener la suma de las líneas o las polilíneas seleccionadas.

; Programa desarrollado por Mario Torres Pejerrey
; http://www.construcgeek.com/

;|Este es un lisp en formato original, se puede ver el código fuente, la intención, es de que el código fuente
; pueda ser modificado y adaptado a la necesidad de cada usuario, lo único que siempre se solicita en estos
; casos es de que siempre se haga referencia al autor del mismo (es decir que no se modifique la autoría del lisp),
; salvo que este se modifique ampliamente, si se construye un nuevo programa tomando como partes un lisp publicado,
; se debería de hacer el comentario de que parte del nuevo programa esta basado en el autor original.|;

; Programa descargado desde http://www.construcgeek.com/
; ConstrucGeek 2008

;Cargar las funciones ActiveX (Visual Lisp)
(vl-load-com)

(defun c:lpl()

;Variable para resumir la ruta de ubicacion en el registro de AX
  (setq Clave "HKEY_CURRENT_USER\\Software\\Construcgeek.com\\Longitud linea.Lsp\\")
  
;Verificamos el numero de decimales
  (setq numDecimalesDef (vl-registry-read (strcat Clave "Opciones") "Número de decimales"))
  (if (null numDecimalesDef)(setq numDecimalesDef 2)) ;Si no esta almacenado en el registro: Nro de decimales por defecto
  
  
  
(setq len 0)
(setq n 0)

(setq MSGDECIMALES (STRCAT "\nIngrese el número de decimales para las longitudes <" (ITOA NUMDECIMALESDEF) ">:"))
(setq NUMDECIMALES (GETINT MSGDECIMALES))

(if (null NUMDECIMALES)
	(setq NUMDECIMALES NUMDECIMALESDEF)
	(progn
		(setq NUMDECIMALESDEF NUMDECIMALES)
		(VL-REGISTRY-WRITE (strcat Clave "Opciones") "Número de decimales" NUMDECIMALES)
	)
)
			

	(princ "\nSeleccione las lineas o  polilineas a hallar la longitud: ")
	(if (setq entidades (ssget)) 
		(progn
			(setq numents (sslength entidades))
	
			(repeat numents
				(setq
				  		ename (ssname entidades n)
				  		entl  (entget ename)
				  		en    (cdr (assoc 0 entl))
				)
			
			  	(setq	oname (vlax-ename->vla-object ename))
			    (setq	param (vlax-curve-getendParam oname))
			    (setq	lenHallada (vlax-curve-getDistAtParam oname param))
			    
			    (setq Len (+ len lenHallada))
			    (setq n(+ n 1))
			    
			)
	
			(if (/= len 0)
				(progn  
					(setq entSup (car (entsel "\nSeleccione el texto a reemplazar con la suma de las longitudes obtenidas o <Enter para terminar>: ")))
		
					(if (not (null entSup))
				  		(progn
				        	(setq entSupvla (vlax-ename->vla-object entsup))
							(setq texto (vla-get-TextString entSupvla))
			
							(setq cont 0)
					  
							(while
					  			(and (> (strlen texto) cont) (/= (substr texto (- (strlen texto) cont) 1) "=")  (<= (- cont 1) (strlen texto)))
					 	  		(setq cont (1+ cont))
					  		)
					  		  
							(setq TextoDescripcion (substr texto 1 (- (strlen texto) cont)))
							(if (= TextoDescripcion "")(setq TextoDescripcion "Longitud ="))
			
							(setq Longitud (strcat TextoDescripcion "" (rtos len 2 NUMDECIMALESDEF) " ml"))
			
				        	(vla-put-TextString entSupvla Longitud)
						)
						(progn
				  			(princ (strcat "\nLongitud: " (rtos len 2 NUMDECIMALESDEF) " ml"))
				  			(princ)
						)
			     	)
			    )
			)
		)
	)
	(princ)

)

(setvar "modemacro" "http://www.construcgeek.com/")
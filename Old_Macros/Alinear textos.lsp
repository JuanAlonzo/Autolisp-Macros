; Rutina para alinear varios textos tomando como base el primero seleccionado

; Programa desarrollado por Mario Torres Pejerrey
; http://cadlisp.mariotorres.pe/
; https://www.patreon.com/mariotorres

;|Este es un lisp en formato original, se puede ver el código fuente, la intención, es de que el código fuente
; pueda ser modificado y adaptado a la necesidad de cada usuario, lo único que siempre se solicita en estos
; casos es de que siempre se haga referencia al autor del mismo (es decir que no se modifique la autoría del lisp),
; salvo que este se modifique ampliamente, si se construye un nuevo programa tomando como partes un lisp publicado,
; se debería de hacer el comentario de que parte del nuevo programa esta basado en el autor original.|;

(defun c:alit()

	;Para evitar en caso de cancelar apararezca el mensaje "; error: Function cancelled"
	(setq old_err *error*)(defun *error* ( a / )(princ "") (setq *error* old_err)(princ))

	(setq ents 0)
	
	(if (null LtraSDef)(setq LtraSDef "Vertical"))
	
	(initget "Vertical Horizontal")
	(setq LtraS (getkword (strcat "\nIngrese el sentido de la alineación [Vertical/Horizontal]<" LtraSDef ">: ")))
	
	(if (null LtraS)(setq LtraS LtraSDef))(setq LtraSDef LtraS)
	
	;Repetir las acciones mientras se seleccione un texto base
	(while
		(setq ents(entsel "\nSeleccione la entidad base: "))
		;Si no es nula la selección
		(if (not (null ents))
			(progn
				(setq ents(car ents))
				(setq ents(entget ents))
				
				;Obtenemos la justificación de texto
				;Si el objeto es un texto
				(if (= (cdr (assoc 0 ents)) "TEXT")
					(progn
						(setq just (cdr (assoc 72 ents)))
						
						(cond
							((= just 0) (setq sxi 10)) 
							((= just 1) (setq sxi 11))
							((= just 2) (setq sxi 11)) 
						)
					)
					(setq sxi 10) ;Si el objeto es una linea
				)
				
				(princ "\nSeleccione las entidades a alinear:")
				
				(if (setq textos(ssget))
					(progn
					
						(setq numt(sslength textos))(setq n 0)
						
						;Repetimos las acciones el número de veces de los textos seleccionados
						(repeat numt
							(setq enti(ssname textos n))
							(setq enti(entget enti))
							
							(setq just (cdr (assoc 72 enti)))
							
							(cond
								((= just 0) (setq sx 10)) 
								((= just 1) (setq sx 11))
								((= just 2) (setq sx 11)) 
							)
							
							(setq ps1(cdr(assoc sxi ents)))
							
							(if (= LtraS "Vertical")
								(setq x(car ps1)) 	;Alineación vertical
								(setq y(cadr ps1)) 	;Alineación horizontal
							)
							
							(setq ps2(cdr(assoc sxi enti)))
							
							(if (= LtraS "Vertical")
								(setq y(cadr ps2))	;Alineación vertical
								(setq x(car ps2)) 	;Alineación horizontal
							)
							
							(setq punto(list x y))
							
							(setq nent	(SUBST (CONS sx punto)(assoc sx enti)enti))
							(entmod nent)
							(setq n(+ n 1))
						)
					)
				)
				
				(setq msg (strcat (rtos numt 2 0) " textos alineados."))
				
				(princ msg)
			) ;Fin de Progn
		);Fin de If
	);Fin de While

(princ)
)


(SETVAR "modemacro" "https://www.patreon.com/mariotorres")
















(SETVAR "modemacro" "MTP")
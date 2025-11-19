; Rutina lisp para mostrar las propiedades y métodos de un objeto gráfico

; Programa Desarrollado por Mario Torres Pejerrey
; http://www.construcgeek.com/

;|Este es un lisp en formato original, se puede ver el código fuente, la intención, es de que el código fuente
; pueda ser modificado y adaptado a la necesidad de cada usuario, lo único que siempre se solicita en estos
; casos es de que siempre se haga referencia al autor del mismo (es decir que no se modifique la autoría del lisp),
; salvo que este se modifique ampliamente.
; Si se construye un nuevo programa tomando como partes un lisp publicado, se deberá de hacer el comentario
; de que parte del nuevo programa esta basado en el autor original.|;

; Programa descargado desde: http://www.construcgeek.com
; ConstrucGeek Blog y Foros!
; ©2008-2009

; Cargar las funciones ActiveX (Visual Lisp)
(vl-load-com)


(defun C:OBT_OBJ (/ OBJ)
	(vlax-dump-object
		(vlax-ename->vla-object
			(car (entsel "\nSeleccione el objeto: "))
		)
		'T
	)
	(princ)
	(textscr)
)

(SETVAR "modemacro" "http://www.construcgeek.com/")
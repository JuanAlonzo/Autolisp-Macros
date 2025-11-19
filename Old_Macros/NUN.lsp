(defun c:NUM () 
  (tblobjname "STYLE" "Borrar") 
(command "_-style" "Borrar" "verdana" "0" "1" "0" "_no" "_no") 
(command "_-layer" "make" "Borrar" "_color" "5" "Borrar" "") 
  
(setq numero (getint "\n Nº...")) 
(setq factor (getreal "\Factor")) 
  
 (while 
  (setq punto (getpoint "Punto: ")) 
  (setq producto (* numero factor)) 
  (command "_text" "_C" punto 1.5 0 (rtos producto 2 0)) 
  (setq numero (1+ numero)) 
  ) 
 ) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Fecha: 22/03/2010                 Comando: NUM   V.1.00 ;;; 
;;; Desarollado por EVAIR P. UVINHA   Jundiaí / SP - Brasil ;;; 
;;;     ___                   ___       ___                 ;;; 
;;;    |    \      / /\    | |   |     |   |     |   |      ;;; 
;;;    |___  \    / /  \   | |___|     |___|     |   |      ;;; 
;;;    |      \  / /____\  | |  \      |         |   |      ;;; 
;;;    |___    \/ /      \ | |   \     |   .     |___| .    ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;  Este pequeño programa se caracteriza por permitir que  ;;; 
;;;  el usuario:                                            ;;; 
;;;  - Comprobar la altura del texto que se inserta;        ;;; 
;;;  - Controlar el crecimiento de los valores que se       ;;; 
;;;    insertan;                                            ;;; 
;;;  - Controle el número de decimales (máximo 14);         ;;; 
;;;  - Controlar el valor inicial que se inserta;           ;;; 
;;;  - Continuar de un número previamente existente que fue ;;; 
;;;    seleccionado;                                        ;;; 
;;;  - Continuar de un número introducido manualmente;      ;;; 
;;;  - Todos los valores adoptados se conservan después de  ;;; 
;;;    cerrado el proyecto para que se pueda continuar su   ;;; 
;;;    servicio la próxima vez que abra este proyecto.      ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;   CUALQUIER ERROR QUE ENCUENTRA, POR FAVOR ME APORTEN   ;;; 
;;;  PARA QUE LO PUEDA CORRIGIR. ESPERO SEA ÚTIL A ALGUIÉN  ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun c:NUM (/ *error* Método OldOsnapMode OldTextSize Punto) 
  (defun *error* (msg) 
    (alert 
      " 
-------------------------------------------------- 
|   LA EJECUCIÓN DEL PROGRAMA FUÉ   | 
|   INTERRUMPIDA INESPERADAMENTE   | 
-------------------------------------------------- 
     " 
    ) 
  ) 
  (vl-load-com) 
  (setq OldTextSize (getvar "textsize")) 
  (setq OldOsnapMode (getvar "osmode")) 
  (initget 1 "CONTINUAR CONFIGURAR") 
  (setq   Método 
    (getkword 
      "\n¿Configurar antes?  < CONTINUAR> [CONTINUAR/CONFIGURAR]" 
    ) 
  ) 
  (cond 
    ((= Método "CONTINUAR") 
     (if (not (dictsearch (namedobjdict) "NUMDICTIONARY")) 
       (progn 
    (vlax-ldata-put "NUMDICTIONARY" "ALTURA" OldTextSize) 
    (vlax-ldata-put "NUMDICTIONARY" "INCREMENTO" 1) 
    (vlax-ldata-put "NUMDICTIONARY" "DECIMAL" 2) 
    (vlax-ldata-put "NUMDICTIONARY" "TEXTO" 1) 
    (NumDictionary) 
       ) 
     ) 
    ) 
    ((= Método "CONFIGURAR") 
     (if (not (dictsearch (namedobjdict) "NUMDICTIONARY")) 
       (progn 
    (vlax-ldata-put "NUMDICTIONARY" "INCREMENTO" 1) 
    (vlax-ldata-put "NUMDICTIONARY" "DECIMAL" 2) 
    (vlax-ldata-put "NUMDICTIONARY" "TEXTO" 1) 
       ) 
     ) 
     (NumDictionary) 
    ) 
  ) 
  (setq Punto (getpoint "\nHaz click en el punto de inserción:")) 
  (while Punto 
    (vl-cmdf "_.text" 
        "MC" 
        Punto 
        (vlax-ldata-get "NUMDICTIONARY" "ALTURA") 
        "1.00" 
        (rtos (vlax-ldata-get "NUMDICTIONARY" "TEXTO") 
         2 
         (vlax-ldata-get "NUMDICTIONARY" "DECIMAL") 
        ) 
    ) 
    (vlax-ldata-put 
      "NUMDICTIONARY" 
      "TEXTO" 
      (+ (vlax-ldata-get "NUMDICTIONARY" "TEXTO") 
    (vlax-ldata-get "NUMDICTIONARY" "INCREMENTO") 
      ) 
    ) 
    (setq Punto (getpoint "\nHaz click en el punto de inserción:")) 
  ) 
  (setvar "osmode" OldOsnapMode) 
  (setvar "textsize" OldTextSize) 
  (princ) 
) 

(defun NumDictionary (/ Incremento Selección Temporário) 
  (setq   Temporário 
    (getreal 
      (strcat "\nDigite la altura del texto:  < " 
         (vl-princ-to-string 
           (vlax-ldata-get "NUMDICTIONARY" "ALTURA") 
         ) 
         " >" 
      ) 
    ) 
  ) 
  (if Temporário 
    (vlax-ldata-put "NUMDICTIONARY" "ALTURA" Temporário) 
  ) 
  (setq   Temporário 
    (getreal 
      (strcat 
        "\nDigite el incremento a usar:  < " 
        (vl-princ-to-string 
          (vlax-ldata-get "NUMDICTIONARY" "INCREMENTO") 
        ) 
        " >" 
      ) 
    ) 
  ) 
  (if Temporário 
    (vlax-ldata-put 
      "NUMDICTIONARY" 
      "INCREMENTO" 
      Temporário 
    ) 
  ) 
  (setq   Temporário 
    (getint 
      (strcat 
        "\nDigite el número de casas decimales a usar:  < " 
        (vl-princ-to-string 
          (vlax-ldata-get "NUMDICTIONARY" "DECIMAL") 
        ) 
        " >" 
      ) 
    ) 
  ) 
  (while (> Temporário 14) 
    (alert "MÁXIMO PERMITIDO:  14 DECIMALES") 
    (setq Temporário 
      (getint 
        (strcat 
          "\nDigite el número de casas decimales a usar:  < " 
          (vl-princ-to-string 
       (vlax-ldata-get "NUMDICTIONARY" "DECIMAL") 
          ) 
          " >" 
        ) 
      ) 
    ) 
  ) 
  (if Temporário 
    (vlax-ldata-put 
      "NUMDICTIONARY" 
      "DECIMAL" 
      Temporário 
    ) 
  ) 
  (vlax-ldata-put 
    "NUMDICTIONARY" 
    "TEXTO" 
    (progn 
      (initget 1 "DIGITAR CONTINUAR") 
      (setq Método 
        (getkword 
          "\n¿Como definir el primer valor a insertar?  <DIGITAR> [DIGITAR/CONTINUAR]" 
        ) 
      ) 
      (cond 
   ((= Método "DIGITAR") 
    (initget 1) 
    (setq Temporário 
      (getreal 
        "\nDigite el primer valor a insertar:" 
      ) 
    ) 
    (if Temporário 
      (vlax-ldata-put "NUMDICTIONARY" "TEXTO" Temporário) 
    ) 
   ) 
   ((= Método "CONTINUAR") 
    (setq Selección 
      (car 
        (entsel 
          "\nSelecciona el valor para seguir la numeración:" 
        ) 
      ) 
    ) 
    (while 
      (and   Selección 
      (not 
        (= (cdr (assoc 0 (entget Selección))) "TEXT") 
      ) 
      ) 
       (alert 
         " 
----------------------------------------- 
|    El objeto seleccionado no es    | 
|   válido. Selecciona otra vez !!!  | 
----------------------------------------- 
         " 
       ) 
       (setq Selección 
         (car 
           (entsel) 
         ) 
       ) 
    ) 
    (+ (distof (cdr (assoc 1 (entget Selección)))) 
       (vlax-ldata-get "NUMDICTIONARY" "INCREMENTO") 
    ) 
   ) 
      ) 
    ) 
  ) 
)
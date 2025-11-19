(defun c:Grid1a()
  (setq osmode0 (getvar "osmode"))
  (setq layer0 (getvar "clayer"))
  (setq style0 (getvar "textstyle"))
  (setvar "cmdecho" 0)
  (setq EsqInfIzq (getpoint "\nIngrese Esquina Inferior Izquierda: "))
  (setq EsqSupDer (getcorner EsqInfIzq "\nIngrese Esquina Superior Derecha: "))
  (setq Ancho (- (car EsqSupDer) (car EsqInfIzq)))
  (setq Alto  (- (cadr EsqSupDer) (cadr EsqInfIzq)))
  (setq Dato1 (strcat "\nAlto x Ancho= " (rtos ancho 2 2) " x " (rtos Alto 2 2)))
  (princ dato1)
  (setq distEste (getint "\nIngrese Distancia en Este: "))
  (setq msg1 (strcat "\nIngrese Distancia en Norte <" (itoa distEste) ">: "))
  (setq distNorte (getint msg1))
  (if ( = distNorte nil)
    (setq distNorte DistEste)
  )
  (initget "I E i e")
  (setq TipoTxt (getkword "\nTexto Interior or Exterior (I / E): ") )
  (setq AlturaTxt (getreal "\nAltura del Texto: "))
  (setq offTxt (/ alturaTXT 2))
  (setq offTxt1 (* alturaTXT 1.6))
; crea la capa Grid_NE y verifica que no este apagada o congelada
  (if (= (tblsearch "layer" "GRID_NE") nil)
    (command "layer" "m" "GRID_NE" "" "")
    (progn
      (setq tblLayer (tblsearch "layer" "GRID_NE"))
      (setq LayerOff (cdr (assoc 62 tbllayer)))
      (setq LayerFrz (cdr (assoc 70 tbllayer)))
      (if (< layerOff 0)
	(command "layer" "on" "GRID_NE" "" "")
      )
      (if (> layerFrz 0)
	(command "layer" "T" "GRID_NE" "" "")
      )
      (setvar "clayer" "GRID_NE")
    )
    
  )
 (if (= (tblsearch "layer" "GRID_NE_TXT") nil)
    (command "layer" "m" "GRID_NE_TXT" "" "")
    (progn
      (setq tblLayer (tblsearch "layer" "GRID_NE_TXT"))
      (setq LayerOff (cdr (assoc 62 tbllayer)))
      (setq LayerFrz (cdr (assoc 70 tbllayer)))
      (if (< layerOff 0)
	(command "layer" "on" "GRID_NE_TXT" "" "")
      )
      (if (> layerFrz 0)
	(command "layer" "T" "GRID_NE_TXT" "" "")
      )
      (setvar "clayer" "GRID_NE_TXT")
    )
    
  )
  
 ; crea el estilo de texto GRID
  (if (= (tblsearch "style" "GRID") nil)
    (command "style" "GRID" "ROMAND" 0 1 0 "N" "N" "N")
    (setvar "TEXTSTYLE" "grid")
  ) 
;Corodenadas de los Puntos a un valor entero ***********************************
  (setq Cx1 (atof (rtos (car EsqInfIzq) 2 0)))
  (setq Cy1 (atof (rtos (car (cdr EsqInfIzq)) 2 0)))
  
  
  (setq Cx3  (atof (rtos (car EsqSupDer) 2 0)))
  (setq Cy3  (atof (rtos (car (cdr EsqSupDer)) 2 0)))
  
  (setq Cx2 Cx3)
  (setq Cy2 Cy1)
  
  (setq Pto1 (list Cx1 Cy1 0))
  (setq Pto2 (list Cx3 Cy1 0))
  (setq Pto3 (list Cx3 Cy3 0))
  (setq Pto4 (list Cx1 Cy3 0))
;**********************************
  (setq D1 (- Cx2 Cx1))  ; distancia en Este
  (setq D2 (- Cy3 Cy1))  ; distancia en Norte
 
;******************************
  (setq Resto1 (rem d1 DistEste))
  (if (= Resto1 0)
    (setq LineasEste (+ (fix (/ d1 DistEste)) 1))
    (setq LineasEste (+ (fix (/ d1 DistEste)) 1))
  )
;*************************  
  (setq Resto2 (rem d2 DistNorte))
  (if (= Resto2 0)
    (setq LineasNorte (+ (/ d2 DistNorte) 1))
    (setq LineasNorte (+ (fix (/ d2 DistNorte)) 1))
  )
;****************************
  (setvar "clayer" "GRID_NE")
  (command "line" pto1 pto4 "")
  (if (= resto1 0)
    (command "array" "l" "" "r" 1 LineasEste DistEste)
    (progn
      (command "array" "l" "" "r" 1 LineasEste DistEste)
      (command "line" Pto2 pto3 "")
    )  
  )  

  (command "line" pto1 pto2 "")
  (if (= resto2 0)
    (command "array" "l" "" "r" (fix LineasNorte) 1 distNorte)
    (progn
      (command "array" "l" "" "r" (fix LineasNorte) 1 distNorte)
      (command "line" Pto3 pto4 "")
    )  
  )  

  ;**** Textos
  (setvar "TEXTSTYLE" "grid")
  (setvar "clayer" "GRID_NE_TXT")
  ; Este Inferior
  (setq coordTxt Cx1)
  (if (= TipoTxt "E")
    (progn
      (setq Cx1Txt Cx1)
      (setq Cy1Txt (- Cy1 offTxt))
      (setq JustTxt "MR")  ; justificacion ml
    )
    (progn
      (setq Cx1Txt (+ cx1 offtxt))
      (setq Cy1Txt (+ cy1 offtxt))
      (setq JustTxt "TL")  ; justificaicon tl
    )
  )	 
  (setq PtoTxt (list Cx1Txt Cy1Txt))
  (setvar "osmode" 0)
  ;-----------------------------
   (sep_Txt CoordTxt) ;
  (setq CoordTxt1 newTXT)
 ;----------------------------- 
  (command "text" JustTxt Ptotxt AlturaTxt 90 (strcat "E " CoordTxt1 ) )

  (repeat ( - LineasEste 1)
    (setq CoordTXT (+ CoordTxt distEste))
    (if (= TipoTxt "E")
     (progn
      (setq Cx1Txt (+ Cx1Txt distEste))
      (setq Cy1txt (- Cy1 offTxt))
      (setq JustTxt "MR")  ; justificacion ml
     ) 
     (progn
      (setq Cx1Txt (+ Cx1Txt distEste))
      (setq Cy1Txt (+ Cy1 offTxt))
      (setq JustTxt "TL")  ; justificaicon tl      
     ) 
    )  
    
    (setq PtoTxt (list Cx1Txt Cy1Txt))
      ;-----------------------------
   (sep_Txt CoordTxt) ;
  (setq CoordTxt1 newTXT)
 ;----------------------------- 

    (command "text" JustTXT Ptotxt AlturaTxt 90 (strcat "E " CoordTxt1 ))
  )
  (if (and (= Resto1 0) (= tipoTxt "I"))
    (progn
      (command "erase" "l" "")
      (setq Cx1Txt (- Cx2 offtxt))
      (setq Cy1Txt (+ Cy2 offtxt))
      (setq PtoTxt (list Cx1Txt Cy1Txt))
        ;-----------------------------
   (sep_Txt CoordTxt) ;
  (setq CoordTxt1 newTXT)
 ;----------------------------- 

      (command "text"  Ptotxt AlturaTxt 90 (strcat "E " CoordTxt1))
    )  
  )
  ; Este Superior
  (setq coordTxt Cx1)
  (if (= TipoTXT "E")
    (progn
      (setq Cx2Txt Cx1)
      (setq Cy2Txt (+ Cy3 offTxt))
      (setq JustTxt "ML")  ; justificacion ml
    )
    (progn
      (setq Cx2Txt (+ Cx1 offTxt))
      (setq Cy2Txt (- Cy3 offTxt))
      (setq JustTxt "TR")  ; justificacion TR
    )
  ) ; fin if
  (setq PtoTxt2 (list Cx2Txt Cy2Txt))
  ; separador de decimales
  ;-----------------------------
   (sep_Txt CoordTxt) ;
  (setq CoordTxt1 newTXT)
  ;---------------
 ; (command "text" JustTXT Ptotxt2 AlturaTxt 90 (strcat "E" (rtos CoordTxt 2 0))) 
  (command "text" JustTXT Ptotxt2 AlturaTxt 90 (strcat "E "  CoordTxt1 )) 

  (repeat ( - LineasEste 1)
    (setq CoordTXT (+ CoordTxt distEste))
    (if (= tipoTxt "E")
      (progn
        (setq Cx2Txt (+ Cx2Txt distEste))
	(setq Cy2Txt (+ Cy3 offTxt))
	(setq JustTxt "ML")  ; justificacion ml
      )
      (progn
	(setq Cx2Txt (+ Cx2Txt DistEste))
	(setq Cy2Txt (- Cy3 offTxt))
	(setq JustTxt "TR")  ; justificacion tr
      )
    ) ; fin if
    
    (setq PtoTxt2 (list Cx2Txt Cy2Txt))
      ;-----------------------------
   (sep_Txt CoordTxt) ;
  (setq CoordTxt1 newTXT)
 ;----------------------------- 

    (command "text" JustTxt Ptotxt2 AlturaTxt 90 (strcat "E " CoordTxt1))
  )
  (if (and (= Resto1 0) (= tipoTxt "I"))
    (progn
      (command "erase" "l" "")
      (setq Cx2Txt (- Cx3 offtxt))
      (setq Cy2Txt (- Cy3 offtxt))
      (setq PtoTxt2 (list Cx2Txt Cy2Txt))
      (command "text"  "R" Ptotxt2 AlturaTxt 90 (strcat "E " CoordTxt1))
    )  
  )
  
  ;Norte Izquierda
  (setq coordTxt Cy1)
  (if (= tipoTxt "E")
    (progn
      (setq Cx4Txt (- Cx1 offTxt))
      (setq Cy4Txt Cy1)
      (setq JustTXT "MR")
      (setq PtoTxt4 (list Cx4Txt Cy4Txt))
        ;-----------------------------
   (sep_Txt CoordTxt) ;
  (setq CoordTxt1 newTXT)
 ;----------------------------- 

      (command "text" JustTXT PtoTxt4 AlturaTxt 0 (strcat "N " CoordTxt1))
    )
    (progn
      (setq Cx4Txt (+ Cx1 offTxt1)) ; h * 1.5
      (setq Cy4Txt (+ Cy1 offTxt))
      (setq PtoTxt4 (list Cx4Txt Cy4Txt))
      ;-----------------------------
      (sep_Txt CoordTxt) ;
      (setq CoordTxt1 newTXT)
      ;----------------------------- 
      (command "text" PtoTxt4 AlturaTxt 0 (strcat "N " CoordTxt1 ))
    )
  )  
  
  (repeat (- (fix lineasNorte) 1)
    (setq CoordTXT (+ CoordTxt distNorte))
    (if (= TipoTxt "E")
      (progn
	(setq Cx4Txt (- Cx1 offTxt))
        (setq Cy4Txt (+ Cy4Txt DistNorte))
	(setq JustTXT "MR")
	(setq PtoTxt4 (list Cx4Txt Cy4Txt))
	  ;-----------------------------
        (sep_Txt CoordTxt) ;
        (setq CoordTxt1 newTXT)
 ;----------------------------- 
        (command "text" JustTxt PtoTxt4 AlturaTxt 0 (strcat "N " CoordTxt1))
      )
      (progn
	(setq Cx4Txt (+ Cx1 offTxt))
        (setq Cy4Txt (+ Cy4Txt DistNorte))
	(setq PtoTxt4 (list Cx4Txt Cy4Txt))
	  ;-----------------------------
        (sep_Txt CoordTxt) ;
        (setq CoordTxt1 newTXT)
  ;----------------------------- 
        (command "text" PtoTxt4 AlturaTxt 0 (strcat "N " CoordTxt1))
      )
    ) ;fin if
  )
  (if (and (= Resto2 0) (= tipoTxt "I"))
    (progn
      (command "erase" "l" "")
      (setq Cx4Txt (+ Cx1 offTxt1))
      (setq Cy4Txt (- Cy3 offtxt))
      (setq PtoTxt4 (list Cx4Txt Cy4Txt))
        ;-----------------------------
      (sep_Txt CoordTxt) ;
      (setq CoordTxt1 newTXT)
 ;----------------------------- 
      (command "text" "TL" PtoTxt4 AlturaTxt 0 (strcat "N " CoordTxt1))
    )  
  )  
  ;Norte Derecha
  (setq coordTxt Cy1)
  (if (= TipoTxt "E")
    (progn
      (setq Cx3Txt (+ Cx2 OffTxt))
      (setq Cy3Txt Cy1)
      (setq JustTxt "ML")      
    )
    (progn
      (if (= Resto1 0)
	(setq Cx3Txt (- Cx2 offTxt1)) ; si resto =0
	(setq Cx3Txt (- Cx2 offTxt))  ; si resto <> 0
      )	
      (setq Cy3Txt (+ Cy2 offTxt))
      (setq JustTxt "R")      
    )
  )
  (setq PtoTxt3 (list Cx3Txt Cy3Txt))
  ;-----------------------------
   (sep_Txt CoordTxt) ;
   (setq CoordTxt1 newTXT)
 ;----------------------------- 
  (command "text" JustTXT PtoTxt3 AlturaTxt 0 (strcat "N " CoordTxt1))
  
  (repeat (- (fix lineasNorte) 1)
    (setq CoordTXT (+ CoordTxt distNorte))
    (if (= TipoTxt "E")
      (progn
	(setq Cx3Txt (+ Cx2 offTxt))
	(setq Cy3Txt (+ Cy3Txt DistNorte))
	(setq JustTXT "ML")
      )
      (progn
	(setq Cx3Txt (- Cx2 offTxt))
	(setq Cy3Txt (+ Cy3Txt DistNorte))
	(setq JustTXT "R")
      )
    )  
    (setq PtoTxt3 (list Cx3Txt Cy3Txt))
      ;-----------------------------
   (sep_Txt CoordTxt) ;
  (setq CoordTxt1 newTXT)
 ;----------------------------- 

    (command "text" JustTxt PtoTxt3 AlturaTxt 0 (strcat "N " CoordTxt1))
  )
    (if (and (= Resto2 0) (= tipoTxt "I"))
    (progn
      (command "erase" "l" "")
      (if (= resto1 0) 
	(setq Cx3Txt (- Cx3 offTxt1 ))
	(setq Cx3Txt (- Cx3 offTxt ))
      )	
      (setq Cy3Txt (- Cy3 offtxt))
      (setq PtoTxt3 (list Cx3Txt Cy3Txt))
        ;-----------------------------
   (sep_Txt CoordTxt) ;
  (setq CoordTxt1 newTXT)
 ;----------------------------- 

      (command "text" "TR" PtoTxt3 AlturaTxt 0 (strcat "N " CoordTxt1 ))
    )  
  )  

; restaurar valores iniciales
  (setvar "osmode" osmode0)
  (setvar "clayer" layer0)
  (setvar "textstyle" style0)
  (setvar "cmdecho" 1)
  (princ)
    
)
;
;Funcion que convierte las coordenadas en Texto separado por comas
(defun sep_Txt (dReal)
  (setq strReal (rtos dreal 2 0)) ; convierte real a cadena sin decimales
     (setq nroCar  (strlen strReal)) ; calcula el numero de caracteres
  (cond
    ((= nroCar 4) ; 1000
      (setq mil1 (substr strReal 1 1))
      (setq cen1 (substr strReal 2))
      (setq newTXT (strcat mil1 "," cen1))
    )
    ((= nroCar 5) ; 10,000
      (setq mil1 (substr strReal 1 2))
      (setq cen1 (substr strReal 3))
      (setq newTXT (strcat mil1 "," cen1))
    )
    ((= nroCar 6) ; 100,000
      (setq mil1 (substr strReal 1 3))
      (setq cen1 (substr strReal 4))
      (setq newTXT (strcat mil1 "," cen1))
    )
    ((= nroCar 7) ; 1'000,000
      (setq millon1 (substr strReal 1 1))
      (setq mil1    (substr strReal 2 3))
      (setq cen1 (substr strReal 5)) 
      (setq newTXT (strcat millon1 "'" mil1 "," cen1))
    )
    ((= nroCar 8) ; 1'000,000
      (setq millon1 (substr strReal 1 2))
      (setq mil1    (substr strReal 3 3))
      (setq cen1 (substr strReal 6)) 
      (setq newTXT (strcat millon1 "'" mil1 "," cen1))
    )
    ((setq newtxt strReal))
  )
)

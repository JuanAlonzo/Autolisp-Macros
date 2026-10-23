(vl-load-com)

(defun c:EXTXT (/ *error* ss ssCalles callesList items sortedItems fullStr clipStr i 
                ent obj pt txt distVal stVal line
               ) 

  ;; Control de errores
  (defun *error* (msg) 
    (if (not (wcmatch (strcase (vl-to-string msg)) "*BREAK*,*CANCEL*,*QUIT*")) 
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; Distancia 2D
  (defun dist2d (p1 p2) 
    (distance (list (car p1) (cadr p1)) (list (car p2) (cadr p2)))
  )

  ;; Copiar texto formateado al portapapeles de Windows vía COM
  (defun PutClipboard (str / html) 
    (vl-catch-all-apply 
      '(lambda () 
         (if (setq html (vlax-create-object "htmlfile")) 
           (progn 
             (vlax-invoke 
               (vlax-get (vlax-get html 'parentwindow) 'clipboarddata)
               'setdata
               "Text"
               str
             )
             (vlax-release-object html)
           )
         )
       )
    )
  )

  ;; Limpiar y extraer solo el valor numérico de la distancia (ej. "102.00m" -> "102.00")
  (defun CleanDist (str / i c res hasDot) 
    (setq str (vl-string-trim " \t\r\n" str)
          res ""
    )
    (setq i 1)
    (while (<= i (strlen str)) 
      (setq c (substr str i 1))
      (if 
        (or (and (>= (ascii c) 48) (<= (ascii c) 57))  ; Dígitos 0-9
            (and (= c ".") (not hasDot))
        ) ; Primer punto decimal
        (progn 
          (if (= c ".") (setq hasDot T))
          (setq res (strcat res c))
        )
      )
      (setq i (1+ i))
    )
    (if (= res "") str res)
  )

  ;; Limpieza de texto de calle (elimina códigos MText y espacios redundantes)
  (defun CleanStreet (str / pos) 
    (setq str (vl-string-trim " \t\r\n" str))
    (while (setq pos (vl-string-search "\\P" str)) 
      (setq str (strcat (substr str 1 pos) " " (substr str (+ pos 3))))
    )
    (while (setq pos (vl-string-search "\\p" str)) 
      (setq str (strcat (substr str 1 pos) " " (substr str (+ pos 3))))
    )
    ;; Unificar espacios dobles
    (while (vl-string-search "  " str) 
      (setq str (vl-string-subst " " "  " str))
    )
    (vl-string-trim " " str)
  )

  ;; Eliminar solo la primera coincidencia de la lista
  (defun RemoveFirst (item lst / res found) 
    (foreach x lst 
      (if (and (not found) (equal x item)) 
        (setq found T)
        (setq res (cons x res))
      )
    )
    (reverse res)
  )

  ;; Obtener calle/avenida más cercana
  (defun GetNearestStreet (pt calles / bestDist bestName d) 
    (if (null calles) 
      "SIN_CALLE"
      (progn 
        (setq bestDist (dist2d pt (caar calles))
              bestName (cadar calles)
        )
        (foreach c (cdr calles) 
          (setq d (dist2d pt (car c)))
          (if (< d bestDist) 
            (setq bestDist d
                  bestName (cadr c)
            )
          )
        )
        bestName
      )
    )
  )

  ;; Ordenamiento continuo por vecino más cercano
  (defun SortNearestNeighbor (lst / start remaining sorted curr best bestD d) 
    (if lst 
      (progn 
        ;; Iniciar por el texto más hacia la izquierda
        (setq start (car lst))
        (foreach item (cdr lst) 
          (if (< (car (car item)) (car (car start))) 
            (setq start item)
          )
        )
        (setq sorted    (list start)
              remaining (RemoveFirst start lst)
        )

        ;; Encadenar al siguiente más próximo
        (while remaining 
          (setq curr  (car sorted)
                best  (car remaining)
                bestD (dist2d (car curr) (car best))
          )
          (foreach item (cdr remaining) 
            (setq d (dist2d (car curr) (car item)))
            (if (< d bestD) 
              (setq bestD d
                    best  item
              )
            )
          )
          (setq sorted    (cons best sorted)
                remaining (RemoveFirst best remaining)
          )
        )
        (reverse sorted)
      )
    )
  )

  ;; 1. Leer capas de calles/avenidas
  (setq ssCalles (ssget "_X" 
                        '((0 . "TEXT,MTEXT") (8 . "CAT_CALLES,CAT_AVENIDAS"))
                 )
  )
  (setq callesList nil)
  (if ssCalles 
    (repeat (setq i (sslength ssCalles)) 
      (setq ent (ssname ssCalles (setq i (1- i))))
      (setq obj (vlax-ename->vla-object ent))
      (setq callesList (cons 
                         (list (vlax-get obj 'InsertionPoint) 
                               (CleanStreet (vla-get-textstring obj))
                         )
                         callesList
                       )
      )
    )
  )

  ;; 2. Selección de los textos de distancia
  (setq ss (ssget "_I" '((0 . "TEXT,MTEXT"))))
  (if (not ss) 
    (progn 
      (princ "\nSelecciona las distancias (o Enter si ya usaste SELECTSIMILAR): ")
      (setq ss (ssget '((0 . "TEXT,MTEXT"))))
      (if (not ss) 
        (setq ss (ssget "_P" '((0 . "TEXT,MTEXT"))))
      )
    )
  )

  ;; 3. Procesar y formatear para Excel
  (if (and ss (> (sslength ss) 0)) 
    (progn 
      (setq items nil)
      (repeat (setq i (sslength ss)) 
        (setq ent (ssname ss (setq i (1- i))))
        (setq obj (vlax-ename->vla-object ent))
        (setq pt (vlax-get obj 'InsertionPoint))
        (setq txt (vla-get-textstring obj))
        (setq distVal (CleanDist txt))
        (setq stVal (CleanStreet (GetNearestStreet pt callesList)))
        (setq items (cons (list pt distVal stVal) items))
      )

      ;; Ordenar por proximidad continua
      (setq sortedItems (SortNearestNeighbor items))

      ;; Generar formato tabulado para Excel: [CALLES]\t[DISTANCIA]\r\n
      (setq fullStr ""
            clipStr ""
      )
      (foreach item sortedItems 
        (setq distVal (cadr item)
              stVal   (caddr item)
        )
        (setq clipStr (strcat clipStr stVal "\t" distVal "\r\n"))
        (setq fullStr (strcat fullStr stVal "\t" distVal "\n"))
      )

      ;; Cargar al portapapeles
      (PutClipboard clipStr)

      ;; Vista previa en consola
      (princ "\n--- LISTO PARA PEGAR EN EXCEL ---")
      (princ (strcat "\n" fullStr))
      (princ "---------------------------------\n")

      ;; Mensaje de confirmación
      (alert 
        (strcat "DATOS COPIADOS AL PORTAPAPELES\n\n" "Paso a seguir:\n" 
                "1. Abre tu Excel.\n" 
                "2. Haz clic en la primera celda vacía de la columna 'CALLES'.\n" 
                "3. Presiona Ctrl + V.\n\n" 
                "La calle y la distancia se colocarán en sus respectivas columnas automáticamente."
        )
      )
    )
    (princ "\nNo se seleccionaron textos.")
  )
  (princ)
)
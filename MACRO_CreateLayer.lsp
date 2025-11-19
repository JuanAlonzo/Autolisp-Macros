; -- Función: CreateLayersByDrawing (CAPA) --
;
; Crea 3 capas para numeración de postes con el nombre del plano
;
; Funcionalidad:
;   - Solicita un texto (nombre del plano, ej: "LMLC012-S")
;   - Crea 3 capas con ese nombre al final:
;     * NUMERACION DE POSTES EXISTENTES - [nombre]
;     * NUMERACION DE POSTES PROYECTADOS DE APOYO - [nombre]
;     * NUMERACION DE POSTES PROYECTADOS - [nombre]
;   - Cada capa tiene un color diferente
;

(vl-load-com)

(defun c:CAPA (/ drawing_name layer1 layer2 layer3) 
  (princ "\n--- Crear Capas de Numeración por Plano ---")

  ;; Solicitar el nombre del plano
  (setq drawing_name (getstring t 
                                "\nIngrese el nombre del plano (ej: LMLC012-S_R): "
                     )
  )

  (if (and drawing_name (/= drawing_name "")) 
    (progn 
      ;; Definir los nombres de las capas
      (setq layer1 (strcat "NUMERACION DE POSTES EXISTENTES_" drawing_name))
      (setq layer2 (strcat "NUMERACION DE POSTES PROYECTADOS DE APOYO_" 
                           drawing_name
                   )
      )
      (setq layer3 (strcat "NUMERACION DE POSTES PROYECTADOS_" drawing_name))

      ;; Crear capa 1 - Color Amarillo (2)
      (Create_Layer_WithColor layer1 2)
      (princ (strcat "\nCapa creada: " layer1 " (Color: Amarillo)"))

      ;; Crear capa 2 - Color Magenta (6)
      (Create_Layer_WithColor layer2 6)
      (princ (strcat "\nCapa creada: " layer2 " (Color: Magenta)"))

      ;; Crear capa 3 - Color Cyan (4)
      (Create_Layer_WithColor layer3 4)
      (princ (strcat "\nCapa creada: " layer3 " (Color: Cyan)"))

      (princ "\n\n*** 3 capas creadas exitosamente ***")
      (princ (strcat "\nPlano: " drawing_name))
    )
    (princ "\nOperación cancelada: No se ingresó ningún nombre.")
  )

  (princ)
)

;; Función auxiliar para crear capas con color específico
(defun Create_Layer_WithColor (layer_name layer_color /) 
  (if (= (tblsearch "Layer" layer_name) nil) 
    ;; Si la capa no existe, crearla
    (command "._Layer" "n" layer_name "c" layer_color layer_name "")
    ;; Si ya existe, actualizarla
    (command "._Layer" "t" layer_name "on" layer_name "u" layer_name "c" layer_color 
             layer_name ""
    )
  )
  (princ)
)

(princ "\nComando CAPA (CreateLayersByDrawing) cargado.")
(princ "\nEscribe \"CAPA\" para crear las capas de numeración.")
(princ)

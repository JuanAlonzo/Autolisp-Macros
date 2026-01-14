# Documentación de Macros AutoLISP

Esta documentación describe todas las macros AutoLISP disponibles en la raíz del proyecto.
Algunas de las macros contienen mejoras implementadas, usando de fuente la macro original.

---

## Índice

1. [MACRO_AddDemand](#macro_adddemand)
2. [MACRO_AddText (prefix-sufix)](#macro_addtext-prefix-sufix)
3. [MACRO_AddTextToLines](#macro_addtexttolines)
4. [MACRO_Align_Blocks](#macro_align_blocks)
5. [MACRO_BlockRename](#macro_blockrename)
6. [MACRO_BreakPoint](#macro_breakpoint)
7. [MACRO_CountText](#macro_counttext)
8. [MACRO_CrearCapaNumeracion](#macro_crearcapanumeracion)
9. [MACRO_DistanceAcumulative](#macro_distanceacumulative)
10. [MACRO_Export_xls](#macro_export_xls)
11. [MACRO_InsideSelector](#macro_insideselector)
12. [MACRO_InsideSelectorCircle](#macro_insideselectorcircle)
13. [MACRO_InsideSelector_GLOBAL](#macro_insideselector_global)
14. [MACRO_Midline](#macro_midline)
15. [MACRO_MNSJ (IMPROVE)](#macro_mnsj-improve)
16. [MACRO_MultiLine](#macro_multiline)
17. [MACRO_Multiline_Count](#macro_multiline_count)
18. [MACRO_NumberIncrement](#macro_numberincrement)
19. [MACRO_PolErase](#macro_polerase)
20. [MACRO_Sum](#macro_sum)
21. [MACRO_SumaText](#macro_sumatext)
22. [MACRO_SumaText (2.0)](#macro_sumatext-20)
23. [MACRO_SwapText (IMPROVE)](#macro_swaptext-improve)

---

## MACRO_AddDemand

**Comando:** `AddDemand`

**Descripción:**  
Agrega automáticamente un texto "1R" debajo de cada texto seleccionado. Útil para agregar demandas en planos de instalaciones eléctricas o telecomunicaciones.

**Características:**

- Crea automáticamente una capa llamada "MagentaLayer" si no existe
- Coloca el nuevo texto en color magenta
- Mantiene el alineamiento con el texto original

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: AddDemand
3. Selecciona los textos donde quieres agregar "1R"
4. Presiona Enter
5. Resultado: Aparecerá "1R" en color magenta debajo de cada texto
```

**Caso práctico:**  
En un plano de postes eléctricos, tienes numeración de lotes (Lote-1, Lote-2, Lote-3...).
Al usar esta macro, automáticamente agregarás "1R" debajo de cada poste para indicar una residencia.

---

## MACRO_AddText (prefix-sufix)

**Comando:** `ADDTXT`

**Descripción:**  
Añade un prefijo o sufijo a textos existentes en el dibujo.
Muy útil para modificar múltiples textos de manera masiva.

**Características:**

- Opción de agregar prefijo o sufijo
- Guarda el último texto utilizado para reutilización
- Funciona con TEXT y MTEXT

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: ADDTXT
3. Elige "Prefix" o "Suffix" (Enter = Suffix por defecto)
4. Ingresa el texto a agregar (ej: "POS-")
5. Selecciona los textos a modificar
6. Presiona Enter
```

**Caso práctico:**  
Tienes números de postes (1, 2, 3, 4) y necesitas agregar el prefijo "POS-" a todos. Resultado: POS-1, POS-2, POS-3, POS-4.

---

## MACRO_AddTextToLines

**Comando:** `ADDTXTLINE`

**Descripción:**  
Agrega texto personalizado en el punto medio de cada segmento de líneas o polilíneas seleccionadas. El texto se orienta según el ángulo del segmento.

**Características:**

- Solicita el texto a agregar
- Permite elegir el color del texto
- Calcula automáticamente el punto medio y ángulo de cada segmento
- Crea capas automáticas según el color elegido

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: ADDTXTLINE
3. Ingresa el texto (ej: "FIBRA 24H")
4. Elige un color (1=Rojo, 2=Amarillo, 3=Verde, etc.)
5. Define el tamaño del texto (ej: 1.5)
6. Selecciona las líneas o polilíneas
7. Presiona Enter
```

**Caso práctico:**  
En un plano de diseño, necesitas etiquetar cada tramo de cable con "FIBRA 24H". La macro coloca automáticamente el texto en el centro de cada segmento con la orientación correcta.

---

## MACRO_Align_Blocks

**Comando:** `AB`

**Descripción:**  
Alinea bloques a lo largo de una polilínea guía.
Los bloques se posicionan sobre la polilínea y se rotan perpendiculares a ella (90 grados).
Incluye opción para rotarlos 180 grados adicionales a peticion del usuario.
Adicionalmente cuenta con llamada recursiva por si se desea usar de manera reiterada.

**Características:**

- Funciona con líneas, arcos, splines y polilíneas
- Rota automáticamente los bloques perpendiculares a la guía
- Opción interactiva para rotar 180° después de alinear
- Proyecta los bloques al punto más cercano de la curva

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: AB
3. Selecciona los bloques a alinear
4. Presiona Enter
5. Selecciona la polilínea guía
6. Responde Si/No a la rotación de 180 grados
```

**Caso práctico:**  
Tienes bloques de FATS que no estan correctamente alineados y una polilínea que representa el trazado de una red.
La macro alinea todos los bloques sobre el trazado y los orienta correctamente. Adicionalmente solicita si es necesario rotar el bloque de manera contraria, por defecto lo deja como esta.

---

## MACRO_BlockRename

**Comando:** `renombrar`

**Descripción:**  
Renombra bloques en el dibujo de manera interactiva. Muestra el nombre actual del bloque seleccionado y permite asignar un nuevo nombre.

**Características:**

- Interfaz simple e interactiva
- Muestra el nombre actual del bloque
- Valida la selección de bloques

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: renombrar
3. Selecciona el bloque a renombrar
4. Ingresa el nuevo nombre del bloque
5. Presiona Enter
```

**Caso práctico:**  
Tienes un bloque llamado "BLOQUE1" que necesitas renombrar a "POSTE_ELECTRICO". La macro te permite hacerlo de forma rápida e intuitiva.

---

## MACRO_BreakPoint

**Comando:** `BRP`

**Descripción:**  
Rompe líneas, polilíneas y arcos en un punto específico. Selecciona automáticamente objetos cercanos al punto indicado.

**Características:**

- Funciona con líneas, polilíneas y arcos
- Selección automática de objetos cercanos
- Manejo de errores robusto
- Preserva las propiedades de los objetos

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: BRP
3. Haz clic en el punto donde quieres romper la línea
4. La línea se dividirá automáticamente en ese punto
```

**Caso práctico:**
Tienes una linea de red construida o una ya existente a la cual le falta la division de sus cruces. La macro detecta la línea cercana y la rompe en el punto exacto.

---

## MACRO_CountText

**Comando:** `CT`

**Descripción:**  
Cuenta y suma valores numéricos en textos seleccionados. Reconoce sufijos especiales (R, C, D y T) para conteos categorizados. Contiene una condicion adicional que filtra D entre mayores y menores a "9D".
Inserta un resumen detallado solo de los sufijos especiales en el dibujo.

**Características:**

- Reconoce números con sufijos R (Residencial), C (Comercial), D (Departamentos) y T (Otros)
- Filtra capas visibles y no congeladas
- Acepta punto o coma como separador decimal
- Contabiliza la cantidad de D (Departamentos) y filtra <=9 y >9
- Genera resumen con totales y conteos por categoría
- El resumen solo muestra los sufijos contabilizados
- Valida textos numéricos

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: CT
3. Selecciona los textos con valores (ej: "3R", "2C", "5D", "1.5")
4. Presiona Enter
5. Indica el punto donde insertar el resumen
```

**Caso práctico:**  
En un plano tienes demandas marcadas: 2R, 3R, 1C, 4C, 2D. Incluso edificios con E/2P/3D.
La macro suma: Total=12, Residencial=5, Comercial=5, Otros=2. A diferencia del SUMATEXT esta macro filtra los valores y omite datos innecesarios.

---

## MACRO_CrearCapaNumeracion

**Comando:** `CAPA`

**Descripción:**  
Crea automáticamente 3 capas para numeración de postes con el nombre del plano al final. Cada capa tiene un color diferente para facilitar la identificación.

**Características:**

- Crea 3 capas específicas para postes
- Asigna colores automáticamente
- Incluye el nombre del plano en cada capa
- Valida si las capas ya existen

**Capas creadas:**

1. NUMERACION DE POSTES EXISTENTES\_[nombre] - Color Amarillo
2. NUMERACION DE POSTES PROYECTADOS DE APOYO\_[nombre] - Color Magenta
3. NUMERACION DE POSTES PROYECTADOS\_[nombre] - Color Cyan

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: CAPA
3. Ingresa el nombre del plano (ej: "LMLC012-S_R")
4. Presiona Enter
5. Las 3 capas se crean automáticamente
```

**Caso práctico:**  
Estás trabajando en el plano "LMLC012-S_R" y necesitas organizar la numeración de postes en capas específicas. La macro crea las 3 capas con nombres estandarizados para hacer la numeracion.

---

## MACRO_DistanceAcumulative

**Comando:** `DA`

**Descripción:**  
Calcula la distancia acumulada entre puntos seleccionados secuencialmente. Útil para medir trayectorias o recorridos.
Cuenta con llamada recursiva por si se requiere utilizar multiples veces.

**Características:**

- Muestra distancia acumulada después de cada punto
- Distancia en unidades del dibujo
- Interfaz interactiva punto a punto
- Muestra total al finalizar

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: DA
3. Selecciona el primer punto
4. Selecciona el segundo punto (muestra distancia parcial)
5. Continúa seleccionando puntos
6. Presiona Enter para finalizar y ver el total
7. Selecciona "Si" para agregar el total en el dibujo
```

**Caso práctico:**  
Necesitas calcular la longitud total de un recorrido de cable continuo.
Vas marcando cada cambio y la macro acumula las distancias. Al final, te mostrara en total en consola, pero tienes opcion de que se muestre en el dibujo.

---

## MACRO_Export_xls

**Comando:** `xls`

**Descripción:**  
Exporta textos seleccionados a un archivo Excel (.xls). Organiza los datos por coordenadas para facilitar su análisis posterior.

**Características:**

- Exporta únicamente objetos TEXT
- Guarda coordenadas X, Y y contenido del texto
- Permite especificar nombre y ubicación del archivo
- Genera archivo compatible con Excel

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: xls
3. Selecciona los textos a exportar
4. Presiona Enter
5. Especifica la ubicación y nombre del archivo .xls
6. El archivo se genera automáticamente
```

**Caso práctico:**  
Tienes numeración de postes en un plano y necesitas generar un listado en Excel con las coordenadas de cada poste para análisis o importación a otro sistema.

---

## MACRO_InsideSelector

**Comando:** `ICS`

**Descripción:**  
Selecciona todos los objetos dentro de una polilínea cerrada. Herramienta de selección avanzada.

**Características:**

- Funciona con polilíneas cerradas
- Selección automática de objetos interiores
- Zoom automático al área seleccionada
- Resalta los objetos seleccionados

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: ICS
3. Selecciona la polilínea cerrada que define el contorno
4. Todos los objetos dentro se seleccionan automáticamente
```

**Caso práctico:**  
Tienes una polilínea que delimita una manzana y necesitas seleccionar todos los postes dentro de ella. La macro lo hace automáticamente.

---

## MACRO_InsideSelectorCircle

**Comando:** `SWC`

**Descripción:**  
Selecciona objetos dentro de curvas cerradas (círculos, elipses). Versión especializada para formas circulares.

**Características:**

- Funciona con círculos y elipses
- Algoritmo optimizado para curvas
- Permite múltiples selecciones
- Soporta curvas complejas

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: SWC
3. Selecciona el círculo o curva cerrada
4. Presiona Enter
5. Todos los objetos dentro se seleccionan
```

**Caso práctico:**  
Tienes un círculo que representa un área de cobertura y necesitas seleccionar todos los elementos (postes, textos, líneas) dentro de esa área.

---

## MACRO_InsideSelector_GLOBAL

**Comando:** `WPS`

**Descripción:**  
Selecciona objetos dentro de círculos o polilíneas. Versión global que puede usarse dentro de otros comandos de selección.

**Características:**

- Funciona como comando independiente o dentro de otros comandos
- Compatible con círculos y polilíneas
- Se puede usar en prompts de selección
- Genera puntos perimetrales para la selección

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: WPS
3. Selecciona el círculo o polilínea límite
4. Los objetos interiores quedan seleccionados

Uso avanzado:
1. Ejecuta cualquier comando que requiera selección (ej: COPY)
2. En el prompt de selección, escribe: WPS
3. Selecciona el contorno
```

**Caso práctico:**  
Durante un comando MOVE, en lugar de seleccionar objetos uno por uno, usas WPS para seleccionar todos los que están dentro de un círculo específico.

---

## MACRO_Midline

**Comando:** `MIDLINE`

**Descripción:**  
Mide la longitud de cada segmento de líneas y polilíneas seleccionadas y coloca el valor en el punto medio de cada segmento. El texto se orienta según el ángulo del segmento.

**Características:**

- Usa "CAT_DIST POSTE" si existe (o crea la capa "dist" en verde)
- Usa el estilo de texto "Style-CHAR_FAST_FONT" si está disponible
- Orienta el texto según el segmento
- Muestra la distancia total al final
- Altura de texto: 1.5 unidades

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: MIDLINE
3. Selecciona las líneas o polilíneas a medir
4. Presiona Enter
5. Cada segmento mostrará su longitud en el centro
```

**Caso práctico:**  
Tienes una linea de red construida o existente sin acotar o medir. La macro coloca la distancia de cada tramo en su punto medio, facilitando el cálculo de distancias.
Con la mejora implementada, ahora se aproxima al entero mas cercano, asi mismo se cambia de capa y tipo de texto a uno especifico ahorrando tiempo de ejecucion.

---

## MACRO_MNSJ (IMPROVE)

**Comando:** `mnsj`

**Descripción:**  
Copia un objeto fuente a múltiples objetos destino, alineando y ajustando propiedades. Opcionalmente puede eliminar los objetos destino después de copiar.

**Características:**

- Trabaja con líneas, polilíneas, círculos, arcos, elipses, splines, texto, mtext y bloques
- Alinea objetos por su centro de bounding box
- Copia propiedades del objeto fuente
- Opción de intercambio bidireccional
- Manejo robusto de errores

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: mnsj
3. Selecciona el objeto fuente (el que quieres copiar)
4. Selecciona los objetos destino (donde quieres copiar)
5. Elige si deseas eliminar los objetos destino
```

**Caso práctico:**  
Tienes un bloque de poste actualizado y necesitas reemplazar 50 bloques antiguos. La macro copia el nuevo bloque en la posición de cada bloque antiguo y elimina los viejos.

---

## MACRO_MultiLine

**Comando:** `MLSum`

**Descripción:**  
Suma valores de textos seleccionados y dibuja líneas desde cada texto hacia un punto central. El resultado se muestra con color según el rango (verde si está entre 16-18, rojo en otros casos).

**Características:**

- Dibuja líneas conectando textos al punto central
- Código de colores (modificable): Verde (16-18), Rojo (otros)
- Coloca el resultado 2 unidades a la derecha del punto central
- Altura del texto: 1.5 unidades
- Muestra la suma total en la línea de comandos

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: MLSum
3. Selecciona los textos con valores numéricos
4. Presiona Enter
5. Indica el punto central
6. Aparecerán líneas y el texto con la suma total
```

**Caso práctico:**  
Estás calculando demandas de predios en un plano. Seleccionas varios textos con valores (2R, 3R, 5R, 4C, 2C) que suman 16.
El resultado aparece en verde porque está en el rango 16-18 que cumple con lo requerido.

---

## MACRO_Multiline_Count

**Comando:** `MCOUNT`

**Descripción:**  
Versión avanzada de suma de textos con líneas. Filtra capas visibles, cuenta sufijos especiales (R, C, D, T) y muestra múltiples resultados dependiendo de lo encontrado (total, R, C, D, T) en el punto central.

**Características:**

- Filtra capas no visibles o congeladas
- Busca capa `CAT_ALIMENTA` especifica, en caso no exista; la crea
- Reconoce sufijos: R (Residencial), C (Comercial), D (Departamentos), T (Otros)
- Dibuja líneas conectando textos al centro
- Muestra 4 textos: Total, Suma R, Suma C, Suma D, Suma T
- Filtra D: Entre <=9 y >9 (Este ultimo contabilizando como 1)
- Altura de texto: 1.5 unidades
- Reporta textos procesados y omitidos

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: MCOUNT
3. Selecciona textos con valores (ej: "3R", "2C", "5D", "1")
4. Presiona Enter
5. Indica el punto central
6. Aparecen líneas y 4 textos con los totales
```

**Caso práctico:**  
En un plano de demandas tienes: 2R, 3R, 1C, 4C, 2D. La macro dibuja líneas al centro y muestra:

- Total: 12
- R: 5
- C: 5
- D: 2

---

## MACRO_NumberIncrement

**Comando:** `numero`

**Descripción:**  
Inserta números incrementales en puntos especificados por el usuario. El número inicial y el factor de incremento son personalizables. Adicional, el tamaño del texto y sus capas tambien son personalizables.

**Características:**

- Número inicial definido por usuario
- Tamaño y capas personalizables
- Factor de incremento personalizable
- Crea capa "Borrar" automáticamente en color cyan
- Usa fuente Verdana
- Altura de texto: 1 unidades
- Inserción interactiva punto por punto

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: numero
3. Ingresa el número inicial (ej: 10)
4. Ingresa el factor de incremento (ej: 5)
5. Haz clic en cada punto donde quieres insertar números
6. Resultado: 10, 15, 20, 25... (incrementando de 5 en 5)
7. Presiona Enter para finalizar
```

**Caso práctico:**  
Necesitas numerar postes empezando desde el 100 con incrementos de 10. Resultado: P-100, P-110, P-120, P-130...

---

## MACRO_PolErase

**Comando:** `PolErase`

**Descripción:**  
Borra objetos usando una polilínea como límite de selección. Ofrece tres modos de selección: Ventana (Window), Captura (Crossing) o Recorte (Fence).

**Características:**

- Tres modos de selección configurable
- Usa polilíneas como contorno
- Manejo de UCS y variables del sistema
- Soporte de undo
- Preserva configuración del usuario

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: PolErase
3. Dibuja o selecciona una polilínea que defina el área
4. Elige el modo (Window/Crossing/Fence)
5. Los objetos se borran según el criterio elegido
```

**Caso práctico:**  
Tienes una zona con muchos elementos que ya no necesitas. Dibujas una polilínea alrededor del área y usas PolErase en modo Crossing para eliminar todo lo que toca o está dentro.

---

## MACRO_Sum

**Comando:** `SUM`

**Descripción:**  
Versión simple de suma de textos con líneas al punto central. Suma valores numéricos y dibuja líneas conectoras.

**Características:**

- Suma textos numéricos (TEXT y MTEXT)
- Dibuja líneas desde cada texto al punto central
- Muestra resultado en el punto central
- Interfaz minimalista

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: SUM
3. Selecciona textos con valores numéricos
4. Presiona Enter
5. Indica el punto central
6. Aparecen líneas y el texto con el total
```

**Caso práctico:**  
Tienes valores de demanda (2R, 3C, 5R, 4R) dispersos en el plano. Usas SUM para visualizar la conexión y obtener el total (14) en un punto central.

---

## MACRO_SumaText

**Comando:** `ST` (versión mejorada)

**Descripción:**  
Suma valores numéricos de textos seleccionados con filtrado de capas y validación avanzada. Versión mejorada de SumaText(2.0) con mejor manejo de errores y feedback.

**Características:**

- Filtra capas no visibles o congeladas
- Acepta punto o coma como separador decimal
- Calcula altura promedio de textos seleccionados
- Valida textos numéricos
- Muestra resumen detallado
- Manejo robusto de cancelaciones
- Inserta resultado en punto especificado

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: ST
3. Selecciona textos con valores (ej: "12.5", "3.2", "8")
4. Presiona Enter
5. Indica el punto donde insertar el resultado
6. Aparece el texto con la suma total (23.7)
```

**Caso práctico:**  
Tienes longitudes de cables en un plano (15.5m, 23.2m, 8.0m). La macro suma todo y coloca el total (46.7m) donde lo necesites.

---

## MACRO_SumaText (2.0)

**Comando:** `ST`

**Descripción:**  
Versión original de suma de textos. Suma números en textos seleccionados y muestra el resultado. Versión más simple que la mejorada.

**Características:**

- Suma valores numéricos de TEXT y MTEXT
- Acepta solo punto como separador decimal
- Muestra total en línea de comandos
- No filtra capas
- Versión compacta y directa

**Ejemplo de uso:**

```
1. Carga la macro en AutoCAD
2. Escribe: ST
3. Selecciona textos con valores numéricos
4. Presiona Enter
5. El resultado aparece en la línea de comandos
```

**Caso práctico:**  
Necesitas sumar rápidamente varios valores numéricos del plano. Seleccionas los textos y obtienes el total en la consola.

---

## MACRO_SwapText (IMPROVE)

**Comando:** `CopyTextTo` o `SwapTextTo`

**Descripción:**  
Copia o intercambia el contenido y capa entre dos textos. Dos modos: CopyTextTo (copia unidireccional) y SwapTextTo (intercambio bidireccional).

**Características:**

- CopyTextTo: Copia contenido y capa de fuente a destino
- SwapTextTo: Intercambia contenido y capa entre ambos textos
- Interfaz interactiva
- Permite múltiples operaciones consecutivas
- Funciona con TEXT y MTEXT

**Ejemplo de uso CopyTextTo:**

```
1. Carga la macro en AutoCAD
2. Escribe: CopyTextTo
3. Selecciona el texto fuente (el que quieres copiar)
4. Selecciona el texto destino (el que será reemplazado)
5. El contenido se copia de fuente a destino
6. Puedes continuar con más pares o presionar Enter
```

**Ejemplo de uso SwapTextTo:**

```
1. Carga la macro en AutoCAD
2. Escribe: SwapTextTo
3. Selecciona el primer texto
4. Selecciona el segundo texto
5. Los contenidos se intercambian entre ambos
6. Puedes continuar con más pares o presionar Enter
```

**Caso práctico:**  
Tienes dos postes con números equivocados: P-10 debería ser P-20 y viceversa. Usas SwapTextTo para intercambiar los valores rápidamente.

---

## Notas Generales

### Carga de Macros

Para cargar cualquier macro en AutoCAD:

1. Escribe `APPLOAD` en la línea de comandos
2. Busca el archivo .lsp correspondiente
3. Haz clic en "Load"
4. La macro estará disponible para usar

### Autoload

Para cargar macros automáticamente al iniciar AutoCAD, agrégalas a la pestaña "Startup Suite" en APPLOAD.

### Compatibilidad

Todas las macros están diseñadas para AutoCAD y software compatible con AutoLISP (BricsCAD, IntelliCAD, etc.).

### Soporte

Para reportar bugs o solicitar mejoras, contacta al equipo de desarrollo.

---

**Última actualización:** Octubre 2025  
**Versión del documento:** 1.0

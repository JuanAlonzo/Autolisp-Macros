# Documentación

## Propósito

Script mejorado para normalizar y separar textos de demanda eléctrica en AutoCAD, específicamente para tipos **R** (Residencia) y **C** (Comercio).

---

## Mejoras Implementadas

### 1. **Función Genérica de Normalización**

- Nueva función `normalizar_tipo(texto, tipo='R')` que elimina código duplicado
- Maneja todos los casos: "R" → "1R", "2R" → "2R", "" → "1R"
- Reutilizable para R y C

### 2. **Soporte Multi-Tipo**

- Función `separar_combinacion(texto, obj)` para manejar "R/C", "2R/3C", etc.
- Detecta automáticamente las partes y las normaliza
- Retorna lista de tuplas con desplazamientos calculados

### 3. **Preservar Propiedades**

- Función `copiar_propiedades(obj_origen, obj_destino)`
- Copia: Layer, Color, Rotation, StyleName
- Los textos nuevos mantienen el estilo del original

### 4. **Cache de Capas**

- Diccionario `capas_cache` global
- Evita llamadas repetidas a AutoCAD para la misma capa
- Mejora significativa de performance

### 5. **Modo Dry-Run**

- Variable `DRY_RUN = False` (cambiar a True para preview)
- Muestra qué cambios se harían sin modificar el dibujo
- Perfecto para validar antes de ejecutar

### 6. **Estadísticas Detalladas**

- Diccionario `stats` con 5 categorías:
  - `separados`: Textos tipo "R/C" separados
  - `modificados_r`: Textos normalizados a "R"
  - `modificados_c`: Textos normalizados a "C"
  - `no_modificados`: Textos sin cambios
  - `errores`: Errores encontrados

### 7. **Case Insensitive**

- Usa `texto.upper()` para manejar "r", "R", "c", "C"
- Funciona con cualquier combinación de mayúsculas/minúsculas

### 8. **Espaciado Dinámico**

- `desplazamiento = altura * 1.5`
- Se adapta automáticamente al tamaño del texto
- Ya no está hardcoded

### 9. **Exportación de Log**

- Variable `EXPORT_LOG = True`
- Genera archivo con timestamp: `cambios_update_demand_YYYYMMDD_HHMMSS.txt`
- Registra todos los cambios y estadísticas

---

## Uso

### Configuración Básica

```python
# En la parte superior del archivo:
DRY_RUN = False     # True = modo preview, False = ejecutar cambios
EXPORT_LOG = True   # True = guardar log, False = solo consola
```

### Ejecución

```bash
python update_demand.py
```

### Modo Preview (Recomendado primero)

```python
DRY_RUN = True  # Cambiar esta línea
```

Ejecutar para ver qué cambios se harían sin modificar el dibujo.

---

## Ejemplos de Salida

### Consola

```
AutoCAD iniciado satisfactoriamente.
Nombre del plano: LMLO001-S_R.dwg

Separado: 'R/C' → '1R' y '1C'
Separado: '2R/3C' → '2R' y '3C'
Modificado: 'R' → '1R'
Modificado: 'c' → '1C'

============================
=== RESUMEN DE EJECUCIÓN ===
============================
Objetos procesados: 45
Objetos omitidos (capas ocultas/congeladas): 8

--- Estadísticas Detalladas ---
Textos separados (R/C): 12
Textos normalizados a 'R': 5
Textos normalizados a 'C': 3
Textos sin cambios: 25
Errores encontrados: 0

--- Capas Procesadas ---
Total de capas únicas: 6
Capas visibles: 4
Capas ocultas/congeladas: 2
============================================================

✅ Cambios aplicados exitosamente
📄 Log exportado a: cambios_update_demand_20251009_143022.txt

¡Proceso completado!
```

### Archivo Log

```
=== LOG DE ACTUALIZACIÓN DE DEMANDA ===
Fecha: 2025-10-09 14:30:22
Plano: Plano_Electrico_01.dwg
Modo: EJECUCIÓN REAL
==================================================

Separado: 'R/C' → '1R' y '1C'
Separado: '2R/3C' → '2R' y '3C'
Modificado: 'R' → '1R'
...

=== RESUMEN DE EJECUCIÓN ===
[estadísticas completas]
```

---

## Notas Importantes

1. **Backup**: Siempre hacer backup del dibujo antes de ejecutar
2. **Modo Preview**: Usar `DRY_RUN = True` primero para validar
3. **Logs**: Los archivos log se acumulan, eliminar periódicamente
4. **Capas**: Solo procesa objetos en capas visibles y no congeladas

---

## Troubleshooting

### El script no modifica nada

- Verificar que `DRY_RUN = False`
- Verificar que los textos estén en capas visibles
- Revisar el log para ver errores

### Los textos nuevos se ven diferentes

- La función `copiar_propiedades` debería mantener el estilo
- Verificar que el objeto original tenga las propiedades definidas

### Error de conexión con AutoCAD

- Asegurarse de que AutoCAD esté abierto
- Verificar que pyautocad esté instalado correctamente

---

## Mejoras Futuras Posibles

- [ ] Interfaz gráfica para configuración
- [ ] Modo batch para múltiples archivos
- [ ] Undo automático en caso de error

---

**Versión:** 2.0 (Optimizada)  
**Fecha:** Octubre 2025  
**Autor:** Sistema de mejora continua

from pyautocad import Autocad, APoint
import re
from datetime import datetime

# ============= CONFIGURACIÓN =============
DRY_RUN = False  # Cambiar a True para modo preview (no hace cambios reales)
EXPORT_LOG = True  # Exportar log de cambios a archivo
LOG_FILE = f"cambios_update_demand_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"

acad = Autocad()
if acad.doc:
    acad.prompt(
        f"AutoCAD iniciado satisfactoriamente.\nNombre del plano: {acad.doc.Name}\n")
else:
    acad.prompt("No se pudo iniciar AutoCAD.")
    exit()

# ============= FUNCIONES AUXILIARES =============

# Cache de capas para optimizar performance
capas_cache = {}


def capa_esta_visible(nombre_capa):
    """Verifica si una capa está visible (con cache)"""
    if nombre_capa not in capas_cache:
        try:
            layer = acad.doc.Layers.Item(nombre_capa)
            capas_cache[nombre_capa] = layer.LayerOn and not layer.Freeze
        except Exception as e:
            print(f"Error al verificar la capa {nombre_capa}: {e}")
            capas_cache[nombre_capa] = False
    return capas_cache[nombre_capa]


def normalizar_tipo(texto, tipo='R'):
    """
    Normaliza texto con tipo (R o C).
    Ejemplos:
        "R" → "1R"
        "2R" → "2R"
        "RC" → "1R"
        "" → "1R"
    """
    texto_upper = texto.strip().upper()
    # Buscar patrón: número opcional seguido del tipo
    match = re.search(rf'(\d*)({tipo})', texto_upper)
    if match:
        num = match.group(1) or "1"
        return f"{num}{tipo}"
    # Si contiene el tipo pero no coincide el patrón
    if tipo in texto_upper:
        return f"1{tipo}"
    return f"1{tipo}"  # Default


def separar_combinacion(texto, obj):
    """
    Separa combinaciones como "R/C", "2R/3C", etc.
    Retorna lista de tuplas: [(texto, desplazamiento_x)]
    """
    texto_upper = texto.upper()
    partes = texto.split('/')

    if len(partes) != 2:
        return None

    # Verificar que contenga R y C
    tiene_r = 'R' in texto_upper
    tiene_c = 'C' in texto_upper

    if not (tiene_r and tiene_c):
        return None

    # Normalizar cada parte
    parte_r = normalizar_tipo(partes[0], 'R')
    parte_c = normalizar_tipo(partes[1], 'C')

    # Calcular desplazamiento basado en altura del texto
    altura = obj.Height if hasattr(obj, 'Height') else 2.5
    desplazamiento = altura * 1.5  # 1.5x la altura del texto

    return [(parte_r, 0), (parte_c, desplazamiento)]


def copiar_propiedades(obj_origen, obj_destino):
    """Copia propiedades visuales del objeto origen al destino"""
    try:
        obj_destino.Layer = obj_origen.Layer
        if hasattr(obj_origen, 'Color'):
            obj_destino.Color = obj_origen.Color
        if hasattr(obj_origen, 'Rotation'):
            obj_destino.Rotation = obj_origen.Rotation
        if hasattr(obj_origen, 'StyleName'):
            obj_destino.StyleName = obj_origen.StyleName
    except Exception as e:
        print(
            f"  Advertencia: No se pudieron copiar todas las propiedades: {e}")


def escribir_log(mensaje):
    """Escribe mensaje en el log"""
    print(mensaje)
    if EXPORT_LOG:
        with open(LOG_FILE, 'a', encoding='utf-8') as f:
            f.write(mensaje + '\n')


# ============= INICIALIZACIÓN =============

# Estadísticas detalladas
stats = {
    'separados': [],
    'modificados_r': [],
    'modificados_c': [],
    'no_modificados': [],
    'errores': []
}

# Contador para seguimiento
objetos_procesados = 0
objetos_omitidos = 0

# Inicializar log
if EXPORT_LOG:
    with open(LOG_FILE, 'w', encoding='utf-8') as f:
        f.write(f"=== LOG DE ACTUALIZACIÓN DE DEMANDA ===\n")
        f.write(f"Fecha: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write(f"Plano: {acad.doc.Name}\n")
        f.write(
            f"Modo: {'DRY-RUN (Preview)' if DRY_RUN else 'EJECUCIÓN REAL'}\n")
        f.write(f"=" * 50 + "\n\n")

if DRY_RUN:
    escribir_log("\n🔍 MODO PREVIEW ACTIVADO - No se harán cambios reales\n")

# ============= PROCESAMIENTO PRINCIPAL =============

for obj in acad.iter_objects('Text'):
    try:
        # Obtener el nombre de la capa del objeto
        nombre_capa = obj.Layer

        # Verificar si la capa está visible
        if not capa_esta_visible(nombre_capa):
            objetos_omitidos += 1
            continue

        objetos_procesados += 1
        texto = obj.TextString if obj.ObjectName == 'AcDbText' else obj.Contents
        texto_upper = texto.upper()

        # === CASO 1: Separación de combinaciones (R/C) ===
        if "/" in texto and ("R" in texto_upper and "C" in texto_upper):
            resultado = separar_combinacion(texto, obj)

            if resultado:
                pos = obj.InsertionPoint
                altura = obj.Height if hasattr(obj, 'Height') else 2.5

                if DRY_RUN:
                    msg = f"[PREVIEW] Separaría: '{texto}' → '{resultado[0][0]}' y '{resultado[1][0]}'"
                    escribir_log(msg)
                    stats['separados'].append(
                        (texto, resultado[0][0], resultado[1][0]))
                else:
                    # Crear los nuevos textos
                    nuevos_textos = []
                    for texto_nuevo, desp_x in resultado:
                        nuevo_obj = acad.model.AddText(
                            texto_nuevo,
                            APoint(pos[0] + desp_x, pos[1], pos[2]),
                            altura
                        )
                        copiar_propiedades(obj, nuevo_obj)
                        nuevos_textos.append(texto_nuevo)

                    # Eliminar el texto original
                    obj.Delete()

                    msg = f"Separado: '{texto}' → '{nuevos_textos[0]}' y '{nuevos_textos[1]}'"
                    escribir_log(msg)
                    stats['separados'].append(
                        (texto, nuevos_textos[0], nuevos_textos[1]))

        # === CASO 2: Solo "R" (normalizar a "1R") ===
        elif texto_upper.strip() == "R":
            if DRY_RUN:
                msg = f"[PREVIEW] Modificaría: '{texto}' → '1R'"
                escribir_log(msg)
                stats['modificados_r'].append((texto, '1R'))
            else:
                if obj.ObjectName == 'AcDbText':
                    obj.TextString = "1R"
                else:
                    obj.Contents = "1R"
                msg = f"Modificado: '{texto}' → '1R'"
                escribir_log(msg)
                stats['modificados_r'].append((texto, '1R'))

        # === CASO 3: Solo "C" (normalizar a "1C") ===
        elif texto_upper.strip() == "C":
            if DRY_RUN:
                msg = f"[PREVIEW] Modificaría: '{texto}' → '1C'"
                escribir_log(msg)
                stats['modificados_c'].append((texto, '1C'))
            else:
                if obj.ObjectName == 'AcDbText':
                    obj.TextString = "1C"
                else:
                    obj.Contents = "1C"
                msg = f"Modificado: '{texto}' → '1C'"
                escribir_log(msg)
                stats['modificados_c'].append((texto, '1C'))

        # === CASO 4: No requiere modificación ===
        else:
            stats['no_modificados'].append(texto)

    except Exception as e:
        error_msg = f"Error al procesar objeto con texto '{texto if 'texto' in locals() else 'desconocido'}': {e}"
        escribir_log(error_msg)
        stats['errores'].append(error_msg)

# ============= RESUMEN FINAL =============

escribir_log("\n" + "=" * 60)
escribir_log("=== RESUMEN DE EJECUCIÓN ===")
escribir_log("=" * 60)
escribir_log(f"Objetos procesados: {objetos_procesados}")
escribir_log(
    f"Objetos omitidos (capas ocultas/congeladas): {objetos_omitidos}")
escribir_log("")
escribir_log("--- Estadísticas Detalladas ---")
escribir_log(f"Textos separados (R/C): {len(stats['separados'])}")
escribir_log(f"Textos normalizados a 'R': {len(stats['modificados_r'])}")
escribir_log(f"Textos normalizados a 'C': {len(stats['modificados_c'])}")
escribir_log(f"Textos sin cambios: {len(stats['no_modificados'])}")
escribir_log(f"Errores encontrados: {len(stats['errores'])}")
escribir_log("")

# Detalles de cambios
if stats['separados']:
    escribir_log("--- Detalles de Separaciones ---")
    for original, r, c in stats['separados'][:10]:  # Primeros 10
        escribir_log(f"  '{original}' → '{r}' y '{c}'")
    if len(stats['separados']) > 10:
        escribir_log(f"  ... y {len(stats['separados']) - 10} más")
    escribir_log("")

if stats['modificados_r']:
    escribir_log("--- Detalles de Normalizaciones 'R' ---")
    for original, nuevo in stats['modificados_r'][:10]:
        escribir_log(f"  '{original}' → '{nuevo}'")
    if len(stats['modificados_r']) > 10:
        escribir_log(f"  ... y {len(stats['modificados_r']) - 10} más")
    escribir_log("")

if stats['modificados_c']:
    escribir_log("--- Detalles de Normalizaciones 'C' ---")
    for original, nuevo in stats['modificados_c'][:10]:
        escribir_log(f"  '{original}' → '{nuevo}'")
    if len(stats['modificados_c']) > 10:
        escribir_log(f"  ... y {len(stats['modificados_c']) - 10} más")
    escribir_log("")

if stats['errores']:
    escribir_log("--- Errores Encontrados ---")
    for error in stats['errores']:
        escribir_log(f"  {error}")
    escribir_log("")

# Resumen de capas procesadas
escribir_log("--- Capas Procesadas ---")
escribir_log(f"Total de capas únicas: {len(capas_cache)}")
capas_visibles = sum(1 for v in capas_cache.values() if v)
capas_ocultas = len(capas_cache) - capas_visibles
escribir_log(f"Capas visibles: {capas_visibles}")
escribir_log(f"Capas ocultas/congeladas: {capas_ocultas}")

escribir_log("=" * 60)

if DRY_RUN:
    escribir_log("\n⚠️  MODO PREVIEW - No se realizaron cambios reales")
    escribir_log("   Cambia DRY_RUN = False para ejecutar los cambios")
else:
    escribir_log("\n✅ Cambios aplicados exitosamente")

if EXPORT_LOG:
    escribir_log(f"\n📄 Log exportado a: {LOG_FILE}")

print("\n¡Proceso completado!")

input("Presione Enter para continuar...")

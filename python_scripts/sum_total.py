from pyautocad import Autocad, APoint
import re

acad = Autocad()
if acad.doc:
    print(f"Conectado a AutoCAD: {acad.doc.Name}")
else:
    print("Error: No se pudo conectar con AutoCAD")
    input("\nPresiona Enter para salir...")
    exit()

# Función para verificar si una capa está visible


def capa_esta_visible(nombre_capa):
    try:
        layer = acad.doc.Layers.Item(nombre_capa)
        return layer.LayerOn and not layer.Freeze
    except Exception as e:
        print(f"Error al verificar la capa {nombre_capa}: {e}")
        return False

# Usa expresión regular para encontrar el número antes de "R", "C" o "D"


def extraer_numero(texto):
    match = re.search(r'(\d+)[RCD]', texto)
    if match:
        return int(match.group(1))
    return 0


# Inicializar contadores
suma_r = 0
suma_c = 0
suma_d = 0
textos_r = []
textos_c = []
textos_d = []
textos_procesados = 0
textos_omitidos = 0

# Solicitar al usuario que seleccione objetos en AutoCAD
print("\nPor favor, selecciona los textos en AutoCAD...")
try:
    seleccion = acad.doc.SelectionSets.Add("TEMPSET")
except:
    # Si ya existe, obtenerlo
    try:
        seleccion = acad.doc.SelectionSets.Item("TEMPSET")
        seleccion.Clear()
    except:
        print("Error al crear el conjunto de selección")
        input("\nPresiona Enter para salir...")
        exit()

try:
    # Permitir al usuario seleccionar objetos
    seleccion.SelectOnScreen()

    if seleccion.Count == 0:
        print("No se seleccionaron objetos.")
    else:
        print(f"\nObjetos seleccionados: {seleccion.Count}")

        # Procesar cada objeto seleccionado
        for obj in seleccion:
            try:
                nombre_capa = obj.Layer

                # Verificar si la capa está visible
                if not capa_esta_visible(nombre_capa):
                    textos_omitidos += 1
                    continue

                # Verificar si es un objeto de texto
                if obj.ObjectName == 'AcDbText' or obj.ObjectName == 'AcDbMText':
                    texto = obj.TextString if obj.ObjectName == 'AcDbText' else obj.Contents
                    textos_procesados += 1

                    # Verificar si el texto contiene "R", "C" o "D" (en mayúsculas)
                    texto_upper = texto.upper()

                    if "R" in texto_upper:
                        numero = extraer_numero(texto_upper)
                        suma_r += numero
                        textos_r.append(f"{texto} ({numero})")

                    if "D" in texto_upper:
                        numero = extraer_numero(texto_upper)
                        suma_d += numero
                        textos_d.append(f"{texto} ({numero})")

                    if "C" in texto_upper:
                        numero = extraer_numero(texto_upper)
                        suma_c += numero
                        textos_c.append(f"{texto} ({numero})")

            except Exception as e:
                print(f"Error procesando objeto: {e}")

        # Mostrar resultados
        print("\n=== RESUMEN ===")
        print(f"Textos procesados: {textos_procesados}")
        print(f"Textos omitidos (capas ocultas/congeladas): {textos_omitidos}")
        print(f"\nResidencia (R): {suma_r}")
        print(f"Comercio (C): {suma_c}")
        print(f"Departamento (D): {suma_d}")
        print(f"\nSuma total (R+C+D): {suma_r + suma_c + suma_d}")
        print("===============")

except Exception as e:
    print(f"Error durante la selección: {e}")
finally:
    # Limpiar el conjunto de selección
    try:
        seleccion.Delete()
    except:
        pass

input("\nPresiona Enter para salir...")

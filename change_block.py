from pyautocad import Autocad, APoint
import re
import pythoncom
from win32com.client import VARIANT
import array
import win32com.client

# Inicializar AutoCAD
acad = Autocad(create_if_not_exists=True)
print("Conectado a AutoCAD")


def reemplazar_bloques():
    """
    Función para reemplazar bloques seleccionados por un bloque específico.
    Pide el nombre del bloque de reemplazo, luego permite al usuario seleccionar
    los bloques a reemplazar, y finalmente reemplaza cada bloque seleccionado
    con el bloque especificado en la misma posición, escala y rotación.
    """
    try:
        # Solicitar el nombre del bloque de reemplazo
        bloque_reemplazo = input("Ingrese el nombre del bloque de reemplazo: ")

        # Verificar si el bloque existe en el dibujo
        try:
            # Intenta obtener el bloque usando Item
            try:
                acad.doc.Blocks.Item(bloque_reemplazo)
                bloque_existe = True
            except:
                # Si falla, intenta iterar a través de los bloques para verificar
                bloque_existe = False
                for i in range(acad.doc.Blocks.Count):
                    if acad.doc.Blocks.Item(i).Name == bloque_reemplazo:
                        bloque_existe = True
                        break

            if not bloque_existe:
                print(
                    f"Error: El bloque '{bloque_reemplazo}' no existe en el dibujo actual.")
                return
        except Exception as e:
            print(f"Error al verificar el bloque: {str(e)}")
            return

        # Mensaje para guiar al usuario
        print("Seleccione los bloques que desea reemplazar...")

        # Solicitar al usuario que seleccione los bloques
        try:
            # Usar la interfaz COM directamente para obtener una selección más específica
            doc = acad.doc  # Acceso correcto al documento activo
            utility = doc.Utility

            # Crear un filtro para seleccionar solo bloques (INSERT)
            filter_type = [0]  # Tipo de dato DXF
            filter_value = ["INSERT"]  # Valor del dato DXF (INSERT = bloque)

            # Convertir las listas a VARIANT para usar con COM
            filter_type_variant = VARIANT(
                pythoncom.VT_ARRAY | pythoncom.VT_I2, filter_type)
            filter_value_variant = VARIANT(
                pythoncom.VT_ARRAY | pythoncom.VT_BSTR, filter_value)

            # Verificar si ya existe un conjunto de selección con este nombre y eliminarlo
            try:
                temp_selection = doc.SelectionSets.Item("TempSelection")
                temp_selection.Delete()
            except:
                pass  # Si no existe, simplemente continuamos

            # Crear un nuevo conjunto de selección
            selection = doc.SelectionSets.Add("TempSelection")
            selection.SelectOnScreen(filter_type_variant, filter_value_variant)

            # Verificar si se seleccionaron bloques
            if selection.Count == 0:
                print("No se seleccionaron bloques.")
                selection.Delete()
                return

            # Contador de bloques reemplazados
            contador = 0

            # Procesar cada bloque seleccionado
            for i in range(selection.Count):
                # Obtener el objeto bloque
                bloque = selection.Item(i)

                # Asegurarse de que es un bloque (INSERT)
                # En algunas versiones de AutoCAD, la propiedad EntityName puede variar
                try:
                    es_bloque = (bloque.EntityName == 'AcDbBlockReference')
                except:
                    try:
                        es_bloque = (bloque.ObjectName == 'AcDbBlockReference')
                    except:
                        es_bloque = False

                if es_bloque:
                    # Guardar las propiedades del bloque original
                    posicion = APoint(
                        bloque.InsertionPoint[0], bloque.InsertionPoint[1], bloque.InsertionPoint[2])

                    # Obtener escala
                    escala_x = bloque.XScaleFactor
                    escala_y = bloque.YScaleFactor
                    escala_z = bloque.ZScaleFactor

                    # Obtener rotación
                    rotacion = bloque.Rotation

                    # Eliminar el bloque original
                    bloque.Delete()

                    # Insertar el nuevo bloque en la misma posición
                    nuevo_bloque = acad.model.InsertBlock(
                        posicion,
                        bloque_reemplazo,
                        escala_x,
                        escala_y,
                        escala_z,
                        rotacion
                    )

                    contador += 1

            # Eliminar el conjunto de selección temporal
            selection.Delete()

            # Mostrar mensaje de finalización
            print(f"Se han reemplazado {contador} bloques.")
            acad.app.StatusBar = f"Se han reemplazado {contador} bloques."

        except Exception as e:
            print(f"Error durante la selección o reemplazo: {str(e)}")

    except Exception as e:
        print(f"Error general: {str(e)}")


# Ejecutar la función si se ejecuta el script directamente
if __name__ == "__main__":
    reemplazar_bloques()

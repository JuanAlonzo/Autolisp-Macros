from pyautocad import Autocad


acad = Autocad()


if acad.doc:
    acad.prompt(
        f"AutoCAD iniciado satisfactoriamente.\nNombre del plano: {acad.doc.Name}\n")
else:
    acad.prompt("No se pudo iniciar AutoCAD.")

input("Presione Enter para continuar...")

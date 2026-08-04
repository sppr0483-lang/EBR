#@utor Act. Alejandro Alberto García de León Jiménez
#Auditoría interna
import pandas as pd
plantilla = pd.read_csv("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/CarteraSegurosSimulada.csv")
plantilla.columns
plantilla['EntidadRiesgo']
####################
###Catálogo de entidades
####################
ENTIDADES_RR8 = {
    # Aguascalientes
    "aguascalientes": "1",

    # Baja California
    "baja california": "2",

    # Baja California Sur
    "baja california sur": "3",

    # Campeche
    "campeche": "4",

    # Coahuila
    "coahuila": "5",
    "coahuila de zaragoza": "5",

    # Colima
    "colima": "6",

    # Chiapas
    "chiapas": "7",

    # Chihuahua
    "chihuahua": "8",

    # Ciudad de México
    "ciudad de mexico": "9",
    "ciudad de méxico": "9",
    "distrito federal": "9",
    "cdmx": "09",

    # Durango
    "durango": "10",

    # Guanajuato
    "guanajuato": "11",

    # Guerrero
    "guerrero": "12",

    # Hidalgo
    "hidalgo": "13",

    # Jalisco
    "jalisco": "14",

    # Estado de México
    "estado de mexico": "15",
    "estado de méxico": "15",
    "mexico": "15",
    "méxico": "15",
    "edomex": "15",

    # Michoacán
    "michoacan": "16",
    "michoacan de ocampo": "16",
    "michoacán de ocampo": "16",

    # Morelos
    "morelos": "17",

    # Nayarit
    "nayarit": "18",

    # Nuevo León
    "nuevo leon": "19",
    "nuevo león": "19",

    # Oaxaca
    "oaxaca": "20",

    # Puebla
    "puebla": "21",

    # Querétaro
    "queretaro": "22",
    "querétaro": "22",

    # Quintana Roo
    "quintana roo": "23",

    # San Luis Potosí
    "san luis potosi": "24",
    "san luis potosí": "24",

    # Sinaloa
    "sinaloa": "25",

    # Sonora
    "sonora": "26",

    # Tabasco
    "tabasco": "27",

    # Tamaulipas
    "tamaulipas": "28",

    # Tlaxcala
    "tlaxcala": "29",

    # Veracruz
    "veracruz": "30",
    "veracruz de ignacio de la llave": "30",

    # Yucatán
    "yucatan": "31",
    "yucatán": "31",

    # Zacatecas
    "zacatecas": "32",

    # Extranjero
    "extranjero": "33"
}
#####################
#Función normalizadora
#####################
import unicodedata

def normalizar_texto(texto):
    if texto is None:
        return None

    texto = str(texto).strip().lower()

    texto = ''.join(
        c for c in unicodedata.normalize('NFD', texto)
        if unicodedata.category(c) != 'Mn'
    )

    texto = ' '.join(texto.split())

    return texto
  ################################
  ################################
  #Catalogo entidades federativas
  def asignar_catalogo(
    df,
    columna_origen,
    diccionario,
    columna_destino="clave",
    devolver_no_encontrados=False
):
    df[columna_destino] = (
        df[columna_origen]
        .apply(normalizar_texto)
        .map(diccionario)
    )

    if devolver_no_encontrados:
        pendientes = (
            df.loc[df[columna_destino].isna(), columna_origen]
              .value_counts()
              .reset_index()
        )

        pendientes.columns = ["Valor", "Registros"]

        return df, pendientes

    return df
############################################
###########################################
  E = asignar_catalogo(
    df = plantilla,
    columna_origen="EntidadRiesgo",
    diccionario=ENTIDADES_RR8,
    columna_destino="Entidad1",
    devolver_no_encontrados=True)
######################################
######################################
 from openpyxl import load_workbook
 import csv
 E[0].to_csv("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/CarteraSegurosSimulada1.csv", index=False)

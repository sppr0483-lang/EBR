#@utor Act. Alejandro Alberto García de León Jiménez
#Auditoría interna
###Se definen librerias y llaves
library(dplyr)
library(readr)
library(readxl)
#####Se carga la plantilla proporcionada por las áreas relacionadas 
#CarteraSegurosSimulada <- read_csv("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/CarteraSegurosSimulada1.csv")
CarteraSegurosSimulada <- read_csv("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/Layout_ok1.csv")
#S = CarteraSegurosSimulada[which(is.na(CarteraSegurosSimulada$EntidadRiesgo1)),]
#S
#Se quitan espacios en blanco a Giro/ocupación para homologar
CarteraSegurosSimulada$Giro_Ocupacion = sapply(CarteraSegurosSimulada$Giro_Ocupacion,function(x){trimws(x)})
###Se quitan . indebidos de fecha de constitución y se sustituyen por /
CarteraSegurosSimulada$Edad_Constitucion<-
  gsub("\\.", "/", CarteraSegurosSimulada$Edad_Constitucion)
#Se crea la columna auxiliar para evitar problemas con fechas erróneas o ausentes
#Para comparar fechas
CarteraSegurosSimulada$Edad_Constitucion1<-
  CarteraSegurosSimulada$Edad_Constitucion
CarteraSegurosSimulada$Edad_Constitucion1 <-
  as.Date(CarteraSegurosSimulada$Edad_Constitucion1,format = "%d/%m/%Y")
##############################
##############################
###Trabajo de nacionalidad
###Si una nación es un país distinto a aquellos considerados de riesgo
##No se nace nada, en caso contrario se eleva a la siguiente categoría de riesgo
nacionalidad = unique(CarteraSegurosSimulada$Nacionalidad)
diccionario_nacionalidad <- c(
  "MEXICANA"  = "Mexicana",
  "MEXICNA"   = "Mexicana",
  "MEXCANA"   = "Mexicana",
  "MEXCIANA"  = "Mexicana",
  "MEXICAA"   = "Mexicana",
  "MEXICO"    = "Mexicana",
  "MEXIANA"   = "Mexicana",
  "MEXICOANA" = "Mexicana",
  "NEXICANA"  = "Mexicana",
  "MEXICNAA"  = "Mexicana",
  "MEXIICANA" = "Mexicana",
  "MEXXICANA" = "Mexicana"
)
CarteraSegurosSimulada$Nacionalidad = sapply(CarteraSegurosSimulada$Nacionalidad, function(x){
  ifelse(!is.na(diccionario_nacionalidad[x]),diccionario_nacionalidad[x],x)
})
unique(CarteraSegurosSimulada$Nacionalidad)
unique(CarteraSegurosSimulada$Entidad1)
which(CarteraSegurosSimulada$Entidad1==unique(CarteraSegurosSimulada$Entidad1)[32])
####Castigar no presenciales otros no hacer nada
#6 es directo
#1 es persona física agente
#2 agente persona moral
#No presencial
unique(CarteraSegurosSimulada$FormaVenta)
#######################################
#######################################

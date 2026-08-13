#####################Se capturará el valor de la última valuación del riesgo inherente de SPP
##############Previamente tuvo que correrse los códigos que actualizan la valuación de producto y zonas geográficas
#########Actualizar catálogos
##############Carre el proceso 002 para calcular el riesgo inherente de la institución.
library(dplyr)
library(readr)
library(readxl)
calif = 0.5865418
fecha_calif_1 = as.Date("2025/07/01")
fecha_calif_2 =  as.Date("2026/06/30")
source("Funciones_principales.R")
#####Se carga la plantilla proporcionada por las áreas relacionadas 
#CarteraSegurosSimulada <- read_csv("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/CarteraSegurosSimulada1.csv")
CarteraSegurosSimulada <- read_csv("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/Layout_ok1.csv", 
                                   col_types = cols(`Fecha de Emisión` = col_date(format = "%d/%m/%Y"), 
                                                    `Fecha Inicio Vigencia` = col_date(format = "%d/%m/%Y"), 
                                                    `Fecha Fin Vigencia` = col_date(format = "%d/%m/%Y")))
##########################Riesgo por cliente
corte = as.Date("2025/06/30")
CarteraSegurosSimulada = filtrado_periodo_Indiv(CarteraSegurosSimulada,corte)
###################
##################################################
################################################
##########################PREPARACION DE LOS DATOS
################################################
##########################Riesgo 
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
#####MACROVARIABLES
###Macrovariable para Fuente de los recursos
Recursos = unique(CarteraSegurosSimulada["FuenteRecursos"])
Recursos =  Recursos %>%arrange(FuenteRecursos)
Recursos$orden = c(4,4,1,2,3)
##################################
##Macrovariable para Forma de pago
Forma_Pago = unique(CarteraSegurosSimulada["MedioPago"])
Forma_Pago =  Forma_Pago %>%arrange(Forma_Pago)
Forma_Pago$orden = c(3,3,3,3,2,1)
##############################
#############################################
llave = c("FuenteRecursos"="FuenteRecursos")
llave1 = c("MedioPago"="MedioPago")
llave2 = c("Nacionalidad"="Nacionalidad")
llave3 = c("Giro_Ocupacion"="Giro_Ocupacion")
########################
X = Sub_Forma_Pago(llave1,Forma_Pago,CarteraSegurosSimulada)
Plantilla_Ocupa <- read_csv("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/Plantilla_ROcupacion.csv")
numero_grupos=5
X = Sub_Ocupacion(numero_grupos,llave3,Plantilla_Ocupa,X)
#################
#############Función de importancia
############Factor de riesgo Clientes
names(X)
orden1 = c("riesgo_fPago","riesgo_Ocupacion","riesgoRecursos")
X = Importancia_clientes(X,orden1)
############################
############################
############Se requiere saber si es física o moral
Persona = unique(CarteraSegurosSimulada["Tipo_Persona"])
X = Agravador_const(X)
unique(X$riesgoRecursos)
########################
#zona geográfica
Base1 = X
CZG = read_xlsx("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/EJECUTOR/Plantilla_riesgo_zona_geografica.xlsx")
Indice_paz = read_excel("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/EJECUTOR/Plantilla_zona_geo.xlsx")
Llave_paz = c("Entidad1"="Clave")
names(Indice_paz)
atributos = c("Clave","RiesgoZG","Riesgo_firearms_crime","Riesgo_homicide","Riesgo_organized_crime","Riesgo_violent_crime" ) 
Y = Asignar_Riesgo_ZG(Indice_paz, CZG)
X=Cruzar_Zona_geo(X,Y, Llave_paz)
riesgos_ZonGeo = c(3,2,1)
riesgos_ZonGeo = riesgos_ZonGeo/max(riesgos_ZonGeo)
Diccionario_ZonaGeo =  c(
  "A"  = riesgos_ZonGeo[1],
  "M"   = riesgos_ZonGeo[2],
  "B"   = riesgos_ZonGeo[3]
)
X$RiesgoZG_num= sapply(X$RiesgoZG, function(x){
  ifelse(!is.na(Diccionario_ZonaGeo[x]),Diccionario_ZonaGeo[x],0)
})

X$Riesgo_firearms_crime_num= sapply(X$Riesgo_firearms_crime, function(x){
  ifelse(!is.na(Diccionario_ZonaGeo[x]),Diccionario_ZonaGeo[x],0)
})
X$Riesgo_homicide_num= sapply(X$Riesgo_homicide, function(x){
  ifelse(!is.na(Diccionario_ZonaGeo[x]),Diccionario_ZonaGeo[x],0)
})
X$Riesgo_organized_crime_num= sapply(X$Riesgo_organized_crime, function(x){
  ifelse(!is.na(Diccionario_ZonaGeo[x]),Diccionario_ZonaGeo[x],0)
})
X$Riesgo_violent_crime_num= sapply(X$Riesgo_violent_crime, function(x){
  ifelse(!is.na(Diccionario_ZonaGeo[x]),Diccionario_ZonaGeo[x],0)
})
###########
names(X)
ordenZG = c("RiesgoZG_num","Riesgo_organized_crime_num","Riesgo_homicide_num","Riesgo_firearms_crime_num","Riesgo_violent_crime_num")
X = Importancia_ZG(X,ordenZG)
#####################
#####################
#####################
#######Factor Monto
#Plantilla_producto = read_csv("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/EJECUTOR/Plantilla_RPRODUCTO.csv")
X= Asignar_Riesgo_Monto(X,20)
riesgos_mont = c(3,2,1)
riesgos_mont = riesgos_mont/max(riesgos_mont)
Diccionario_Montos =  c(
  "A"  = riesgos_mont[1],
  "M"   = riesgos_mont[2],
  "B"   = riesgos_mont[3]
)
X$RiesgoMonto_Num = sapply(X$RiesgoMonto, function(x){
  ifelse(!is.na(Diccionario_Montos[x]),Diccionario_Montos[x],0)
})
##############
#############
##########Factor Riesgo Producto
atributos1 = c("Grupo")
RP = read_csv("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/Riesgo_Producto/Bases/Plantilla_RPRODUCTO.csv")
Llave_producto = c("NotaTecnica" ="Registro NT")
library(writexl)
write_xlsx(X,"C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/Riesgo_Producto/Bases/REPORTE_DULCE_DEF.xlsx")
X=Cruzar_producto(X,RP, Llave_producto,atributos1)
X$RiesgoProducto = X$Grupo/7
X$RiesgoProducto = ifelse(!is.na(X$RiesgoProducto),X$RiesgoProducto,1)
######################################
######################################
####Medida Global
names(X)
importanciaN = c("RiesgoCliente","RiesgoMonto_Num","RiesgoZONA_GEOGRAFICA","RiesgoProducto" )
pesos  = c(4:1)
pesos = pesos/sum(unique(pesos))
EBR = as.matrix(X[,importanciaN],ncol=4)%*%as.matrix(pesos, ncol=1)
EBR = as.data.frame(cbind(X,EBR))
################
#####################
####################
###Calificación  Global Global
#############################
############################
#########Seleccionar nuestras EBR
EBR_PREVIA = EBR
################tomar el mayor riesgo global
EBR_PREVIA = EBR_PREVIA%>%group_by(RFC)%>%
summarise(EBR_F = max(EBR))%>% distinct()
##############Primer filtrado, cruzar solo con los factores de interés
##############Para cruzar por la izquierda
EBR_1 = EBR%>%select(RFC,RiesgoCliente,RiesgoMonto_Num,RiesgoZONA_GEOGRAFICA,RiesgoProducto,EBR)%>%distinct
###############Cruce por RFC y EBR_F
EBR_PREVIA_DEF = EBR_PREVIA%>%left_join(EBR_1, by = c("RFC"="RFC","EBR_F"="EBR"))%>%distinct
######################Segundo filtrado si hubiera duplicados que por decimales
######no se consideraran diferentes tomar filtrado sobre RFC y EBR_F
EBR_PREVIA_DEF <- EBR_PREVIA_DEF %>%
distinct(RFC, EBR_F, .keep_all = TRUE)
unique(EBR$RFC)
#######################################
#######################################
#OUTPUT FINAL EBR CONTIENE TODAS LAS EVALUACIONES
####EBR_PREVIA_DEF CONTIENE LAS EVALUACIONES POR CLIENTE.
############################
#####Integración con calificación compañia
EBR_PREVIA_DEF$Calif_comp  =calif
EBR_PREVIA_DEF$Valuacion_I  =fecha_calif_1
EBR_PREVIA_DEF$Valuacion_S  =fecha_calif_2
###############################################################
###############################################################

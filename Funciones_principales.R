#@utor Act. Alejandro Alberto García de León Jiménez
#Auditoría interna
###Se definen librerias y llaves
library(dplyr)
library(readr)
library(readxl)
##Código con las principales funciones aplicadas
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
##############################
#############################################
llave = c("FuenteRecursos"="FuenteRecursos")
llave1 = c("MedioPago"="MedioPago")
llave2 = c("Nacionalidad"="Nacionalidad")
llave3 = c("Giro_Ocupacion"="Giro_Ocupacion")
#####################################
####################################
#######################################
###########FACTOR DE RIESGO CLIENTE
##########################################
############Funciones para calcular sub-factores de riesgo.
Sub_recursos = function(llave,Recursos,CarteraSegurosSimulada){
  Recursos$riesgoRecursos = Recursos$orden/max(Recursos$orden)
  CarteraSegurosSimulada1 <- CarteraSegurosSimulada %>% left_join(select(Recursos, FuenteRecursos, riesgoRecursos),by = llave)
  return(CarteraSegurosSimulada1)
}
Sub_Forma_Pago = function(llave1,Forma_Pago,CarteraSegurosSimulada){
  X = Sub_recursos(llave,Recursos,CarteraSegurosSimulada)
  Forma_Pago$riesgo_fPago = Forma_Pago$orden/max(unique(Forma_Pago$orden))
  CarteraSegurosSimulada1 <- X %>% left_join(select(Forma_Pago, MedioPago, riesgo_fPago),by = llave1)
  return(CarteraSegurosSimulada1)
}
##############
##############
####Aplicación de funciones anteriores
X = Sub_Forma_Pago(llave1,Forma_Pago,CarteraSegurosSimulada)
#################
################Sub-Factor Ocupación
#######################
Plantilla_Ocupa <- read_csv("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/Plantilla_ROcupacion.csv")
######SUBFACTOR OCUPACION
Sub_Ocupacion = function(numero_grupos,llave3,Plantilla_Ocupa,CarteraSegurosSimulada){
  Plantilla_Ocupa$riesgo_Ocupacion = Plantilla_Ocupa$Grupo/numero_grupos
  CarteraSegurosSimulada1 <- CarteraSegurosSimulada %>% left_join(select(Plantilla_Ocupa, Giro_Ocupacion, riesgo_Ocupacion),by = llave3)
  return(CarteraSegurosSimulada1)
}
numero_grupos=5
X = Sub_Ocupacion(numero_grupos,llave3,Plantilla_Ocupa,X)
#################
#############Función de importancia
############Factor de riesgo Clientes
names(X)
orden1 = c("riesgo_fPago","riesgo_Ocupacion","riesgoRecursos")
Importancia_clientes = function(X,orden1){
  Base = X
  pesos = c(3,2,1)
  pesos = pesos/sum(unique(pesos))
  Riesgo = Base[orden1]
  Riesgo = as.matrix(Riesgo)
  Riesgo = Riesgo%*%as.matrix(pesos,nncol=1)
  Base$RiesgoCliente  = as.numeric(Riesgo)
  return(Base)
}
X = Importancia_clientes(X,orden1)
############################
###########Agravante por edad
############Se requiere saber si es física o moral
Persona = unique(CarteraSegurosSimulada["Tipo_Persona"])
Agravador_const = function(CarteraSegurosSimulada){
  K1 = CarteraSegurosSimulada
  K1$Agrava_EDAD = "No"
  for(i in 1:nrow(CarteraSegurosSimulada)){
    if(!is.na(K1$Edad_Constitucion1[i])){
      if(K1$Tipo_Persona[i]=="PM"){
        ####Revisar fecha de constitución
        if(abs(as.numeric(K1$Edad_Constitucion1[i]-Sys.Date()))<2){
          K1$Agrava_EDAD[i] = "Si"
        }
        
      }else if(K1$Tipo_Persona[i]=="PF"){
        if(abs(as.numeric(K1$Edad_Constitucion1[i]-Sys.Date()))<25){
          K1$Agrava_EDAD[i] = "Si"
        }
      }else{
        K1$Agrava_EDAD[i] = "Si"
      }
      
    }else{
      K1$Agrava_EDAD[i] = "No_fecha"
    }
    
  }
  return(K1)
}

X = Agravador_const(X)
unique(X$riesgoRecursos)
########################
########################
#####Zona geográfica
#######################
Base1 = X
CZG = read_xlsx("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/EJECUTOR/Plantilla_riesgo_zona_geografica.xlsx")
Indice_paz = read_excel("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/EJECUTOR/Plantilla_zona_geo.xlsx")
Llave_paz = c("Entidad1"="Clave")
names(Indice_paz)
atributos = c("Clave","RiesgoZG","Riesgo_firearms_crime","Riesgo_homicide","Riesgo_organized_crime","Riesgo_violent_crime" ) 
Asignar_Riesgo_ZG <- function(Indice_paz, CZG){
  Base1 = Indice_paz
  columnas <- c(
    "overall score",
    "firearms crime",
    "homicide",
    "organized crime",
    "violent crime"
  )
  
  for(k1 in seq_along(columnas)){
    
    k <- columnas[k1]
    
    # Nombre de la columna de salida
    nombre_riesgo <- if(k == "overall score"){
      "RiesgoZG"
    } else {
      paste0("Riesgo_", gsub(" ", "_", k))
    }
    
    # Crear columna
    Base1[[nombre_riesgo]] <- "0"
    
    for(i in 1:nrow(Base1)){
      
      for(j in 1:nrow(CZG)){
        if(j==nrow(CZG)){
          Base1[[nombre_riesgo]][i] <- CZG[[6]][j]
          break
        }else{
        if(CZG[j, k1] > Base1[[k]][i]){
          
          Base1[[nombre_riesgo]][i] <- CZG[[6]][j-1]
          break
          
        }
        }
        
      }
      
    }
    
  }
  
  return(Base1)
  
}
############################
#############################Anexa al indice de paz el riesgozona geográfica
Y = Asignar_Riesgo_ZG(Indice_paz, CZG)
#write_csv(Y, "C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/EJECUTOR/Plantilla_ZG.csv")
####################Asignar a X riesgo Zona geográfica
Cruzar_Zona_geo =  function(Base1,Y, Llave_paz){
  Base1 = Base1 %>% left_join(select(Y, Clave, atributos),by = Llave_paz)
  return(Base1)
}
X=Cruzar_Zona_geo(X,Y, Llave_paz)
########################################
###################################Asignar riesgo zona geográfica de manera cuantitativa
riesgos_ZonGeo = c(3,2,1)
riesgos_ZonGeo = riesgos_mont/max(riesgos_mont)
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
##############
##############
###########
#names(X)
################Integración de todos los suriesgos en una sola medida
##Importancia de zona geográfica
ordenZG = c("RiesgoZG_num","Riesgo_organized_crime_num","Riesgo_homicide_num","Riesgo_firearms_crime_num","Riesgo_violent_crime_num")
Importancia_ZG = function(X,ordenZG){
  Base = X
  pesos = c(5,4,3,2,1)
  pesos = pesos/sum(unique(pesos))
  Riesgo = Base[ordenZG]
  Riesgo = as.matrix(Riesgo)
  Riesgo = Riesgo%*%as.matrix(pesos,nncol=1)
  Base$RiesgoZONA_GEOGRAFICA  = as.numeric(Riesgo)
  return(Base)
}
X = Importancia_ZG(X,ordenZG)
#####################
#####################
#####################
#######Factor Monto
###########################
###Si se desea importar la plantilla producto debe cambiarse esta ruta
#Plantilla_producto = read_csv("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/EJECUTOR/Plantilla_RPRODUCTO.csv")
##################Función que asigna graduación a riesgo monto
Asignar_Riesgo_Monto <- function(X, tipo_cambio){
  
  niveles <- list(
    c(2500, 7500),
    tipo_cambio * c(2500, 7500)
  )
  
  X$RiesgoMonto <- "0"
  
  for(i in 1:nrow(X)){
    
    if(X$Moneda[i] == 20){
      k <- niveles[[1]]
    } else {
      k <- niveles[[2]]
    }
    
    if(X$MontoOperacion[i] < k[1]){
      X$RiesgoMonto[i] <- "B"
      
    } else if(X$MontoOperacion[i] <= k[2]){
      X$RiesgoMonto[i] <- "M"
      
    } else {
      X$RiesgoMonto[i] <- "A"
    }
  }
  
  return(X)
}
X= Asignar_Riesgo_Monto(X,20)
###############Asignación de riesgo monto numérico
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
#############ESTO ESTA PENDIENTE!!!!!!!!!!!!!!
##########Factor Riesgo Producto
atributos1 = c("Grupo")
RP = read_csv("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/Riesgo_Producto/Bases/Plantilla_RPRODUCTO.csv")
Llave_producto = c("NotaTecnica" ="Registro NT")
Cruzar_producto =  function(X,RP, Llave_producto,atributos1){
  Base1 = X %>% left_join(select(RP, `Registro NT`, atributos1),by = Llave_producto)
  return(Base1)
}
##############################
###########################
library(writexl)
write_xlsx(X,"C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/Riesgo_Producto/Bases/REPORTE_DULCE_DEF.xlsx")
Z=Cruzar_producto(X,RP, Llave_producto,atributos1)
Casos = Z[which(is.na(Z$Grupo)),]
str(Y[, c("RiesgoZG",
          "Riesgo_firearms_crime",
          "Riesgo_homicide")])
unique(Casos$NotaTecnica)
names(Y)
Y$Riesgo_firearms_crime

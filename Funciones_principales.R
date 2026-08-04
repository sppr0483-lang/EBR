#@utor Act. Alejandro Alberto García de León Jiménez
#Auditoría interna
###Se definen librerias y llaves
library(dplyr)
library(readr)
library(readxl)
llave = c("FuenteRecursos"="FuenteRecursos")
llave1 = c("MedioPago"="MedioPago")
llave2 = c("Ocupacion"="Ocupacion")
#####################################
####################################
#######################################
###########FACTOR DE RIESGO CLIENTE
##########################################
############Funciones para calcular sub-factores de riesgo.
Sub_recursos = function(llave,Recursos,CarteraSegurosSimulada){
  Recursos$riesgoRecursos = Recursos$orden/sum(Recursos$orden)
  CarteraSegurosSimulada1 <- CarteraSegurosSimulada %>% left_join(select(Recursos, FuenteRecursos, riesgoRecursos),by = llave)
  return(CarteraSegurosSimulada1)
}
Sub_Forma_Pago = function(llave1,Forma_Pago){
  X = Sub_recursos(llave,Recursos,CarteraSegurosSimulada)
  Forma_Pago$riesgo_fPago = Forma_Pago$orden/sum(unique(Forma_Pago$orden))
  CarteraSegurosSimulada1 <- X %>% left_join(select(Forma_Pago, MedioPago, riesgo_fPago),by = llave1)
  return(CarteraSegurosSimulada1)
}
#################################################################
#################################################################
##########################ZONA GEOGRÁFICA
#############Una vez calculado el factor de riesgo cliente se calcula 
####################el factor zona geográfica
CZG = read_xlsx("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/EJECUTOR/Plantilla_riesgo_zona_geografica.xlsx")
Indice_paz = read_excel("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/EJECUTOR/Plantilla_zona_geo.xlsx")
Llave_paz = c("Entidad1"="Clave")
names(Indice_paz)
###################Asignación de riesgo a la plantilla "Plantilla_riesgo_zona_geografica.xlsx"
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
#########################################
#########################################
Y = Asignar_Riesgo_ZG(Indice_paz, CZG)
####################
write_csv(Y, "C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/EJECUTOR/Plantilla_ZG.csv")
###############################################################
###############################################################
########Factor de riesgo zona geográfica
Cruzar_Zona_geo =  function(Base1,Y, Llave_paz){
  Base1 = Base1 %>% left_join(select(Y, Clave, atributos),by = Llave_paz)
  return(Base1)
}
#####################################
######INSUMO CON FACTOR DE RIESGO
X=Cruzar_Zona_geo(X,Y, Llave_paz)

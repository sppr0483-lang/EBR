#@utor Act. Alejandro Alberto García de León Jiménez
#Auditoría interna
###Se definen librerias y llaves
library(dplyr)
library(readr)
library(readxl)
##Código con las principales funciones aplicadas
#######################################
#Periodo
################Funciones de filtrado
##Función 1
filtrado_periodo_C = function(CarteraSegurosSimulada,corte1,corte2){
  if(corte1>corte2){
    print("fechas de corte inconsistentes")
    break
  }else{
  S = CarteraSegurosSimulada%>%filter(corte1 <=`Fecha de Emisión` & `Fecha de Emisión` <=corte2)
  }
  return(S)
}
################################
##Función 2
filtrado_periodo_Indiv= function(CarteraSegurosSimulada,corte){
    S = CarteraSegurosSimulada%>%filter(`Fecha Inicio Vigencia`<= corte & corte <=`Fecha Fin Vigencia`)
  return(S)
}
##Función 3
Sub_recursos = function(llave,Recursos,CarteraSegurosSimulada){
  Recursos$riesgoRecursos = Recursos$orden/max(Recursos$orden)
  CarteraSegurosSimulada1 <- CarteraSegurosSimulada %>% left_join(select(Recursos, FuenteRecursos, riesgoRecursos),by = llave)
  return(CarteraSegurosSimulada1)
}
##Función 4
Sub_Forma_Pago = function(llave1,Forma_Pago,CarteraSegurosSimulada){
  X = Sub_recursos(llave,Recursos,CarteraSegurosSimulada)
  Forma_Pago$riesgo_fPago = Forma_Pago$orden/max(unique(Forma_Pago$orden))
  CarteraSegurosSimulada1 <- X %>% left_join(select(Forma_Pago, MedioPago, riesgo_fPago),by = llave1)
  return(CarteraSegurosSimulada1)
}
#############3
######SUBFACTOR OCUPACION
##Función 5
Sub_Ocupacion = function(numero_grupos,llave3,Plantilla_Ocupa,CarteraSegurosSimulada){
  Plantilla_Ocupa$riesgo_Ocupacion = Plantilla_Ocupa$Grupo/numero_grupos
  CarteraSegurosSimulada1 <- CarteraSegurosSimulada %>% left_join(select(Plantilla_Ocupa, Giro_Ocupacion, riesgo_Ocupacion),by = llave3)
  return(CarteraSegurosSimulada1)
}
#############Función de importancia
############Factor de riesgo Clientes
##Función 6
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
##Función 7
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
#zona geográfica
##Función 8
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
##Función 9
Cruzar_Zona_geo =  function(Base1,Y, Llave_paz){
  Base1 = Base1 %>% left_join(select(Y, Clave, atributos),by = Llave_paz)
  return(Base1)
}
##Función 10
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
#######Factor Monto
##Función 11
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
##########Factor Riesgo Producto
##Función 12
Cruzar_producto =  function(X,RP, Llave_producto,atributos1){
  Base1 = X %>% left_join(select(RP, `Registro NT`, atributos1),by = Llave_producto)
  return(Base1)
}
######################################
######################################
####Medida Global
################
#####################
####################
###Calificación  Global Global
#############################
############################
#########Seleccionar nuestras EBR
####
##Función 13
EBR_NIVELES = function(EBR_PREVIA_DEF){
  x = c(.40,.80,1)
  y1 = EBR_PREVIA_DEF$EBR_F
  EBR_PREVIA_DEF$Cali_EBR = sapply(y1,function(y1){
    if(y1<=x[1]){
      return(1/3)
    }else if(y1>= x[1] & y1<=x[2]){
      return(2/3)
    }else if(y1>x[2]){
      return(1)
    }
  })
  #################################
  y1 = EBR_PREVIA_DEF$EBR_F
  EBR_PREVIA_DEF$Cali_EBR = sapply(y1,function(y1){
    if(y1<=x[1]){
      return(1/3)
    }else if(y1>= x[1] & y1<=x[2]){
      return(2/3)
    }else if(y1>x[2]){
      return(1)
    }
  })
  ##################################
  y1 = EBR_PREVIA_DEF$RiesgoCliente
  EBR_PREVIA_DEF$Cali_RiesgoCliente = sapply(y1,function(y1){
    if(y1<=x[1]){
      return(1/3)
    }else if(y1>= x[1] & y1<=x[2]){
      return(2/3)
    }else if(y1>x[2]){
      return(1)
    }
  })
  #################################
  y1 = EBR_PREVIA_DEF$RiesgoMonto_Num
  EBR_PREVIA_DEF$Cali_RiesgoMonto = sapply(y1,function(y1){
    if(y1<=x[1]){
      return(1/3)
    }else if(y1>= x[1] & y1<=x[2]){
      return(2/3)
    }else if(y1>x[2]){
      return(1)
    }
  })
  ################################
  y1 = EBR_PREVIA_DEF$RiesgoZONA_GEOGRAFICA
  EBR_PREVIA_DEF$Cali_RiesgoZONA_GEOGRAFICA = sapply(y1,function(y1){
    if(y1<=x[1]){
      return(1/3)
    }else if(y1>= x[1] & y1<=x[2]){
      return(2/3)
    }else if(y1>x[2]){
      return(1)
    }
  })
  ########################################
  y1 = EBR_PREVIA_DEF$RiesgoProducto
  EBR_PREVIA_DEF$RiesgoProducto = sapply(y1,function(y1){
    if(y1<=x[1]){
      return(1/3)
    }else if(y1>= x[1] & y1<=x[2]){
      return(2/3)
    }else if(y1>x[2]){
      return(1)
    }
  })
  #########################################
  return(EBR_PREVIA_DEF)
}
#############################################
############################################
############################################

#@utor Act. Alejandro Alberto García de León Jiménez
#Auditoría interna
#' Preparar datos2#'
#' #' @return M#' @export
Generador_X = function(n){
#n <- ncol(C)-1
M <- t(sapply(0:(2^n - 1), function(x) {
  as.integer(intToBits(x))[1:n]
}))
return(M)
}
library(readr)
#' Preparar datos2#'
#' #' @param ruta Ruta del archivo CSV.6
#' #' @return Matriz.7#' @export
Preparar_datos = function(ruta){
  Factor_Riesgo_Producto <- read_csv(ruta)
  X = Factor_Riesgo_Producto[,-1]
  X[is.na(X)] = 0
  X = as.matrix(X)
  return(X)
}
#' Construir_Grupos_interiores#'
#' #' @param C centroides
#' #' @param X matriz de notas técnicas
#' #' @return lista con Grupos y nombres#' @export
Construir_Grupos_interiores = function(C,X){
  n = nrow(C)
  l = list()
  s = colMeans(X)
  for(i in 1:n){
    l[[i]] = n*t(t(C[i,]-s))%*%t(C[i,]-s)
  }
  B = Reduce(`+`, l)
  return(B)
}
#' Fisher_A#'
#' #' @param C1 centroides
#' #' @param X matriz de notas técnicas
#' #' @return lista con función de altman#' @export
Fisher_A = function(C1,X){
  B = Construir_Grupos_interiores(C1,X)
  #nombres = Construir_Grupos_interiores(C1,X)[[3]]
  #diag(W)[diag(W) == 0] <- 1
  Y<- X[, apply(X, 2, var) > 0]
  W  = cov(Y)
  S <- W
  x = solve(S)
  x = x%*%B
  eigenvectores = eigen(x)[[2]]
  eigenvectores = eigenvectores[,1]
  eigenvectores  =Re(eigenvectores)
  eigenvectores =  eigenvectores / as.numeric(sqrt(t(eigenvectores) %*% W %*% t(t(eigenvectores))))
  return(eigenvectores)
  }

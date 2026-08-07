#@utor Act. Alejandro Alberto García de León Jiménez
#Auditoría interna
#' distancia_fisher#'
#' #' @param C1 centroides
#' #' @param X matriz de notas técnicas
#' #'  @param P1 un elemento a agrupar
#' #'  @param grupo un elemento de los centroides
#' #' @return distancia de fisher#' @export
distancia_fisher = function(C1,X,P1,grupo){
  nombres = Fisher_A(C1,X)
  #C2 = C1
  #P2 = P1
  #P2 = P2[nombres]
  #C2 = C2[,nombres]
  difffe = P1-C1[grupo,]
  d = abs(sum(Fisher_A(C1,X)*difffe))
  return(d)
}
#' Grupo#'
#' #' @param C1 centroides
#' #' @param X matriz de notas técnicas
#' #'  @param P1 un elemento a agrupar
#' #'  @param grupo un elemento de los centroides
#' #' @return grupo asignado#' @export
Grupo = function(C1,X,P1){
 lista = list()
  for(grupo in 1:nrow(C1)){
    lista[[grupo]] = distancia_fisher(C1,X,P1,grupo)
  }
 k = which.min(unlist(lista))
  return(k)
}
#' Valor_distancia#'
#' #' @param C1 centroides
#' #' @param X matriz de notas técnicas
#' #'  @param P1 un elemento a agrupar
#' #' @return valor distancia mínima asignado#' @export
Valor_distancia = function(C1,X,P1){
  lista = list()
  for(grupo in 1:nrow(C1)){
    lista[[grupo]] = distancia_fisher(C1,X,P1,grupo)
  }
  k = min(unlist(lista))
  return(k)
}
#' Valor_distancia2#'
#' #' @param C1 centroides
#' #' @param X matriz de notas técnicas
#' #'  @param P1 un elemento a agrupar
#' #' @return valor distancia mínima asignado#' @export
Valor_distancia2 = function(C1,X,P1){
  lista = list()
  for(grupo in 1:nrow(C1)){
    lista[[grupo]] = distancia_fisher(C1,X,P1,grupo)
  }
  k = median(unlist(lista))
  return(k)
}

library(igraph)
setwd(getwd())

leerDatos<-function(archivo) {
  frDatos <- read.delim(archivo, sep="", skip=10, header=FALSE)
  frDatos <- frDatos[-length(frDatos$V1),] #Elimina última fila
  frDatos <- frDatos[,c(-1,-4,-6,-7)] 
  frDatos[,1]<-sub(",", "", frDatos[,1]) #elimina ,
  frDatos[,2]<-sub(")", "", frDatos[,2]) # elimina )
  frDatos <- data.matrix(frDatos) # convierte a numericos
  return(frDatos)
}

ultimaCiudad <- function(frDatos){
  maxpunto <- max(max(frDatos[,1]),max(frDatos[,2]))
}

crear_matriz_Adjunta <- function(frDatos) {
  maxpunto <- ultimaCiudad(frDatos)
  matrizAdjunta <- matrix(0L, nrow = maxpunto, ncol = maxpunto)
  cantidadaparsear <- dim(frDatos)[1]
  for (i in 1:cantidadaparsear) {
    indiceorigen <- frDatos[i,1]
    indicedestino <- frDatos[i,2]
    peso <- frDatos[i,3]
    matrizAdjunta[indiceorigen,indicedestino] <- peso
  }
  matrizAdjunta <- t(matrizAdjunta) + matrizAdjunta
  return(matrizAdjunta)
}

crearAdjucaminos <- function(Adjvector){
  x <- Adjvector
  for (i in 1:length(Adjvector)){
    if (Adjvector[i] == 0) {
      x[i] <- Inf
    }
  }
  return(x)
}

algoritmo_vecino_proximo <- function(Adjunta){
  vecino = matrix(0L, nrow = dim(Adjunta)[1], ncol = dim(Adjunta)[2])
  dimension = dim(Adjunta)[1]
  porrevisar <- 1:dimension
  nodo_actual <- round(runif(1, min = 1, max = dimension))|
  while(TRUE){
    caminos <- crearAdjucaminos(Adjunta[,nodo_actual])
    while(TRUE){
      nodo_posible <- which(Adjunta[,nodo_actual] == min(caminos))[1]
      if(nodo_posible %in% porrevisar){
        porrevisar[nodo_posible] <- 0
        vecino[nodo_actual, nodo_posible] <- 1
        nodo_actual <- nodo_posible
        break()
      } else {
        caminos[nodo_posible] <- Inf
        }
    }
    if( sum(porrevisar) == 0){
      break()
    }
  }
  
}


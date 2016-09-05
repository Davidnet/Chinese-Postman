library(igraph)
setwd(getwd())



#----------------Leer datos--------------------------------------------
# Funcion leerDatos
# Del archivo formado por el paste (en instruciones mas abajo), me crea la estructura de datos de información
# recibe: Ruta del Archivo
# returna: Estructura de dato
leerDatos<-function(archivo) {
  frDatos <- read.delim(archivo, sep="", skip=10, header=FALSE)
  frDatos <- frDatos[-length(frDatos$V1),] #Elimina última fila
  frDatos <- frDatos[,c(-1,-4,-6,-7)] 
  frDatos[,1]<-sub(",", "", frDatos[,1]) #elimina ,
  frDatos[,2]<-sub(")", "", frDatos[,2]) # elimina )
  frDatos <- data.matrix(frDatos) # convierte a numericos
  return(frDatos)
}

#---------------------Creacion de Matriz adjunta -------------
# Función creador matriz adjunta
# De la informacion de frDatos y de la informacion del punto maximo es decir del nodo maximo,
# crear la matriz adjunta con los pesos especificados
# retorna la matriz adjunta
crear_matriz_Adjunta <- function(frDatos, maxpunto) {
  
  matrizadjunta <- matrix(0L, nrow = maxpunto, ncol = maxpunto)
  cantidadaparsear <- dim(frDatos)[1]
    for (i in 1:cantidadaparsear) {
      indiceorigen <- frDatos[i,1]
      indicedestino <- frDatos[i,2]
      indicepeso <- frDatos[i,3]
      matrizadjunta[indiceorigen,indicedestino] <- indicepeso
      }
  matrizadjunta <- t(matrizadjunta) + matrizadjunta
  return(matrizadjunta)
}

#------------------------GENERADOR DE SOLUCIÓN INICIAL------------------------
# Creemos que una solucion inicial esrecorer por cada valor diferente de cero de frDatos$v3 != 0,
# para la tarea 4, esperamos modificar aleatoriamente la solución.
# Retorna una solucion inicial
crear_solucioninicial<-function(frDatos, matrizadjunta){
  solucion_inicial = matrix(0L, nrow = dim(matrizadjunta)[1], ncol = dim(matrizadjunta)[2])
  for (i in 1:cantidadaparsear) {
    indiceorigen <- frDatos[i,1]
    indicedestino <- frDatos[i,2]
    indicepeso <- frDatos[i,3]
    solucion_inicial[i,j] <- 1
  }
  solucion_inicial = t(solucion_inicial) + solucion_inicial
  return(solucion_inicial)
}


crear_solucioninicial2<-function(frDatos, matrizadjunta){
  solucion_inicial = matrix(0L, nrow = dim(matrizadjunta)[1], ncol = dim(matrizadjunta)[2])
  x2 <- 1:dim(matrizadjunta)
  Flag = TRUE
  while(Flag){
    x1 <- round(runif(1, min = 1, max = dim(matrizadjunta)))
    x2[x1] <- 0
    maximo <- max(matrizadjunta[,x1])
    indice <- which(matrizadjunta[,x1] == maximo)[1]
    solucion_inicial[x1,indice] <- 1
    if(sum(x2)==0){
      Flag = FALSE
    }
  }
  return(solucion_inicial)
}
#----------------------Calculador de distancia total--------------
#funcion distancia_total
#Recibe una matriz adjunta y una posible solucion y hace el calculo de costo
#retorna costo total

distancia_total <- function(matrizadjunta, solucion){
  sumatotal <- 0
  for (i in 1:(dim(solucion))) {
    for (j in 1:dim(solucion)){
      sumatotal <- sumatotal + solucion[i,j]*matrizadjunta[i,j]
    }
    
  }
  return(sumatotal)
}

#-------------------------Modificar la Solucion------------------
# Toma una solución y devuelve una solución modifica 
# retorna una nueva solución

solucionmodificada <- function(solucion){
  return(solucion)   ## Para futuro de tarea 4
}

#------------------------------EMPEZAR A BUSCAR SOLUCION-------
# Coge un frDatos y trata de resolver el problema de recorido
#input: un frDatos
#output: una matriz de solucion
empezar_Solucion <- function(frDatos){
  t <- 10000
  flag = TRUE
  cantidadaparsear <- dim(frDatos)[1]
  maxv2 <- max(frDatos[,1])
  maxv3 <- max(frDatos[,2])
  maxpunto <- max(maxv2,maxv3)
  matriz_Adjunta <- crear_matriz_Adjunta(frDatos, maxpunto)
  solucioninicial <- crear_solucioninicial2(frDatos, matriz_Adjunta)
  mejorsolucion <- solucioninicial
  while(flag){
  calidad1 <- distancia_total(matriz_Adjunta, solucioninicial)
  solucionModificada <- solucionmodificada(solucioninicial)
  calidad2 <- distancia_total(matriz_Adjunta, solucionModificada)
  cantidadpaso = exp((calidad2- calidad1)/t)
  if((calidad2 > calidad1) || (1 < cantidadpaso)){
    solucioninicial <- solucionModificada
    t <- t - 1
  }
  else {
    if(calidad1>calidad2){
      solucionModificada <- solucioninicial
    }
  }
  if(t<0){
    flag = FALSE
  }
  }
  return(solucioninicial)
}



#----------------Volver iGraph------------------------------------------
volverIgraph<-function(frDatos) {
  nVertices <- max(frDatos[,c(1,2)])
  g <- graph.empty(directed=F) + vertices(1:nVertices) # para respetar el orden, que coincida nodo con fila y columna
  g <- g + graph.data.frame(data.frame(from=frDatos[,1],to=frDatos[,2],weight=frDatos[,3]),directed=F)# 
}

#-----------------------------------------------------------------------
#   Instrucciones Sueltas
#-----------------------------------------------------------------------

lInstancias <-  paste0("./gdb/gdb", 1:23 ,".dat" )
for (i in 1:23) {
  frDatos <- leerDatos(lInstancias[i])
  solucion <- empezar_Solucion(frDatos)
}



#volver grafo igraph
  g <- volverIgraph(frDatos)
plot(g, layout=layout_with_kk, edge.width=E(g)$weight)

library(igraph)
leerDatos<-function(archivo) {
  frDatos <- read.delim(archivo, sep="", skip=10, header=FALSE)
  frDatos <- frDatos[-length(frDatos$V1),] #Elimina última fila
  frDatos <- frDatos[,c(-1,-4,-6,-7)]
  frDatos[,1]<-sub(",", "", frDatos[,1]) # Elimina ,
  frDatos[,2]<-sub(")", "", frDatos[,2]) # Elimina )
  frDatos <- data.matrix(frDatos) # Convierte a Númericos
  return(frDatos)
}

modificarSolucion <- function(solucion, m_obligatorios, adjunta){
  solucion[ is.na(solucion) ] <- Inf
  x <- ceiling(runif(1, min=0, max = max(m_obligatorios[,1])))
  y <- adjunta[,x]
  y[ y== 0] <- Inf
  
  return(solucion)
}

ultimaCiudad <- function(frDatos){
  maxpunto <- max(max(frDatos[,1]),max(frDatos[,2]))
}

plotear_solucion <- function(solucion){
  g <- graph.adjacency(solucion, mode = "undirected")
  plot(g)
}

simulated_annealing <- function(m_adjunta, m_solucioninicial, m_obligatorios){
  t <- 10000
  S <- m_solucioninicial
  while(t > 0){
    sol_mod <- modificarSolucion(S, m_obligatorios, adjunta = m_adjunta)
    t1 <- calculo_distancia(m_adjunta, sol_mod)
    t2 <-  calculo_distancia(m_adjunta, S)
    Flag1 <- ( t1 < t2)
    Flag2 <- (runif(1) < exp((t1 - t2)/t ))
    if (Flag1 || Flag2) {
      S <- sol_mod
      t <- t-1
    }
  }
  S
}

crear_solucion_inicial <- function(m_adjunta, m_obligatoria){
  m_adjunta[ m_adjunta = Inf ] <- 0
  g <- graph.adjacency(m_adjunta, weighted=TRUE, diag = FALSE )
  s.paths <- shortest.paths(g, algorithm = "dijkstra")
}

alt_crear_solucion_inicial <- function(m_adjunta, m_obligatoria){
  solucion <- matrix(nrow = dim(m_adjunta)[1], ncol = dim(m_adjunta)[1])
  for (i in 1:length(m_obligatoria[,1])) {
    solucion[m_obligatoria[i,1],m_obligatoria[i,2]] <- 1
  }
  x <- calculo_distancia(m_adjunta, solucion)
  cat(" Distancia inicial: ")
  cat(x)
  return(solucion)
}
calculo_distancia <- function(m_adjunta, m_caminos){
  m_adjunta[is.na(m_adjunta)] <- 0
  m_caminos[is.na(m_caminos)] <- 0
  sum(m_adjunta*m_caminos)
}

recorridos_necesarios <- function(frDatos){
  iniciales <- as.vector(frDatos[,1])[c(TRUE, FALSE)] # Sacar los impares de el origen
  finales <- as.vector(frDatos[,2])[c(TRUE,FALSE)]  # Sacar los impares del destino
  matriz <- as.matrix(cbind(iniciales,finales))
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
  #matrizAdjunta[matrizAdjunta == 0] <- Inf
}

calcular_solucion_usando_sa <- function(frDatos){
  adjunta <- crear_matriz_Adjunta(frDatos)
  m_necesarios <- recorridos_necesarios(frDatos)
  solucion_inicial <- alt_crear_solucion_inicial(adjunta, m_necesarios)
  solucion <- simulated_annealing(adjunta, solucion_inicial, m_necesarios)
  x <- calculo_distancia(adjunta, solucion)
  cat("\t distancia final es: ")
  cat(x)
  return(solucion)
}

### Instruciones sueltas ###
setwd(getwd())
lInstancias <-  paste0("./gdb/gdb", 1:23 ,".dat" )
sink("tablasolucion.txt")
for (i in 13:23){
  frDatos <- leerDatos(lInstancias[i])
  cat("Nombre del Archivo: ")
  cat(lInstancias[i])
  solucion <- calcular_solucion_usando_sa(frDatos)
  plotear_solucion(solucion)
  cat("\n")
}
sink()

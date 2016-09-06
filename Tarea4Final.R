#Set the working directory
setwd(getwd())

# Instalación de Paquetes Adicionales
#install.packages("igraph")
library("igraph")
#---------------------------------------------------------------------
# Funciones
#---------------------------------------------------------------------


# FUnción Leer Datos
# Recibe un archivo como parómetro
# Realiza la lectura de los datos y los guarda en un dataframe
# Retorna los datos leidos

leerDatos<-function(archivo) {
  
  frDatos <- read.delim(archivo, sep="", skip=10, header=FALSE)
  frDatos <- frDatos[-length(frDatos$V1),] #Elimina última fila
  frDatos <- frDatos[,c(-1,-4,-6,-7)]
  frDatos[,1]<-sub(",", "", frDatos[,1]) # Elimina ,
  frDatos[,2]<-sub(")", "", frDatos[,2]) # Elimina )
  frDatos <- data.matrix(frDatos) # Convierte a Númericos
  return(frDatos)
  
}


# Función Modificar Solución
# Recibe como parómetros la solución inicial y los arcos obligatorios
# Retorna la Solución teniendo en cuenta los arcos obligatorios

modificarSolucion <- function(solucion, m_obligatorios, adjunta){
  solucion[ is.na(solucion) ] <- Inf
  x <- ceiling(runif(1, min=0, max = max(m_obligatorios[,1])))
  y <- adjunta[,x]
  y[ y== 0] <- Inf
  minimo <- min(y)
  indice <- match(minimo, y)
  escribir <- TRUE
  if(minimo != Inf ){
    for (i in 1:dim(m_obligatorios)[1]) {
      for (j in 1:2)
      {
        if(indice == m_obligatorios[i,j]){
          escribir <- FALSE
        }
      }
    }
  }
  if(escribir){
    solucion[x,indice] <- 1
  }
  solucion[solucion == Inf] <- 0
  return(solucion)
  }
  


# Función óltima Ciudad
# Recibe los datos extraidos de los archivos
# Obtiene la cantidad de ciudades para dimensionar el problema
# Retorna la dimensión de los problemas

ultimaCiudad <- function(frDatos){
  
  maxpunto <- max(max(frDatos[,1]),max(frDatos[,2]))
  
}

# Función plotear solución
# Recibe la Solución como parómetro
# Grafica la solución con las conexiones de los arcos

plotear_solucion <- function(solucion){
  
  g <- graph.adjacency(solucion, mode = "undirected")
  plot(g)
  
}

# Función SA
# Recibe como parómetro la solución inicial, los arcos obligatorios y la matriz adjunta con los costos asociados
# A partir de una temperatura se realiza un vecindario y se itera para explorar posibles mejoras en FO
# Retorna una solución Modificada

simulated_annealing <- function(m_adjunta, m_solucioninicial, m_obligatorios){
 
  t <- ceiling(runif(1, 1000, 5000)) #Temperatura
  S <- m_solucioninicial #Solución
  
  cat("\t número de iteraciones:  ")
  cat(t)
  while(t > 0){
      sol_mod <- modificarSolucion(S, m_obligatorios, adjunta = m_adjunta)
      t1 <- calculo_distancia(m_adjunta, sol_mod)
      t2 <-  calculo_distancia(m_adjunta, S)
      Flag1 <- ( t1 < t2)
      Flag2 <- (runif(1) < exp((t1 - t2)/t )) #Probabilidad de Aceotación
      if (Flag1 || Flag2) {
          S <- sol_mod
          t <- t-1
      }
  }

  return(S)
}

# Función Crear Solución Inicial
# Recibe la matriz de costos y los arcos obligatorios como parómetros
# Retorna una solución factible inicial

crear_solucion_inicial <- function(m_adjunta, m_obligatoria){
  
  m_adjunta[ m_adjunta = Inf ] <- 0
  g <- graph.adjacency(m_adjunta, weighted=TRUE, diag = FALSE )
  s.paths <- shortest.paths(g, algorithm = "dijkstra")
  
}

# Función Solución Inicial Alternativa
# Recibe la matriz de costos y los arcos obligatorios como parómetros
# Retorna una solución factible inicial

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

# Función calcular distancias
# Recibe como parómetros la matriz de costos y la matriz de arcos
# Calcula la distancia y los costos entre arcos

calculo_distancia <- function(m_adjunta, m_caminos){
  
  m_adjunta[is.na(m_adjunta)] <- 0
  m_caminos[is.na(m_caminos)] <- 0
  sum(m_adjunta*m_caminos)
  
}

# Función Recorrido Necesario 
# Recibe como parómetro los datos extraidos de los problemas
# Retorna los arcos obligatorios del cartero RURAL chino

recorridos_necesarios <- function(frDatos){
  
  iniciales <- as.vector(frDatos[,1])[c(TRUE, FALSE)] # Sacar los impares de el origen
  finales <- as.vector(frDatos[,2])[c(TRUE,FALSE)]  # Sacar los impares del destino
  matriz <- as.matrix(cbind(iniciales,finales))
  
}

# Función Crear Matriz Adjunta
# Recibe los datos de los problemas como parómetros
# Guarda en una matriz los costos o pesos de cada arcos del problema
# Retorna la matriz de costos (adjunta)

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

# Función Solución con SA
# Recibe los datos como parómetros
# Itera y trabaja en los vencindarios para buscar soluciones con mejor FO
# Retorna una solución

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

#---------------------------------------------------------------------
# Código Ejecutable
#---------------------------------------------------------------------

#Lectura de Archivos, óltimas 10 instancias
lInstancias <-  paste0("./gdb/gdb", 1:23 ,".dat" )
sink("tablasolucion.txt")

# Soluciones, creación de tabla y gróficas de los problemas
# La tabla se encuentra en un archivo aparte, se imprime y se genera en el directorio de trabajo
for (i in 13:23){
  
  frDatos <- leerDatos(lInstancias[i])
  cat("Nombre del Archivo: ")
  cat(lInstancias[i])
  solucion <- calcular_solucion_usando_sa(frDatos)
  plotear_solucion(solucion)
  cat("\n")
  
}
sink()

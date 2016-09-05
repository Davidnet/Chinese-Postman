#Llama la libreria que calcula el Gessiano y el Gradiente de manera numérica
#install.packages("numDeriv") #Por si s requiere instalar dicho paquete
library("numDeriv")

#-------------------------------------------------------------------------------------------------
# Funciones
#--------------------------------------------------------------------------------------------------
#F
# Guarda la función requerida
#Entra como parámetro un valor de x
# retorna el valor de la función evaluada en x
f <- function(x){
  y <- x^2 - sin(x)
  
  return(y)
}

#F2
# Guarda la función requerida para la segunda parte
#Entra como parámetro un valor de x y y
# retorna el valor de la función evaluada en x y en y
f2 <- function(x){
  
  k <- x[1]^2 + x[2]^2 + 5*x[1]*x[2] + 2*x[1] - 3*x[2] -20
  
  return(k)
}

#Búsqueda Exhaustiva
#Realiza búsqueda Exhaustiva para encontrar el mínimo de la función
#Tiene como entrada la intensidad de la búsqueda
#Retorna el mínimo de la función
busqueda_Exhaustiva <- function(h){
  
  x0 <- 1
  x_A <- x0 - h

  while (f(x_A) < f(x0)) {

    x0 <- x_A
    x_A <- x_A - h
      
  }
  
  return (f(x_A))
}

# Razón Dorada
# Función que busca el mínimo de una función a tráves del número phi
# Tiene como parámetros dos límites: el superior e inferior y la tolerancia
# Retorna el mínimo encontrado con dichos parámetros


razon_Dorada <- function(x_U, x_l, tol) {
  
  R <- (1+sqrt(5))/2
  
  while (x_U - x_l > tol) {
    
    d <- (x_U - x_l)/R
    x_1 <- x_l + d
    x_2 <-x_U - d
    
    if (f(x_1) < f(x_2)) {
      
      return(x_1)
      
    }
    else {
      
      return(x_2)
      
    }
  }
}

#Gradiente
#Calcula el grandiente de la función
#tiene como parámetros las coordenadas del punto que se quiere
#Retorna un vector con los componentes del gradiente

gradiente <- function(x,y){
  
  coordenadas <- c(x,y)
  a <- grad(func=f2,coordenadas)
  
  return(a)
}

#Hessian
# Calcula el Gessiano de la función
#Tiene como parámetros las coordenadas x y y de un punto
#Retorna el Gessiano como matriz de la función

Gessiano <- function(x,y) {
  
  coordenadas1 <- c(x,y)
  g <- hessian(func = f2, coordenadas1)
  
  return(g)
  
}


#Método de Newton
#Tiene alpha como parámetro para la intensidad de la búsqueda
#Retorna el mínimo de la función de acuerdo a un vector aleatorio

met_Newton <- function(alpha) {
  
  x <- runif(1, min =-1000000, max = 1000000)
  y <- runif(1, min =-1000000, max = 1000000)
  
  v <- c(x,y)
  
  while (v != c(0,1.5)) {
  
  ma <- Gessiano(v[1],v[2])    
  d <- 1/(det(ma))
  
  invGess <- d*matrix(c(ma[2,4],-ma[1,2],-ma[2,1],ma[1,1]), nrow = 2, ncol = 2)
  
  mmult <- invGess %*% gradiente(v[1],v[2])
  
  v <- v - alpha*mmult
  
  }
  
  return(v)
}



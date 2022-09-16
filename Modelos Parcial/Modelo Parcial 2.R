# Ejercicio 1
# Programar una función "Multiplos" que, dado un vector "v", cuyos elementos son 
# enteros, detecte en el mismo los elementos que sean múltiplos de su número de 
# orden en el vector. La función deberá generar un vector "r" que los contenga en 
# la misma secuencia en que aparecen en el vector original.

#Ejemplo:
#  1  2  3   4  5  6
#v = c(7, 3, 9, 10, 7, 6)
#Multiplos(v)
#r =  [7,9,6] 

es_multiplo <- function(a,b){
  return (a%%b == 0)
}

Multiplos = function(v){
  r = c()
  for (i in 1:length(v)){
    if (es_multiplo(v[i], i)){
      r = append(r, v[i])
    }
  }
  return(r)
}

v = c(7, 3, 9, 10, 7, 6)
Multiplos(v)

# Ejercicio 2
# Dada una matriz cuadrada "A" de orden n cualquiera, cuyos elementos son
# enteros,programar una función "Diagonal" que  encuentre los n elementos mayores 
# entre todos ellos.
# Para ordenar los elementos. puede utilizar el comando "sort()" , que trabaja 
# también sobre matrices. Puede investigar el comando "sort()" con " ?sort() "
# Con los n elementos mayores, rellene la diagonal de una matriz "R" de la misma 
# dimensión que "A"
# Con los restantes elementos, rellene los lugares fuera de la diagonal 
# (no importa que mantengan o no la ubicación original).
# La salida será la matriz "R", así construida.

#Ejemplo:
#  A=matrix(c(1,3,5,4,7,9,5,10,2), ncol=3)
#[1,]    1    4    5
#[2,]    3    7   10
#[3,]    5    9    2

#R = 
#[1,]    7    3    5
#[2,]    1    9    5
#[3,]    2    4   10

obtener_mayores = function(v, n){
  aux = c()
  for (i in 1:length(v)){
    if (length(v) - i < (n)){
      aux = append(aux, v[i])
    }
  }
  
  return(aux)
}

obtener_menores = function(v, n){
  aux = c()
  for (i in 1:length(v)){
    if (i <= n){
      aux = append(aux, v[i])
    }
  }
  
  return(aux)
}

contiene_elemento = function(v, x){
  for (i in 1:length(v)){
    if (v[i] == x) return(TRUE)
  }
  
  return(FALSE)
}

Diagonal = function(A){
  R = A
  orden = ncol(A)
  mayores = obtener_mayores(sort(A), orden)
  menores = obtener_menores(sort(A), length(A)-orden)
  diag = diag(R)

  for (i in 1:orden){
    for (j in 1:orden){
      if (i == j){
        R[i, j] = mayores[i]
      } else {
        R[i, j] = menores[1]
        menores = menores[-1]
      }
    }
  }
  
  return(t(R)) ##Para que quede igual que el resultado, no hacia falta...
}



A=matrix(c(1,3,5,4,7,9,5,10,2), ncol=3); A
Diagonal(A)


#R = 
#[1,]    7    3    5
#[2,]    1    9    5
#[3,]    2    4   10







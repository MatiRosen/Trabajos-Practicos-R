#Parcial 1
# Ejercicio 1
# Programar una función "Repetidos1" que, dado un vector "v", cuyos elementos 
# son enteros desde 1 hasta 10, detecte en el mismo los elementos que estén 
# repetidos. La función deberá reportar una matriz "resultado1" de 2 columnas 
# donde en la 1era. columna aparezcan los números que se repiten y en la 2da. 
# columna la cantidad de veces (2 ó más) que aparecen
# Ejemplo:
#  v = c(7, 3, 9, 10, 7, 6, 7, 3, 7,5, 1)

# Repetidos1(v)
# resultado1 =    [1,]  3  2 [2,]  7  4
enteros_en_rango = function(x, a, b){
  for (i in 1:length(x)){
    if (x[i] != round(x[i])) return(FALSE)
    if (x[i] < a || x[i] > b) return(FALSE)
  }
  
  return(TRUE)
}

veces_repetidas = function(v, x){
  cant = 0
  
  for (i in 1:length(v)){
    if (v[i] == x){
      cant = cant+1
    }
  }
  
  return(cant)
}


Repetidos1 = function(v){
  if (!enteros_en_rango(v, 1, 10)) return("No son numeros enteros o no están en el rango...")
  resultado1 = matrix(c(0, 0), ncol = 2)

  for (i in 1:length(v)){
    cant = veces_repetidas(v, v[i])
    if (cant > 1){
      if (resultado1[1, 2] == 0){
        resultado1[nrow(resultado1), ] = c(v[i], cant)
      } else{
        if (veces_repetidas(resultado1[, 1], v[i]) == 0)
        resultado1 = rbind(resultado1, c(v[i], cant))
      }
    }
  }
  
  return(resultado1)
}

v = c(7, 3, 9, 10, 7, 6, 7, 3, 7,5, 1)

Repetidos1(v)


#Ejercicio 2
# Idem anterior pero ahora los números se deberán buscar en una matriz "A" dato 
# de dimensión genérica m x n. El reporte será ahora una matriz "resultado2"de 3 
# columnas. En la 1era. deberá aparecer el número que se repite, tantas veces 
# como se encuentre en la matriz. En la 2da. y la 3ra se deberá reportar fila y 
# columna de cada aparición.
#Ejemplo:
#  A = 
#  [1,]  5  3  4
#  [2,]  6  5  3
#  [3,]  3  7  9

#Repetidos2(A)   
#resultado2 = 
#  [1,] 5  1  1
#  [2,] 5  2  2
#  [3,] 3  1  2
#  [4,] 3  2  3
#  [5,] 3  3  1
contiene_elemento = function(v, x){
  for (i in 1:length(v)){
    if (v[i] == x) return(TRUE) 
  }
  
  return(FALSE)
}
Repetidos2 = function(A){
  resultado2 = matrix(0, ncol = 3)
  repetidos = Repetidos1(sort(A))[, 1]
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      if (contiene_elemento(repetidos, A[i, j])){
        resultado2 = rbind(resultado2, c(A[i,j], i, j))
      }
    }
  }
  
  return(resultado2[-1,])
}

A = matrix(c(5,6,3,3,5,7,4,3,9), 3); A
Repetidos2(A)






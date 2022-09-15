# Ejercicio 1
# Escriba un programa – función cuya salida sea el elemento n-ésimo de la
# sucesión de Fibonacci. La sucesión de Fibonacci tiene como sus dos primeros
# elementos el 0 y el 1. Los restantes términos se calculan como la suma de los
# dos anteriores.

fibonacci <- function(n){
  suma = 0
  numAnteriorASuma = 0
  contador = 1
  primeraVez = TRUE
  while(contador <= n){
    suma = suma + numAnteriorASuma
    numAnteriorASuma = suma - numAnteriorASuma
    if (primeraVez){
      numAnteriorASuma = 1
      primeraVez = FALSE
    }
    contador = contador + 1
  }
  
  return(suma)
}
fibonacci(10)

#-------------------------------------------------------------------------------
# Ejercicio 2
# Escriba un programa (función) que determine si un número entero positivo dado
# es par o impar. Recuerde que puede hacerlo comprobando si el número es
# divisible exactamente por 2. La salida deberá ser 1 si es par y 0 si no es par.
# Luego, utilizando ese programa como esclavo, escriba una nueva función que
# cuente los número pares e impares de una matriz de tamaño arbitrario que
# contiene elementos enteros positivos. La salida de este programa será una matriz
# cuyo primer elemento sea la cantidad de valores pares en A y el segundo
# elemento la cantidad de valores impares en A.

esPar = function(x){
  if (x < 0 || x != round(x)){
    return("No es un numero entero positivo")
  }
  if (x%%2 == 0){
    return(1)
  } else{
    return(0)
  }
}

contarPares = function(A){
  if (!is.matrix(A) ){
    return("No es una matriz semidefinida positiva")
  }
  v = matrix(1:2)
  cantPares = 0
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      if (!is.numeric(esPar(A[i, j]))){
        return("No es una matriz semidefinida positiva entera...")
      }
      if (esPar(A[i, j])){
        cantPares = cantPares + 1
      }
    }
  }
  v[1, 1] = cantPares
  v[2, 1] = (nrow(A)*ncol(A)) - cantPares
  return(v)
}
matriz = matrix(round(runif(15)*100), 3); matriz

contarPares(matriz)

#-------------------------------------------------------------------------------
# Ejercicio 3
# Escriba un programa – función que reporte una aproximación del número pi
# sumando “n” términos de la serie de Leibnitz.
# 𝑗
# ∑((-1)^n)/(2n+1) =𝜋/4
#𝑛=0
# Verificar que, al aumentar “n”, disminuye el error. 

pi_leibnitz = function(n){
  suma = 0
  for (i in 1:n){
    suma = suma + 4*(((-1)^(i-1))/(2*(i-1)+1))
  }
  return(suma)
}

pi(100)
pi(10000)
pi(100000)

#-------------------------------------------------------------------------------
# Ejercicio 4
# Escriba un programa (función) que calcule el número pi mediante simulación de
# ‘lluvia’ al azar, usando n observaciones. El proceso es el siguiente: Se generan
# puntos al azar (x, y) con x, y pertenecientes a [0, 1] y se verifica si pertenecen o
# no al interior de un cuarto de círculo de radio uno (Es decir, se comprueba si
# x^2 + y^2 < 1). La proporción de puntos en el interior del círculo converge en
# probabilidad al área de ese cuarto de círculo a medida que se usan más puntos.
# Nota: El área de un cuarto de círculo de radio 1 es A=pi/4.

piLluvia = function(n){
  x = runif(n)
  y = runif(n)
  puntos = 0
  for (i in 1:n){
    if ((x[i]^2 + y[i]^2) < 1){
      puntos = puntos + 1
    }
  }
  
  return(4*puntos / n)
}

piLluvia(100000)

#-------------------------------------------------------------------------------
# Ejercicio 5
# Escriba una función en R que devuelva el mínimo elemento del vector x.
obtenerMin = function(x){
  min = NULL
  for (i in 1:length(x)){
    if (is.null(min) || x[i] < min){
      min = x[i]
    } 
  }
  
  return(min)
}
v = round(runif(50, 1, 100)); v
obtenerMin(v)

#-------------------------------------------------------------------------------
# Ejercicio 6
# Escriba una función que, dado un vector, calcule la varianza, pudiendo indicar
# como argumento de la función, si se busca una varianza muestral o poblacional. 
calcularVar = function(x, t='P'){
  if (t=='P'){
    return(sum((x-mean(x))^2)/(length(x)))
  } else if(t=='M'){
    return(sum((x-mean(x))^2)/(length(x)-1))
  } else{
    return("El tipo debe ser 'M': Muestral o 'P': Poblacional")
  }
}

x=round(runif(5)*100) ; x
calcularVar(x, 'M')

#-------------------------------------------------------------------------------
# Ejercicio 7
# Escriba una función en R que se aplique sobre un escalar x que sea entero y
# positivo y determine si x es primo o no. Un número primo es aquel que sólo es
# divisible por 1 y por sí mismo. Para saberlo deberá dividir el número por todos
# los otros entre 1 y x y ver si surge algún resto cero. Si el número es primo el
# programa retorna un valor 1. Si no lo es retorna un valor 0.

enteroYPositivo = function(x){
  return(x > 0 && x == round(x))
}


esPrimo = function(x){
  if (!enteroYPositivo(x)){
    return("No es entero positivo...")
  }
  if (x == 2){
    return(1)
  }
  y = x-1
  for (i in 2:y){
    if (x%%i == 0){
      return(0)
    }
  }
  
  return(1)
 
}

esPrimo(13)

#-------------------------------------------------------------------------------
# Ejercicio 8
# Escriba una función en R que aplique el programa anterior como esclavo para
# determinar si los elementos de una matriz o vector y de enteros positivos son
# primos. El programa deberá retornar un vector o matriz de igual dimensión que 
# y, pero con 1s donde los elementos correspondientes de y son primos y 0s donde
# los elementos de y son no primos.

elementosPrimos = function(y){
  for (i in 1:length(y)){
    y[i] = esPrimo(y[i])
  }
  
  return(y)
}

vector = c(1:18) ; vector
matriz = matrix(vector, 6) ; matriz
elementosPrimos(vector)
elementosPrimos(matriz)

#-------------------------------------------------------------------------------
# Ejercicio 9
# Escriba un programa tipo función que genere una matriz A de dimensión m x n.
# Dicha matriz deberá tener como elementos números primos entre 0 y 101.

obtenerNumerosPrimosEntre = function(a, b){
  numeros = c()
  for (i in a:b){
    if (primo(i) == 1){
      numeros = append(numeros, i)
    }
  }
  
  return(numeros)
}
obtenerNumerosPrimosEntre(0, 101)


generarMatrizElementosPrimos = function(m, n){
  cantElementos = m*n
  primos = obtenerNumerosPrimosEntre(0, 101)
  return(matrix(replicate(cantElementos, sample(primos, 1)), m, n))
}

generarMatrizElementosPrimos(2, 4)

#-------------------------------------------------------------------------------
# Ejercicio 10
# Escriba una función en R tal que tome a un vector x como argumento de entrada
# y devuelva un vector y cuyos elementos surgen de ordenar x de menor a mayor
# mediante el siguiente procedimiento ("Método de la Burbuja" o “bubble sort”):
# Se recorre todo el vector x comparando cada elemento con el anterior. Si están
# en orden incorrecto se permutan y se continúa avanzando, comparando y si es
# necesario, permutando. Una vez que se llega al final de x se vuelve a comenzar.
# El proceso termina cuando, ante un recorrido completo en x no se realiza
# ninguna permutación.

metodoBurbuja = function(x){
  elementoAnterior = x[1]
  for (i in 1:length(x)){
    if (x[i] < elementoAnterior){
      elementoRempl = x[i-1]
      x[i-1] = x[i]
      x[i] = elementoRempl
    }
    elementoAnterior = x[i]
  }
  
  return(x)
}


vectoresIguales = function(x, y){
  if (length(x) != length(y)) return(FALSE)
  for (i in 1: length(x)){
    if (x[i] != y[i]) return(FALSE)
  }
  
  return(TRUE)
}

ordenarVector = function(x){
  huboPermutacion = TRUE
  while(huboPermutacion){
    huboPermutacion = TRUE
    y = x
    x = metodoBurbuja(x)
    if (vectoresIguales(x, y)){
      huboPermutacion = FALSE
    }
  }
  return(x)
}

ordenarVector(c(9, 8, 2, 3, 5, 1, -2, 5, 4, 33, 6, 7 ,3 ,2))

#-------------------------------------------------------------------------------
# Ejercicio 11
# Escriba una función similar a la anterior pero que admita un segundo argumento,
# tal que si vale 0 ordena de menor a mayor, mientras que si vale 1 ordena de
# mayor a menor.

metodoBurbuja = function(x, o = 0){
  elementoAnterior = x[1]
  for (i in 1:length(x)){
    if ((o == 0 && x[i] < elementoAnterior) || (o == 1 && x[i] > elementoAnterior)){
      elementoRempl = x[i-1]
      x[i-1] = x[i]
      x[i] = elementoRempl
    }
    elementoAnterior = x[i]
  }
  return(x)
}

ordenarVector = function(x, o = 0){
  huboPermutacion = TRUE
  while(huboPermutacion){
    huboPermutacion = TRUE
    y = x
    x = metodoBurbuja(x, o)
    if (vectoresIguales(x, y)){
      huboPermutacion = FALSE
    }
  }
  return(x)
}



ordenarVector(c(9, 8, 2, 3, 5, 1, -2, 5, 4, 33, 6, 7 ,3 ,2), 0)

#-------------------------------------------------------------------------------
# Ejercicio 12
# Escriba un programa con formato de función que realice lo siguiente: Encontrar
# el número más pequeño en una matriz dada y reportarlo, así como su posición en
# la matriz. Si se repite, reportar todas las posiciones en que se encuentra. El input
# deberá ser una matriz arbitraria de m*n y el output, el escalar correspondiente al
# valor mínimo, así como el vector con la posición del número encontrado (todas
# las posiciones, si hubiera más de una). 

numMasPeque <- function(A){
  if (!is.matrix(A)){
    return("Se debe ingresar una matriz...")
  }
  numMasChico = min(A)
  v = c()
  for (i in 1:length(A)){
    if (A[i] == numMasChico){
      v = append(v, i)
    }
  }
  print(numMasChico); print(v)
}

numMasPeque(matrix(c(5, 3, 4, 3, 7, 8, 5, 6, 7, 6, 3, 4), 3, 4))

#-------------------------------------------------------------------------------
# Ejercicio 13
# En una lista de números enteros consecutivos desde “a” hasta “b” encontrar
# aquellos que son divisibles por “c”. Reportar un vector con los números que
# cumplan la condición. Los argumentos deberán ser un vector y el escalar por el
# cual se quiere dividir.

encontrarDivisibles = function(x, a){
  v = c()
  for (i in 1:length(x)){
    if (x[i] %% a == 0){
      v = append(v, x[i])
    }
  }
  
  return(v)
}

encontrarDivisibles(c(10:50), 7)

#-------------------------------------------------------------------------------
# Ejercicio 14
# Dada una matriz dato genere otra que sea la imagen espejada de la original
# respecto del eje vertical.

invertirVector <- function(a){
  b = a[length(a)]
  for (i in 1:length(a)){
    b = append(b, a[length(a)-i])
  }
  return(b)
}

espejarMatriz = function(A){
  if (!is.matrix(A)){
    return("No es una matriz")
  }
  
  matriz = A
  for (i in 1:nrow(A)){
    v = c()
    for(j in 1:ncol(A)){
      v = append(v, A[i, j])
    }
    matriz[i, ] = invertirVector(v)
  }
  return(matriz)
}

espejarMatriz(matrix(c(1:25), 5, 5))

#-------------------------------------------------------------------------------
# Ejercicio 15
# Generar una función que, para cada elemento de una matriz de n*m, determine
# si cada elemento es primo y/o par, y exprese los resultados en una sola matriz. Si
# el número fuera primo, en la matriz de output se debería ver 1, si fuera entero, se
# debería ver 1, y si fuera ambas, se debería ver el número 2. 
enteroYPositivo = function(x){
  return(x > 0 && x == round(x))
}


esPrimo = function(x){
  if (!enteroYPositivo(x)){
    return("No es entero positivo...")
  }
  if (x == 2){
    return(1)
  }
  y = x-1
  for (i in 2:y){
    if (x%%i == 0){
      return(0)
    }
  }
  
  return(1)
  
}

esPrimo(2)

esPar <- function(x){
  if (x%%2 == 0){
    return(1)
  } else{
    return(0)
  }
}

matrizEsPrimoOPar = function(A){
  tamanio=ncol(A)*nrow(A)
  B = matrix(c(1:tamanio), nrow(A), ncol(A))
  for(i in 1:tamanio){
    B[i] = esPrimo(A[i]) + esPar(A[i])
  }
  return(B)
}

matrizEsPrimoOPar(matrix(c(1:18), 3))

#-------------------------------------------------------------------------------
# Ejercicio 16
# Generar una función que, dado un escalar que indique el número de caras de un
# dado, itere tiradas hasta alcanzar una de las dos siguientes condiciones: el
# número de tiradas “n” (argumento de la función) o el valor “s” (argumento de la
# función) de la suma de las tiradas. Se recomienda usar la función sample.
# Recuerde que dos condiciones pueden incluirse utilizando el símbolo “&”. 

iterarDado <- function(x, n, s){
  dado <- 1:x
  contador = 0
  suma = 0
  while(contador < n && suma < s){
    suma = suma + sample(dado, 1)
    contador = contador + 1
  }
  sprintf("Se iteró %d veces, y la suma fue de %d", contador, suma)
}


iterarDado(10, 12, 54)
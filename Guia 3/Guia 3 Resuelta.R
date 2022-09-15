#Ej 1
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

#Ej 2
Escriba un programa (función) que determine si un número entero positivo dado
es par o impar. Recuerde que puede hacerlo comprobando si el número es
divisible exactamente por 2. La salida deberá ser 1 si es par y 0 si no es par.
Luego, utilizando ese programa como esclavo, escriba una nueva función que
cuente los número pares e impares de una matriz de tamaño arbitrario que
contiene elementos enteros positivos. La salida de este programa será una matriz
cuyo primer elemento sea la cantidad de valores pares en A y el segundo
elemento la cantidad de valores impares en A.

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

#Ej 3
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

#Ej 4
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


#Ej 5
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

#Ej 6
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

#Ej 7
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

#Ej 8
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


#Ej 9
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

#Ej 10
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


#Ej 11
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


#Ej 12
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


#Ej 13
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


#Ej 14
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


#Ej 15
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

#Ej 16
Generar una función que, dado un escalar que indique el número de caras de un
dado, itere tiradas hasta alcanzar una de las dos siguientes condiciones: el
número de tiradas “n” (argumento de la función) o el valor “s” (argumento de la
función) de la suma de las tiradas. Se recomienda usar la función sample.
Recuerde que dos condiciones pueden incluirse utilizando el símbolo “&”. 

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















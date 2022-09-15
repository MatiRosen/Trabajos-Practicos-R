#Ej 1
#Escribir un programa que recorra los elementos de un vector v y encuentre los
#que son divisibles por un número dato x.
#Reporte una copia del vector donde los elementos divisibles se reemplacen por "div".
#Ejemplo: v=(1:9); w=[1 2 DIV 4 5 DIV 6 7 DIV]

esDivisible <- function(a, b){
  return(a%%b==0)
}

numsDivisibles <- function(v, x){
  w = c()
  for(i in 1:length(v)){
    if (esDivisible(v[i], x)){
      w = append(w, "DIV")
    } else{
      w = append(w, v[i])
    }
  }
  
  return(w)
}

numsDivisibles(1:9, 3)

#Ej 2
#Programe una función que detecte si un número entero positivo es o no un cuadrado
#perfecto: cuadrado(x). Utilizando la función anterior como "esclavo" programe una
#función "maestro" que tenga como argumento una matriz cualquiera de orden “m” x
#“n”.
#Esta función detecta(A) debe detectar los cuadrados perfectos de la matriz de entrada
#y devolver una matriz similar pero con dichos cuadrados perfectos reemplazados por
#"CP".

cuadrado <- function(x){
  if (x != round(x) || x < 0){
    return("No es un numero entero positivo.")
  }
  
  if (sqrt(x) == round(sqrt(x))){
    return("CP")
  } else{
    return(x)
  }
}

detecta = function(A){
  B = A
  for (i in 1:length(A)){
    B[i] = cuadrado(A[i])
  }
  
  return(B)
}

detecta(matrix(c(1:18), 3))

#Ej 3
#Dado un vector de números v (dato) eliminar los elementos repetidos generando un
#nuevo vector w. con los elementos de este vector construir un nuevo vector u que
#contenga todos los productos posibles formados por 2 elementos distintos de w.
#Reportar el vector u.

contieneElemento <- function(v, a){
  if (length(v) == 0){
    return(FALSE)
  }
  for (i in 1:length(v)){
    if (v[i] == a){
      return(TRUE)
    } 
  }
  return(FALSE)
}

eliminarRepetidos = function(v){
  w = c()
  for (i in 1:length(v)){
    if (!contieneElemento(w, v[i])){
      w = append(w, v[i])
    }
    
  }
  return(w)
}

productosPosibles = function(v){
  w = eliminarRepetidos(v)
  u = c()
  for (i in 1:length(w)){
    for (j in 1:length(w)){
      if (i != j){
        u = append(u, w[i]*w[j])
      }
    }
  }
  
  return(eliminarRepetidos(u))
}
prueba = c(1,2,5,3,5,5,3,6,7)
eliminarRepetidos(prueba)
productosPosibles(prueba)



#Ej 4
#Dada una matriz A de n x 2, donde cada fila representa las coordenadas (x , y) de un
#punto en el plano. Determinar cuáles de dichos puntos se encuentran dentro de la zona
#determinada por los ejes cartesianos y la recta y = a - b x , con a y b positivos dados
#por el vector "param". Reportar las filas que representan puntos dentro de la zona.

estaDentro = function(x, y, param){
  a = param[1]
  b = param[2]
  if ((y < a && x < 0) || (y > a && x > 0)) return(FALSE)
  if (y == a && x != 0) return(FALSE)
  
  if (x > (y-a)/(-b) && y < a) return(FALSE)
  if (x < (y-a)/(-b) && y > a) return(FALSE)
  
  return(TRUE)
}

puntosEnZona = function(x, param){
  if (param[1] <= 0 || param[2] <= 0) return("a o b no es positivo.")
  
  v = c()
  for (i in 1:nrow(A)){
    x = A[i, 1]
    y = A[i, 2]
    if (estaDentro(x, y, param)){
      cat("La fila", i, " está dentro de la zona\n")
      v = append(v, i)
    }
  }
  
  return(v)
}

A = matrix(c(-1,2,3,16,5,6), ncol=2) ; A

puntosEnZona(A, c(6, 9))

#Ej 5
#Dado un vector de números llamado "base" de enteros positivos, buscar en otro vector
#v cualquiera de números enteros positivos la aparición de los elementos de "base".
#Cada vez que aparezca un elemento en v que ya exista en "base", reemplazar dicho
#elemento en v por el número 0. Reportar el vector w.
#base=[2145] v=[3456792385] w=[3456790380]

esEnteroPositivo <- function(x){
  for (i in 1:length(x)){
    if (x[i] != round(x[i]) || x[i] < 0) return(FALSE)
  }
  
  return(TRUE)
}

contieneElemento <- function(v, x){
  for (i in 1:length(v)){
    if (v[i] == x) return(TRUE)
  }
  
  return(FALSE)
}

buscarElementos <- function(base, v){
  if (!esEnteroPositivo(base)){
    return("La base no tiene todos sus elementos enteros positivos.")
  } else if(!esEnteroPositivo(v)){
    return("El vector no tiene todos sus elementos enteros positivos.")
  }
  
  w = v
  for (i in 1:length(v)){
    if (contieneElemento(base, v[i])){
      w[i] = 0
    }
  }
  
  return(w)
}

base = c(2, 1, 4, 5)
v =c(3, 4, 5, 6, 7, 9, 2, 3, 8, 5)
buscarElementos(base, v)

#Ej 6
#Programe una función que reconozca si un número "x" es par o impar. Llamele par(x).
#Con dicho programa como esclavo programe una función maestro llamado
#"seleccion" que recorra un vector "v" #(dato) de enteros positivos y verifique si los
#elementos impares son números impares y si los elementos pares son números pares.
#El programa debe dejar inalterados los elementos que cumplan las condiciones y debe
#reemplazar por el valor 0 los elementos que no las cumplan.
#Ejemplo: v=[7, 12, 4, 6, 8, 5, 9, 20]
#seleccion(v) -> [7, 12, 0, 6, 0, 0, 9, 20]
par <- function(x){
  return(x%%2 == 0)
}

seleccion = function(v){
  for(i in 1:length(v)){
    if (par(i) != par(v[i])){
      v[i] = 0
    }
  }
  
  return(v)
}

v= c(7, 12, 4, 6, 8, 5, 9, 20)
seleccion(v)

#Ej 7
#Escriba una función "mayor_que" que tenga como argumentos dos variables reales x
#y a. si el número x es mayor que a el programa debe retornar un 1 (verdadero), sino
#debe retornar un 0 (falso). Escriba un programa detecta que tenga como argumentos
#un vector v de dimensión libre y un escalar h. Utilizando el programa esclavo, se
#deberá analizar si cada elemento de v es mayor que el número h. Como salida debe
#generar un vector w de la misma longitud que v, que tenga 1s en las posiciones de los
#elementos mayores que h, y 0 en las menores o iguales.

mayor_que <- function(x, a){
  if (x > a){
    return(1)
  } else{
    return(0)
  }
}


detecta = function(v, h){
  w = v
  for (i in 1:length(v)){
     w[i] = mayor_que(v[i], h)
  }
  return(w)
}

detecta(c(1,3,2,4,6,8,7,6,5), 5)


#Ej 8
#Escriba un programa que detecte en un vector dato los números que llamaremos
#"contiguos", que resultan del producto de dos enteros seguidos, por ejemplo 6 = 2*3 o
#56 = 7*8. El reporte del programa será una matriz de n * 3 donde en la primera
#columna sea la posición del número contiguo en el vector y las otras dos sean los
#factores en que se descompone el contiguo.
#Sugerencia: pruebe con los enteros cercanos a la raíz del número.

buscar_contiguo = function(x){
  v = c(x)
  for (i in 2:x){
    aux = x%%i
    if (aux == 0 && (x/i+1 == i)){
      v = append(v, c(x/i, i))
    }
  }
  
  return(v)
}

buscar_contiguo(6)

detectar_contiguos <- function(dato){
  contiguos = c()
  for (i in 1:length(dato)){
    contiguo = buscar_contiguo(dato[i])
    if (length(contiguo) == 3){
      contiguos = append(contiguos, contiguo)
    }
  }
  
  return(t(matrix(contiguos, ncol=3)))
}

detectar_contiguos(c(1,2,3,4,5,6,56))

#Ej9
#Programar una función cuyo desempeño sea generar un vector con elementos enteros
#positivos aleatorios entre 0 y 100, llamado "a", de longitud "n" (argumento). Con los
#elementos de dicho vector "a" generar otro vector "b" cuyo primer elemento sea 1 y
#los elementos siguientes se calculan sumando y restando alternativamente los
#números del vector a. La cantidad de elementos del vector b deberá ser, entonces n+1

num_aleatorio = function(a, b){
  return(round(runif(1, a, b)))
}

generar_vector_nums_aleatorios <- function(n){
  a = c()
  for (i in 1:n){
    a = append(a, num_aleatorio(0,100))
  }
  print(a)
  return(a)
}

generar_vector <- function(n){
  a = generar_vector_nums_aleatorios(n)
  b = c(1)
  for(i in 1:n){
    aux = a[i]
    if (i %% 2 != 0){
      aux = b[i] - aux
    } else{
      aux = b[i] + aux
    }
    b = append(b, aux)
  }
  
  return(b)
}

generar_vector(3)


#Ej 10
#Escriba una función "elementos" que tenga como argumentos una matriz cuadrada A
#y un escalar positivo b. El programa debe crear una nueva matriz c que tenga en su
#diagonal principal los valores de a multiplicados por b y fuera de su diagonal principal
#los valores de a divididos por b. La matriz c será la salida del programa.

elementos = function(A, b){
  C = A
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      if (i == j){
        C[i,j] = A[i, j] * b
      } else{
        C[i,j] = A[i, j] / b
      }
    }
  }
  
  return(C)
}

elementos(matrix(c(1:9), 3), 5)

#Ej 11
#Generar un programa esclavo divisible(x,v) que deberá determinar si un número "x"es
#divisible por alguno de los elementos de un vector dado "v". La salida será TRUE o 1
#si es divisible y FALSE o 0 si no lo es. Con ese esclavo generar una función
#Divisible(A) que deberá determinar si los elementos de una matriz cuadrada son
#divisibles por alguno de los elementos de su diagonal principal. En caso de que lo
#sean se deberán reemplazar por "d", si no lo son se reemplazarán por "nd".

divisible = function(x, v){
  for (i in 1:length(v)){
    if (x%%v[i] == 0) return(TRUE)
  }
  
  return(FALSE)
}

Divisible = function(A){
  diagonal = diag(A)
  B = A
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      if (divisible(A[i, j], diagonal)){
        B[i, j] = "d"
      } else{
        B[i, j] = "nd"
      }
    }
  }
  
  return(B)
}

Divisible(matrix(seq(3, 27, 1), 5))


#12
#PARTE 1:
#  La secuencia de Collatz de un número entero se construye de la siguiente forma:
#  *Si el número es par, se lo divide por dos;
#  *Si es impar, se le multiplica tres y se le suma uno;
#  *La sucesión termina al llegar a uno.
#  Dado un número entero positivo n, cree una función cuyo resultado sea su secuencia
#  de Collatz

es_par <- function(x){
  return(x%%2 == 0)
}

sec_collatz = function(n){
  v = c()
  while (n != 1){
    v = append(v, n)
    if (es_par(n)){
      n = n/2
    } else{
      n = (n*3) + 1
    }
  }
  v = append(v, 1)
  return(v)
}

sec_collatz(6)

#PARTE 2
#  Dado un vector, cree una matriz cuya primera columna sea el vector, y en las filas, a
#  continuación de cada elemento, la secuencia de Collatz correspondiente. En caso que
#  no coincida la longitud, completar con 0 una vez terminada la misma

matriz_collatz = function(v){
  mayorValor = 0
  for (i in 1:length(v)){
    sec = sec_collatz(v[i])
    if (length(sec) > mayorValor){
      mayorValor = length(sec)
    }
  }
  
  A = matrix(nrow=mayorValor, ncol = 3, byrow = TRUE)
  for (i in 1:length(v)){
    sec = sec_collatz(v[i])
    cant = mayorValor - length(sec)
    if (cant > 0){
      sec = append(sec, replicate(cant, 0))
    }
    A[, i] = sec
  }
  print(A)
  
  return(t(A))
}

matriz_collatz(c(6, 11, 27))

#Ej 13
#Un tablero de ajedrez es una grilla de ocho filas y ocho columnas, numeradas de 1 a 8.
#Dos de las piezas del juego de ajedrez son el alfil y la torre. El alfil se desplaza en
#diagonal, mientras que la torre se desplaza horizontal o verticalmente. Una pieza
#puede ser capturada por otra si está en una casilla a la cual la otra puede desplazarse.
#Escriba una función que reciba como argumento las posiciones en el tablero de un
#alfil y de una torre, e indique cuál pieza captura a la otra.
tamaño_numeros = function(x){
  return(floor(log10(x)) + 1)
}

separar_numeros <- function(x){
  v = c()
  tamaño = tamaño_numeros(x)
  for(i in 1:tamaño){
    num = floor((x/(10^(tamaño-i))))
    for (j in 1:length(v)){
      if (length(v) == 0) break
      num = (num - (v[j] * 10^(length(v)+1-j)))
    }
    v = append(v, num)
  }
    
  return(v)
}

fuera_rango = function(v, a, b){
  for (i in 1:length(v)){
    if (v[i] < a || v[i] > b) return(TRUE)
  }
  return(FALSE)
}

es_diagonal = function(x, y){
  for (i in 1:7){
    if (x[1] == y[1] + i && x[2] == y[2] - i) return(TRUE)
    if (x[1] == y[1] - i && x[2] == y[2] + i) return(TRUE)
    if (x[1] == y[1] + i && x[2] == y[2] + i) return(TRUE)
    if (x[1] == y[1] - i && x[2] == y[2] - i) return(TRUE)
  }
  return(FALSE)
}

ajedrez <- function(torre, alfil){
  if (torre == alfil){
    return("Las piezas están en el mismo lugar...")
  }
  
  t = separar_numeros(torre)
  a = separar_numeros(alfil)
  if (fuera_rango(t, 1, 8) || fuera_rango(a, 1, 8)) return("Piezas fuera de rango...")
  
  A = matrix(ncol = 8, nrow = 8)
  for(i in 1:8){
    for (j in 1:8){
      A[i, j] = (i*10)+j
    }
  }
  
  print(A)
  if (t[1] == a[1] || t[2] == a[2]){
    return("La torre captura al alfil...")
  } else if (es_diagonal(t, a)){
    return("El alfil captura a la torre")
  } else{
    return("Ninguno se capturó")
  }
}

ajedrez(88,38)

#Ej 14
#Diseña un programa que, dados cinco puntos en el plano, determine cuál de los cuatro
#últimos puntos es más cercano al primero. Un punto se representará con dos variables:
#  una para la abcisa y otra para la ordenada. La distancia entre dos puntos (x1, y1) y
#(x2, y2) es RAIZ[(x1 − x2)^2 + (y1 − y2)^2].

puntos_cercanos = function(a,b,c,d,e){
  abcisa = a[1]
  ordenada = a[2]
  distMasCercano = sqrt((abcisa - b[1])^2 + (ordenada - b[2])^2)
  masCercano = b
  A = matrix(c(c, d, e), nrow=3, byrow = TRUE); print(A)
  for (i in 1:3){
    dist = sqrt((abcisa -A[i, 1])^2 + (ordenada-A[i,2])^2)
    if (dist < distMasCercano){
      distMasCercano = dist
      if (i == 1){
        masCercano = c
      } else if (i == 2){
        masCercano = d 
      } else{
        masCercano = e
      }
    }
  }
  
  return(masCercano)
}

puntos_cercanos(c(1,2), c(1,5), c(10,1), c(9,5), c(3, 6))

#Ej 15
#Sabiendo que en R existe un vector "letters" cuyos elementos son las letras del
#abecedario, indique una función que, dado un vector de números enteros positivos
#hasta el 26, "decodifique" el mensaje oculto, retornando un vector cuyos elementos
#sean las letras correspondientes. Indicar cantidad de vocales y consonantes.

en_rango = function(v, min, max){
  for (i in 1:length(v)){
    if (v[i] > max || v[i] < min) return(FALSE)
  }
  return(TRUE)
}

es_vocal = function(x){
  return(x == "a" || x == "e" || x == "i" || x == "o" || x == "u")
}

decodificar <- function(v){
  if (!en_rango(v, 1, 26))return("Hay un numero menor a 1 o mayor a 26...")
  w = c()
  cantVocales = 0
  for (i in 1:length(v)){
    w = append(w, letters[v[i]])
    if (es_vocal(letters[v[i]])){
      cantVocales = cantVocales + 1
    }
  }
  cat("Hay ", cantVocales," vocales y ", length(v)-cantVocales," consonantes...")
  return(w)
}


decodificar(c(13, 5, 12, 9, 20, 5, 1, 13, 15))


#Ej 16
#Una de las técnicas de criptografía más rudimentarias consiste en sustituir cada uno de
#los caracteres por otro situado "n" posiciones más a la derecha. Si n = 2, por ejemplo,
#sustituiremos la "a" por la "c", la "c" por la "e", y así sucesivamente. El problema que
#aparece en las últimas n letras del alfabeto tiene fácil solución: en el ejemplo, la letra
#"y" se sustituirá por la "a" y la letra "z" por la "b". La sustitución debe aplicarse a las
#letras y a los dígitos (el 0 se sustituye con el 2, el 9 con el 1,..).
#Diseña un programa que lea un vector de letra / número y el valor de
#"n" y muestre su versión criptográfiada.

obtener_numero_letra <- function(x){
  for (i in 1:26){
    if (x == letters[i]) return(i)
  }
  return(0)
}

criptografiar <- function(x, n){
  w = c()
  for (i in 1:length(x)){
    if(is.numeric(x[i])){
      w = append(w, x[i] + n)
    } else{
      if (obtener_numero_letra(x[i]) > 26 - n){
        w = append(w, letters[obtener_numero_letra(x[i]) - 26 + n])
      } else{
        w = append(w, letters[obtener_numero_letra(x[i]) + n])
      }
    }
  }
  
  return(w)
}

criptografiar(c("h", "o", "l", "v"), 4)

#Ej 17
#Define una función que devuelva el número de días que tiene un año determinado.
#Ten en cuenta que un año es bisiesto si es divisible por 4 y no divisible por 100,
#excepto si es también divisible por 400, en cuyo caso es bisiesto.
#Ejemplos: El número de días de 2002 es 365: el número 2002 no es divisible por 4, así
#que no es bisiesto. El año 2004 es bisiesto y tiene 366 días: el número 2004 es
#divisible por 4, pero no por 100, así que es bisiesto. El año 1900 es divisible por 4,
#pero no es bisiesto porque es divisible por 100 y no por 400. El año 2000 si es
#bisiesto: el número 2000 es divisible por 4 y, aunque es divisible por 100, también lo
#es por 400).

es_bisiesto <- function(año){
  if (año %% 4 != 0 || (año %% 100 == 0 && año %% 400 != 0)) return(FALSE)
  return(TRUE)
}

dias_año = function(año){
  if (es_bisiesto(año)){
    return(366)
  } else{
    return(365)
  }
}

dias_año(2000)

#Ej 18
#Diseña una función que, dada una lista de números enteros, devuelva el número de
#"series" que hay en ella. Llamamos "serie" a todo tramo de la lista con valores
#idénticos.
#Por ejemplo, el vector [1, 1, 8, 8, 8, 8, 0, 0, 0, 2, 10, 10] tiene 5 "series" (tener en
#cuenta que el 2 forma parte de una "serie" de un solo elemento).

devolver_series <- function(v){
  numSeries = 1
  for (i in 2:length(v)){
    if (v[i] != v[i-1]){
      numSeries = numSeries + 1
    }
  }
  return(numSeries)
}

devolver_series(c(1, 1, 8, 8, 8, 8, 0, 0, 0, 2, 10, 10, 1, 1, 0))

#Ej 19
#Primera Parte
# Se desea verificar si el generador de números aleatorios de R funciona bien. Para
# probarlo se debe escribir una función prob(n,k). La función debe generar con el
# comando runif() un vector de n números enteros 1 y 10 (redondee para arriba). Con
# ese vector se verificará que la frecuencia de aparición de números x <= k sea la que
# indica la distribución uniforme.
# Ejemplo:
#   prob(100000,5) aprox. 0.5


prob = function(n, k){
  x = ceiling(runif(n, 1, 10))
  cant_apariciones = 0
  for (i in 1:length(x)){
    if (x[i] <= k){
      cant_apariciones = cant_apariciones + 1
    }
  }
  
  return(cant_apariciones / n)
}
prob(100000,5)

#Segunda Parte
# Idem anterior pera ahora se desea verificar el comando rchisq. Para probarlo se debe
# escribir una función prob2(n,gl,k).
# El vector v será el que surja de la aplicación directa del comando para "gl" grados de
# libertad. Con ese vector se verificará que la frecuencia de aparición de números x <=
#   k sea la que indica la distribución chi cuadrado con gl grados de libertad.
# Ejemplo:
#   prob2(100000,5,7) aprox 0.7793597

prob2 = function(n, gl, k){
  v = ceiling(rchisq(n, gl))
  cant_apariciones = 0
  for (i in 1:length(v)){
    if (v[i] <= k){
      cant_apariciones = cant_apariciones + 1
    }
  }
  
  return(cant_apariciones / n)
}
prob2(100000,5,7)

#Ej 20
#Primera Parte
# Se pide escribir una función "bin_to_int(b)" que transforme un número binario "b" (de
# base 2) en un número entero.
# Recuerde que así como un número con base decimal tiene unidades, decenas,
# centenas, etc. que son las potencias de 10, el número con base 2 utiliza la potencias de
# 2. Así el número "1 1 0 1" indica (de derecha a izquierda)
# 1 * 2^0 + 0 * 2^1 + 1 * 2^2 +1 * 2^3 = 1*0+0*2+1*4+1*8 = 1 + 0 + 4 + 8 = 13
# Para simplificar el número binario vendrá indicado como un vector con un elemento
# por número.
# Ejemplo:
#   b=c(1,1,0,1) bin_to_int(b)=13
# b=c(1,0,1,0,0,1,1) bin_to_int(b)=83
# b=c(1,1,0,1,1,0,1) bin_to_int(b)=109


bin_to_int <- function(b){
  v = c()
  for (i in length(b):1){
    v = append(v, b[i]*2^(length(b) - i))
  }
  
  return(sum(v))
}

b=c(1,1,0,1,1,1,1) 
bin_to_int(b)
#Segunda Parte
# Idem anterior pero ahora la función se llamará "int_to_bin(n)" y deberá transformar
# un número entero positivo en un número binario. Se adjunta el link para un video
# explicativo de la metodología, que usted deberá programar en su función
# https://www.youtube.com/watch?v=-4rUKlNeCEs
# Ejemplos: Los mismos de la parte I pero invertidos
# b=c(1,1,0,1) bin_to_int(b)=13
# b=c(1,0,1,0,0,1,1) bin_to_int(b)=83
# b=c(1,1,0,1,1,0,1) bin_to_int(b)=109


int_to_bin = function(n){
  bin = c()
  while(n != 1){
    if (n %% 2 == 0){
      bin = append(bin, 0)
    } else{
      bin = append(bin, 1)
    }
    n = floor(n/2)
  }
  bin = c(bin, 1)
  
  return(rev(bin))
}

int_to_bin(109)


#Ej 21
# En el programa R, para eliminar un elemento se pone un signo menos en la posición
# del elemento a eliminar. Por ejemplo v = [3 2 2 14] w = v[-c(3)] = [3 2 14]
# Dado un vector de números enteros positivos v (dato) eliminar los elementos
# repetidos generando un nuevo vector w.
# Note que cada vez que se elimina un elemento la longitud de vector cambia, por lo
# tanto se sugiere reemplazar en un primer paso todos los repetidos por 0 y luego
# eliminar los 0s.
# v = [3 2 2 14 3] w = [3 2 14]

contiene_elemento = function(v, x){
  if (length(v) == 0) return(FALSE)
  for (i in 1:length(v)){
    if (v[i] == x) return(TRUE)
  }
  
  return(FALSE)
}

eliminar_repetidos = function(v){
  w = c()
  for (i in 1:length(v)){
    if (!contiene_elemento(w, v[i])){
      w = c(w, v[i])
    }
  }
  
  return(w)
}
v = c(3, 2, 2, 14, 3)
eliminar_repetidos(v)

#Ej 22
# Escriba un programa "cercano" cuyo argumento sea una matriz de n filas y 2
# columnas que representan coordenadas x e y en el plano cartesiano. El otro argumento
# del programa será un par ordenado v representando las coordenadas de un punto en el
# plano.
# El programa debe detectar cuál de los puntos de la matriz está más cercano al punto v.
# Para calcular las distancias recuerde la fórmula: puntos=(x1, y1) y (x2, y2)
# distancia=RAIZ CUADRADA[(x1 − x2)^2 + (y1 − y2)^2].
# Para encontrar la mínima distancia de las calculadas pruebe los comandos min() y
# which.min()
# La salida debe ser un vector que contenga el j indicando la fila en la matriz A del
# punto más cercano, los valores x e y del punto y la distancia de dicho punto al punto
# dato v.

cercano <- function(A, v){
  x1 = v[1]
  y1 = v[1]
  w = c(1, A[1,1], A[1,2], sqrt((x1 - A[1, 1])^2 + (y1 - A[1, 2])^2))
  
  for (i in 1:nrow(A)){
    distancia = sqrt((x1 - A[i, 1])^2 + (y1 - A[i, 2])^2)
    if (distancia < w[4]){
      w = c(i, A[i, 1], A[i, 2], distancia)
    }
  }
  
  return(w)
}
M = matrix(c(1:16), ncol = 2) ; m
cercano(M, c(3,3))


#Ej 23
# Escriba un programa "espejo" que genere a partir de una matriz cualquiera a otra
# matriz cuyos elementos sean una imagen a espejo de la original. La imagen a espejo
# será respecto de un eje vertical si el argumento eje = 1 y respecto de un eje horizontal
# si el argumento eje = 2. Si encuentra un comando para hacerlo, no lo use.
revertir = function(x){
  v = c()
  contador = length(x)
  while (contador >= 1){
    v = c(v, x[contador])
    contador = contador - 1
  }
  
  return(v)
}

espejo <- function(A, eje=1){
  B = A
  if (eje == 1){
    for (i in 1:ncol(A)){
      B[i, ] = revertir(A[i, ])
    }
  } else{
    for (i in 1:nrow(A)){
      B[, i] = revertir(A[, i])
    }
  }

  return(B)
}
M = matrix(c(1:9), 3); M
espejo(M, 2)

#Ej 24
# Escribir un programa "diagonales" que extraiga de una matriz A cuadrada cualquiera
# los vectores formados por sus diagonales secundarias y por la contradiagonal. Si
# encuentra un comando para hacerlo, no lo use.

diagonales = function(A){
  v = c()
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      if (i == ncol(A)-j+1){
        v = c(v, A[i, j])
      }
    }
  }
  
  return(v)
}

M = matrix(c(1:9), 3, byrow = TRUE); M
diagonales(M) #Devuelve la contradiagonal porque no se que son las secundarias..

#Ej 25
# Escribir un programa "índices" que extraiga de una matriz todos los elementos que
# tengan la suma de sus dos índices (de fila y columna) múltiplos de a. El reporte
# deberá ser un vector indicando fila y columna para cada valor extraído.

es_multiplo = function(a, b){
  return(a%%b == 0)
}

indices = function(A, a){
  v = c()
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      if (es_multiplo(i+j, a)){
        v = c(v, "Fila: ", i, "Columna: ", j)
      }
    }
  }
  
  return(v)
}

indices(M, 3)

#Ej 26
# Escribir un programa "intervalo" que tenga como argumento una matriz A de enteros
# positivos de dimensión cualquiera. El programa deberá encontrar los elementos de la
# matriz que son mayores e iguales que un número a y menores o iguales que un
# número b, ambos argumentos del mismo. El reporte deberá ser un vector indicando
# para cada valor encontrado, fila y columna. Si hay valores repetidos incluir todos.

esta_en_rango <- function(x,a,b){
  return(x >= a && x <= b)
}

intervalo = function(A, a, b){
  v = c()
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      if (esta_en_rango(A[i, j], a, b)){
        v = c(v, "Fila: ", i, "Columna: ", j)
      }
    }
  }
  return(v)
}

intervalo(M, 3, 8)

#Ej 27
# Primera Parte
# Se pide escribir una función "multiplo(b,k)" que verifique si un número x y los
# números que surgen del mismo son múltiplos de un número "k" dado (entero
# positivo).
# Por números que surgen de x se entiende los números que se forman tomando los
# dígitos de x en forma secuencial y acumulativa de izquierda a derecha.
# Por ejemplo del número 3948743 surgen los números: 3, 39, 394, 3948, 39487,
# 394874 y 3948743
# Para encontrar dichos números hay 2 alternativas:
# Dividir x por las potencias crecientes de x comenzando de la potencia 0 y tomar el
# redondeo del resultado hacia abajo
# floor(3948743/1) = 3948743
# floor(3948743/10) = 394874
# floor(3948743/100) = 39487
# ...
# floor(3948743/1000000) = 3
# floor(3948743/10000000) = 0 - > este no se usa
# Otra posibilidad es ingresar el número como vector de dígitos
# x = c(3, 9, 4, 8, 7, 4, 3)
# Ejemplo:
#  multiplo(3948743,2)= c(394874, 3948, 394)

vector_a_num <- function(x){
  sum = 0
  for (i in 1:length(x)){
    sum = sum + (x[i]*10^(length(x)-i))
  }
  
  return(sum)
}

obtener_cant_decimales <- function(x){
  return(floor(log10(x)+1))
}

multiplo <- function(x, k){
  v = c()
  if (length(x) > 1){
    x = vector_a_num(x)
  }
  decimales = obtener_cant_decimales(x)
  for (i in 1:decimales){
    num = floor(x/(1*10^(i-1)))
    if (num %% k == 0){
      v = append(v, num)
    }
  }
  return(v)
}

multiplo(c(3948743),4)


#Segunda Parte
# Idem anterior pero ahora la función se llamará "multiplo2(b,k)" y deberá considerar
# como números que surgen de x a los números que se forman tomando los dígitos de x
# en forma secuencial y acumulativa de izquierda a derecha y de derecha a izquierda.
# Por ejemplo para el número 3948743 surgen los números:
#  3, 39, 394, 3948, 39487, 394874, 3948743, 3, 43, 743, 8743, 48743, 948743, 3948743
# (no importa si aparecen repetidos)
# Ejemplos:
#  multiplo2(3948743,4)= c(3948)
vector_a_num <- function(x){
  sum = 0
  for (i in 1:length(x)){
    sum = sum + (x[i]*10^(length(x)-i))
  }
  
  return(sum)
}

obtener_cant_cifras <- function(x){
  cant_cifras = 1
  while (x >= 10){
    x = x/10
    cant_cifras = cant_cifras + 1
  }
  
  return(cant_cifras)
}
num_a_vector <- function(x){
  v = c()
  cifras = obtener_cant_cifras(x)
  i = cifras
  while (i >= 1){
    num = floor(x/(10^(i-1)))
    if (length(v) == 0){
      v = append(v, num)
      i = i - 1
      next
    }
    for (j in 1:length(v)){
      num = num - (v[j] * 10^(length(v)-j+1))
    }
    v = append(v, num)
    i = i - 1
  }
  
  
  return(v)
}

multiplo2 <- function(x, k){
  v = c()
  if (length(x) == 1){
    x = num_a_vector(x)
  }
  for (i in 1:length(x)){
    y = c()
    for (j in 1:i){
      y = append(y, c(x[j]))
    }
    num = vector_a_num(y)
    if (num %% k == 0){
      v = append(v, num)
    }
  }
  
  i = length(x)
  while (i >= 1){
    y = c()
    for (j in i:length(x)){
      y = append(y, c(x[j]))
    }
    num = vector_a_num(y)
    if (num %% k == 0){
      v = append(v, num)
    }
    i = i-1
  }
  
  return(v)
}

multiplo2(c(3948733),3)










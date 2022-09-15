# Ej 1
x = rnorm(1, 4, sqrt(5)) ; x
if (x < 4){
  print("inferior")
} else{
  if (x>4){
    print("superior")
  } else{
    print("iguales")
  }
}

# Ej 2
set.seed(10)
v = round(rnorm(10, 4, sqrt(5)), 2) ; v
media = mean(v) ; media
for (i in 1:length(v)){
  if (v[i] < media){
    v[i] = 0
  } else{
    v[i] = 1
  }
}
v

#Ej 3
set.seed(10)
v = round(rnorm(10, 4, sqrt(5)), 2) ; v
media = mean(v) ; media
w = c()
for (i in 1:length(v)){
  if (v[i] < media){
    w[i] = "I" 
  } else{
    w<-append(w,"S") x #Es lo mismo de cualquiera de las 2 formas
  }
}
w

# Ej 4
v = rbinom(17, 77, 0.368) ; v
suma = sum(v)
if (suma > 481.712){
  print(2)
} else if (suma < 481.712){
    print(0)
  } else{
    print(1)
  }

#Ej 5
A = matrix(rnorm(3*4, 37, 9), 3, 4) ; A
B = matrix(1, 3, 4)
for (i in 1:nrow(A)){
  for (j in 1: ncol(A)){
    if (A[i, j] >= 35){
      B[i, j] = 1
    } else{
      B[i, j] = 0
    }
  }
}
B

# Ej 6
Generar una matriz de 20x9 con números aleatorios redondeados generados con
distribución exponencial de parámetro 𝜆=0.007. Analizar si la suma de los
componentes de cada columna es mayor a 1800 y expresar el resultado en un nuevo
vector (de 0s y 1s). Realizar la misma operación, pero analizando si la suma de cada
fila es mayor a 9000.

A = matrix(round(rexp(20*9, 0.007)), 20, 9) ; A
v = c()
for (i in 1:ncol(A)){
  sumaCol = 0;
  for (j in 1:nrow(A)){
    sumaCol = sumaCol + A[j, i]
  }
  if (sumaCol > 1800){
    v = append(v, 1)
  } else{
    v = append(v, 0)
  }
}
v
x = c()
for (i in 1:nrow(A)){
  sumaFila = 0
  for (j in 1:ncol(A)){
    sumaFila = sumaFila + A[i, j]
  }
  if (sumaFila > 900){
    x = append(x, 1)
  } else{
    x = append(x, 0)
  }
}
x

#Ej 7
k = runif(1, 76, 245)
suma = 0
cantIteraciones = 0

while(suma < k){
  suma = suma + runif(1,)
  cantIteraciones = cantIteraciones+1
}
print(cantIteraciones)

#Ej 8
k = runif(1, 76, 245)
suma = 0
cantIteraciones = 0

while(suma < k){
  valor = runif(1)
  if (valor <= 0.7 && valor >= 0.5){
    valor = 0.45
  } else if(valor < 0.5){
    valor = 0
  }
  suma = suma + valor
  cantIteraciones = cantIteraciones+1
}
print(cantIteraciones)















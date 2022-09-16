# Ejercicio 1
# Dada una matriz cuadrada "A" de orden "n" dato cualquiera, cuyos elementos son 
# todos cero y un vector "v" de longitud (n^2-n), programar una función 
# "relleno(n,v) que rellene FILA POR FILA la matriz "A" con los elementos de "v", 
# salvo en la diagonal principal, que seguirá teniendo elementos iguales a cero.
# La salida será la matriz A así modificada

#Ejemplo:
#v=c(1,2,3,4,5,6)
#relleno(3,v)
#A=
#[1,]    0    1    2
#[2,]    3    0    4
#[3,]    5    6    0


relleno = function(n, v){
  if (length(v) != n^2-n) return("Longitud erronea...")
  A = matrix(0, n, n)
  for (i in 1:n){
    for (j in 1:n){
      if (i!=j){
        A[i,j] = v[1]
        v = v[-1]
      }
    }
  }
  
  return(A)
}
v=c(1,2,3,4,5,6)
relleno(3,v)

# Ejercicio 2
# Programar una función "letras" que, dado un vector "v" cuyos elementos son 
# letras del abecedario, detecte cuáles letras son vocales y cuáles no. 
# La función deberá generar un nuevo vector r de la misma longitud, cuyos 
# elementos sean las letras "v" y "c", coincidiendo con la ubicación de las vocales y consonantes. 
# También deberá determinar la cantidad de consonantes y vocales en el vector 
# original e imprimir dicho conteo en la pantalla.

#Ejemplo:
#  v = c("a", "f" , "n" , "u", "d" )
#letras(v) 
#r = ["v", "c" , "c" , "v", "c"]

#vocales consonantes 
#2           3

contiene_elemento = function(v, x){
  for (i in 1:length(v)){
      if (v[i] == x) return(TRUE)
  }
  
  return(FALSE)
}

letras = function(v){
  r = v
  vocales = c("a","e","i","o","u")
  cant_vocales = 0
  for (i in 1:length(v)){
    if (contiene_elemento(vocales, v[i])){
      r[i] = "v"
      cant_vocales = cant_vocales + 1
    } else{
      r[i] = "c"
    }
  }
  cat("Tiene ", cant_vocales, " vocales y ", length(v)-cant_vocales, " consonantes")
  
  return(r)
}


v = c("a", "f" , "n" , "u", "d" )
letras(v) 
r = ["v", "c" , "c" , "v", "c"]











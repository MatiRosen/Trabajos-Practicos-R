# Ejercicio 1
# Usted trabaja para una consultora económica. Su jefe le pide escribir un programa
# "inflacion_acumulada" que tenga como argumento una matriz A de dimensión 4x3; donde las filas
# representan los 4 trimestres del año y las columnas los respectivos meses de cada trimestre.
# Cada elemento de la matriz indicará (en porcentaje) la inflación del respectivo mes.
# Reportar la inflación acumulada en el año, en porcentaje

# AYUDA: Recuerde que la inflación acumulada se calcula como:
# Inflacion.acumulada = (1 + Inflación Enero en porcentaje/100)*(1 + Inflación Febrero en
# porcentaje /100)*(1 + Inflación Marzo en porcentaje/100)*...*(1 + Inflación Diciembre en
# porcentaje/100)

# Ejemplo:
# Si la matriz de datos de la inflación mensual es:
#     [,1] [,2] [,3]
#[1,]    5    4    3
#[2,]    6    6    4
#[3,]    5    4    3
#[4,]    5    6    3

# La inflación acumulada sera del 69.47
inflacion_acumulada = function(A){
  acum = 1
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      acum = acum * (1 +(A[i,j]/100))
    }
  }
  
  return((acum-1)*100)
}

A = matrix(c(5,6,5,5,4,6,4,6,3,4,3,3), 4, 3) ; A
inflacion_acumulada(A)


# Ejercicio 2
# Idem anterior pero reportar una matriz de salida donde cada elemento sea la inflación acumulada hasta
# dicho mes. Si la matriz de datos mensuales de inflación es la del Ejercicio 1, la matriz pedida será:
#         [,1]     [,2]     [,3]
#[1,] 1.050000 1.092000 1.124760
#[2,] 1.192246 1.263780 1.314332
#[3,] 1.380048 1.435250 1.478308
#[4,] 1.552223 1.645356 1.694717

inflacion_mes_a_mes = function(A){
  acum = 1
  R = A
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      acum = acum * (1 +(A[i,j]/100))
      R[i,j] = acum
    }
  }
  
  return(R)
}

A = matrix(c(5,6,5,5,4,6,4,6,3,4,3,3), 4, 3) ; A
inflacion_mes_a_mes(A)
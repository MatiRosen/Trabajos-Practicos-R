#-------------------------------------------------------------------------------
# Ejercicio 1) 
# Genere 3 vectores aleatorios de 10 componentes x, y, z. Use la función RUNIF(n) 
x = c(runif(10)) ; x
y = c(runif(10)) ; y
z = c(runif(10)) ; z

#-------------------------------------------------------------------------------
# Ejercicio 2)
# Genere 3 vectores aleatorios x, y, z de 10 componentes, pero con distribución
# U(-100,100) usando RUNIF(n). Vea luego RUNIF(n, mín, máx)


# Uso set.seed(10) para comprobar que los numeros en ambos casos sean iguales:
# Caso 1: 
set.seed(10)
x = c(runif(10)*200-100) ; x
y = c(runif(10)*200-100) ; y
z = c(runif(10)*200-100) ; z

# Caso 2:
# Utilizo runif(n, min, max) y observo si obtengo el mismo resultado. 
set.seed(10)
x = c(runif(10, -100, 100)) ; x
y = c(runif(10, -100, 100)) ; y
z = c(runif(10, -100, 100)) ; z

#-------------------------------------------------------------------------------
# Ejercicio 3)
# Realice las siguientes operaciones con los vectores obtenidos en el caso anterior:

#   a) u= x+ y+ z
u = x + y + z ; u
#                     10
#   b)  k = <x, y> =  ∑ x𝑖 𝑦𝑖
#                    𝑖=1 
#   (Producto escalar, tendrá que transponer uno de los vectores)

k = x*t(y) ; k

#   c) v =𝑥/ ‖𝑥‖
#   (Vector unitario en la dirección de X, use NORM(vea help) y AS.MATRIX)
v = x/(norm(as.matrix(x), "1")); v

#                 10 
#   d) 𝑤 =|𝑥| / ∑ 𝑥𝑖
#                𝑖=1
#     (Vector de probabilidad a partir de X, use ABS y SUM)

w = abs(x)/ sum(x) ; w
#-------------------------------------------------------------------------------
# Ejercicio 4)          n
# Calcule 𝑥𝑚 = (1/n)* ∑𝑥𝑖
#                      𝑖=1
xm = sum(x)/length(x); xm
mean(x)
#-------------------------------------------------------------------------------
# Ejercicio 5)
# Calcule el vector dx de desviaciones de 𝑥 en el cual cada componente dx = 𝑥𝑖−𝑥𝑚.
# Puede generar un vector de unos y multiplicarlo por 𝑥𝑚 y luego restarlo de𝑥. 
# Utilice el cálculo del ejercicio anterior para 𝑥𝑚 y intente incluirlo en una sola línea. (Explore
# la función MATRIX para vectores y matrices). Verifique que la suma de los componentes de dx es nula.

dx = x-c(rep(1, length(x)))*xm ;dx
sum(dx)
#-------------------------------------------------------------------------------
# Ejercicio 6)                                                    𝑛
# Calcule la varianza de los elementos de x como Var(x) = (1/n-1)* ∑ (𝑥𝑖−𝑥𝑚)^2
#                                                                 𝑖=1

#Trabaje con vectores y para la sumatoria use SUM. Recuerde que para obtener un
#vector cuyas componentes sean el cuadrado de las componentes originales debe
#plantear u = v ^ 2. Verifique con COV.

varX= sum(dx^2) / (length(x)-1) ; varX
var(x)
#-------------------------------------------------------------------------------
# Ejercicio 7)
# Calcule la covarianza de las componentes de los vectores x e y con la fórmula:
#                      𝑛
# Cov(x, y) = (1/n-1)* ∑(𝑥𝑖−𝑥𝑚)(𝑦𝑖−𝑦𝑚)
#                     𝑖=1
# Trabaje con los vectores dx y dy y use SUM. Verifique con COV.
ym = sum(y)/length(y) ; ym
dy = y-c(rep(1, length(y)))*ym ; dy
cov = sum(dx * dy) / (length(x) -1) ; cov
cov(x, y)
#-------------------------------------------------------------------------------        
# Ejercicio 8)
# Calcula Var(x+y) y verifique que Var(x+y) = Var(x) + Var(y) + 2 Cov (x,y). Utilice
# las soluciones a los ejercicios anteriores para la verificación. Trabaje con vectores
# como lo viene haciendo.
varY= sum(dy^2) / (length(y)-1) ; varY
var(y)
varXMasY = varX + varY + 2* cov; varXMasY
var(x+y)
#-------------------------------------------------------------------------------
# Ejercicio 9)
# Genere 3 matrices con componentes aleatorios enteros U (-100,100). Utilice redondeo.
# Dimensiones: A: 5x3 B: 4x5 C: 5x5

A = matrix(round(runif(3*5, -100, 100)), 5, 3); A
B = matrix(round(runif(4*5, -100, 100)), 4, 5); B
C = matrix(round(runif(5*5, -100, 100)), 5, 5); C
#-------------------------------------------------------------------------------
# Ejercicio 10)
#  Calcule: a) C*A b) B*C*A c) A^T*C
#   a)
C%*%A
#   b)
B%*%C%*%A
#   c)
t(A)%*%C
#-------------------------------------------------------------------------------
# Ejercicio 11)
# a) Verifique que [C*A]^t = A^t*C^t (Reste [C*A]^t-A^t*C^t y verifique que el resultado
# sea la matriz nula) b) Ídem para [B*C*A]^t = A^t*C^t*B^t
#   a)
D = t(C%*%A)
E = t(A)%*%t(C)
D == E
D - E

#   b)
D = t(B%*%C%*%A)
E = t(A)%*%t(C)%*%t(B)
D == E
D - E
#-------------------------------------------------------------------------------
# Ejercicio 12)
# a) Verifique que 𝐴*𝐴^t es simétrica (Debe ser 𝐴*𝐴^t = [A*A^t]^t
# b) Ídem para 𝐴^t*A

#   a)
D = A%*%t(A)
D == t(D)

#   b)
E = t(A)%*%A
E == t(E)
#-------------------------------------------------------------------------------
# Ejercicio 13) 
# Calcule los autovalores y los autovectores de 𝐶,𝐴*𝐴^t 𝑦𝐴^t*A
# (Explore la función EIGEN con help)
eigen(C)
eigen(D)
eigen(E)
#-------------------------------------------------------------------------------
# Ejercicio 14)
# Calcule la matriz inversa de C y verifique que 𝐶^(−1)*𝐶 = 1

# Primero verificamos si tiene determinante.
det(C)
solve(C)
round(solve(C)%*%C)
#-------------------------------------------------------------------------------
# Ejercicio 15) 
# Genere una matriz identidad de 5x5, una matriz de 6x6 con todos sus componentes
# iguales a 1, una matriz nula de 4x4, y una matriz diagonal con sus elementos no nulos
# iguales a las componentes de un vector x dado. (Explore MATRIX Y DIAG)


# Para generar una matriz identidad puedo usar diag, o puedo multiplicar 
# cualquier matriz por su inversa...
identidad = diag(5) ; identidad
identi2 = round(solve(C)%*%C); identi2
todo1 = matrix(rep(1, 6*6), 6) ; todo1
nula = matrix(rep(0, 4*4), 4) ; nula
x= seq(10, 100, 25) ; x
diag(x, length(x))




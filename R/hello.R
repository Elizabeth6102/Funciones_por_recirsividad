# Multiplicación por sumas sucesivas
sum_suc_rec <- function(b, a) {
  if (b == 0) {
    return(0)
  } else {
    return(a + sum_suc_rec(b - 1, a))
  }
}

# Potencia por multiplicaciones sucesivas
potencia_suc_rec <- function(base, exponente) {
  if (exponente == 0) {
    return(1)
  } else {
    return(sum_suc_rec(base, potencia_suc_rec(base, exponente - 1)))
  }
}

# Ejemplo de uso
b<-potencia_suc_rec(2, 3)
b

####################################################################################
#Multiplicacion de 2 numeros por sumas sucesivas
##Funcion Sin recursividad
sum_suce<-function(b,a){
  suma1<-0
  i<-1
  while (i<=b) {
    suma1<- suma1 +a
    i <-i+1
  }
  return(suma1)
}

sum_suce1<-function(b,a){
  suma1<-0
  i<-1
  while (i<=a) {
    suma1<- suma1 +b
    i <-i+1
  }
  return(suma1)
}

#Funcion de recurividad
recursiva_suma<- function(a,b){
  if(a==0){
    return(0)
  }else{
    return(b+recursiva_suma(a-1,b))
  }
}
recursiva_suma<- function(a,b){
  if(b==0){
    return(0)
  }else{
    return(a+recursiva_suma(a,b-1))
  }
}

cat("Ingreseun numero:\n")
a<- readLines(n=1)
a<- as.numeric(a)

cat("Ingrese otro numero:\n")
b<-readLines(n=1)
b<- as.numeric(b)


c<-sum_suce(b,a)
c1<-sum_suce1(b,a)
cat("La multiplicacion M1 es: ",c)
cat("\nLa multiplicacion M2 es: ",c1)


##################################################################################
# Fourth level recursive helper function
sumar_aux <- function(x, y, accum) {
  if (y == 0) {
    return(accum)
  } else {
    return(sumar_aux(x, y - 1, accum + x))
  }
}

# Third level recursive helper function
sumar_rec <- function(x, y) {
  return(sumar_aux(x, y, 0))
}

# Second level recursive helper function
sumar <- function(x, y) {
  return(sumar_rec(x, y))
}

# Main recursive factorial function
factorial_con_sumas_rec <- function(n) {
  if (n <= 1) {
    return(1)
  } else {
    return(sumar(factorial_con_sumas_rec(n - 1), n))
  }
}
numero <- as.integer(readline(prompt = "Ingresa un número para calcular su factorial: "))

resultado <- factorial_con_sumas_rec(numero)

cat("El factorial de", numero, "es:", resultado, "\n")

###################################################################################
#Recursividad
fact1 <- function(N){
  f<-1
  i = 1
  while ( i <= N) {
    f<- f*i
    i <- i+1
  }
  return(f)
}

f_recursiva <- function(N){
  if(N==0){
    return(1)
  }else{
    return(N*f_recursiva(N-1))
  }
}

cat("Ingrese un numero:\n")
N<- readLines(n=1)
N<- as.numeric(N)

f2<- fact1(N)
cat("El factorial de la variable",N,"es: ", f2)

f3 <- f_recursiva(N)
cat("\nCon recursividad es", f3)

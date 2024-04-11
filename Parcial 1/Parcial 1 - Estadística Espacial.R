# Parcial 1 - Estadística Espacial #####

# Integrantes:
# - John Anderson Guarin Lopez
# - German Camilo Vasquez Herrera

# Librerías Utilizadas ####

rm(list=ls())
if (!require(geoR)){install.packages("geoR");library(geoR)}
if (!require(gstat)){install.packages("gstat");library(gstat)}
if (!require(MASS)){install.packages("MASS");library(MASS)}
if (!require(scatterplot3d)){install.packages("scatterplot3d");library(scatterplot3d)}

# Punto 1 ####

## Gráfico de la tendencia de acuerdo al modelo ####

beta_0 <- 4
beta_1 <- 5
beta_2 <- 2

tendencia <- function(x, y){
  4 + 5*x + 2*y
}

## Preparar variables ####
x <- seq(0, 9, length = 10)
y <- seq(0, 9, length = 10)
z <- outer(x, y, tendencia)
persp(x, y, z, col=4, main = "Gráfico de la Función de Tendencia")

## Definición del modelo de covarianza ####

grid<-expand.grid(seq(0,9),seq(0,9))
plot(grid)
distancia<-dist(grid,diag=T,upper=T)
distancia<-as.matrix(distancia)
sig2 <- 3
phi <- 2
covarianza<-sig2*exp((-distancia)/phi) # Modelo de covarianza exponencial
covarianza<-as.matrix(covarianza)
plot(distancia,covarianza,pch=16, main="Modelo de covarianza exponencial")

## Simulación del modelo exponencial, usando descomposición de Cholesky ####

set.seed(17112000)
medias.ncte <- beta_0+beta_1*grid[,1]+beta_2*grid[,2]
covar_chol <- chol(covarianza)
normal_sample <- matrix(rnorm(100), nrow = 100)
normal.ncte<-covar_chol%*%normal_sample
dim(covarianza)
length (normal.ncte)
var(normal.ncte-medias.ncte)
datosncte<-cbind(grid[,1],grid[,2], normal.ncte)
datosncte<-as.geodata(datosncte, coords=1:2, var=3) # 100 datos (uno para cada coordenada)
plot(datosncte, scatter3d=TRUE)

## Estimación del variograma de datos no estacionarios ####

### Cálculo de los residuales ####
modelo<-lm(normal.ncte~grid[,1]+grid[,2])
modelo
beta0_est<-modelo$coefficients[[1]]
beta1_est<-modelo$coefficients[[2]]
beta2_est<-modelo$coefficients[[3]]
residuales<-normal.ncte-(beta0_est+beta1_est*grid[,1]+beta2_est*grid[,2])
residuales_coord<-as.matrix(cbind(x_coord=grid[,1],y_coord=grid[,2], residuales))
residuales<-as.geodata(residuales_coord, coords=1:2, var=3)
 
## Estimación del modelo de variograma (con los residuales) ####
variog_residuales  <- variog(residuales, option = "bin", max.dist = 10)
plot(variog_residuales )
ini.vals <- expand.grid(seq(2,5,l=5), seq(5,10,l=5))
modelo_residuales <- variofit(variog_residuales, ini=ini.vals, fix.nug=FALSE, wei="npair", min="optim")
plot(variog_residuales, xlab="Distancia (m)",ylab="Semivarianza", main = "Variograma residuales", pch=16, col=4)
lines(modelo_residuales)

## Hacer predicción por kriging universal ####

### Definir el enmallado de predicción ####

x<-c(0,9,9,0,0)
y<-c(0,0,9,9,0)
datoscte.borde<-cbind(x,y)
plot(datoscte.borde)
datosncte.grid<-expand.grid(Este=seq(0,9,l=75), Norte=seq(0,9,l=75))
plot(datosncte.grid, main = "Enmallado de Predicción")
points(grid, col=4, lwd=4, pch=16)

### Kriging Universal ####

names(modelo_residuales)
Sigsq_est<-modelo_residuales$cov.pars[1]
phi_est<-modelo_residuales$cov.pars[2]
modelo_residuales 
datosncte.kc<-krige.conv(datosncte, loc=datosncte.grid,  krige= krige.control(nugget=0,trend.d="1st", trend.l="1st",
                        cov.pars=c(Sigsq=Sigsq_est, phi_est= phi_est)))
image(datosncte.kc, main="Predicción Kriging Universal", xlab="Este", ylab="Norte")
contour(datosncte.kc,main="", add=TRUE, drawlabels=TRUE)

### Error estándar de predición ####

image(datosncte.kc, xlim=c(0,9), ylim=c(0,9),val=sqrt(datosncte.kc$krige.var), main="Error estándar de predicción", col=terrain.colors(100))
contour(datosncte.kc, val=sqrt(datosncte.kc$krige.var), main="", add=TRUE, drawlabels=TRUE)
summary(sqrt(datosncte.kc$krige.var))

# Punto 4 ####
## Caso 1: rho=0 (Caso insesgado) ####

### Asignación valores ####
n<-10 
mu<-0
sig2<-1
rho<-0
m<-100 

### Vector de medias simulación ####
media<-rep(mu,n)

### Matriz de varianzas y covarianzas simulación ####
Sig<-matrix(sig2*rho, nrow=n, ncol=n)
diag(Sig)<-sig2
var_mues<-c()
set.seed(17112000)
for(i in 1:m){
  datos<-mvrnorm(1,media,Sig)
  var_mues[i]<-var(datos)
}
mean(var_mues)

## Caso 2: rho!=0 (Caso sesgado) ####

### Asignación valores ####
n<-10
mu<-0
sig2<-1
rho<-0.5
m<-100

### Vector de medias simulación ####
media<-rep(mu,n)

### Matriz de varianzas y covarianzas simulación ####
Sig<-matrix(sig2*rho, nrow=n, ncol=n)
diag(Sig)<-sig2
var_mues<-c()
set.seed(17112000)
for(i in 1:m){
  datos<-mvrnorm(1,media,Sig)
  var_mues[i]<-var(datos)
}
mean(var_mues)
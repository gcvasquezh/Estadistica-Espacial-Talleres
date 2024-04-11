#### Ejercicio Predicción Kriging - Estadística Espacial ####

## Nombre: 

library(geoR)
library (gstat)
library(MASS)
library(ade4)

grid<-expand.grid(seq(0,2),seq(0,2))
plot(grid)

distancia<-dist(grid)
distancia<-as.matrix(distancia)

datos <- c(12,14,NA,15,NA,14,10,12,13)
coordenadas<-cbind(grid[,1],grid[,2],datos)
colnames(coordenadas)<-c("X","Y","Datos")
coordenadas <- as.geodata(coordenadas,coords=1:2,var=3)
plot(coordenadas)

variograma<-2.81*(1-exp(-distancia/2))

covariograma<-var(na.omit(coordenadas$data))-variograma

# Matriz C_ij
C.ij<-covariograma[-c(3,5),-c(3,5)]

# Vector C_i0
C.i0<-covariograma[-c(3,5),5]

# Pesos lambda
lambda<-ginv(C.ij)%*%C.i0

# Datos sin los NA's
datossin<-datos[-c(3,5)]

# Estimación
t(lambda)%*%datossin




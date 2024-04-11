<<<<<<< HEAD
#### Test de Mantel - Temperatura Bogotá 2023 ####

# Integrantes:
# John Anderson Guarin Lopez
# German Camilo Vasquez Herrera

# Estadística Espacial

# Librerías Utilizadas ----------------------------------------------------

if (!require(ade4)){install.packages("ade4");library(ade4)}
if (!require(readr)){install.packages("readr");library(readr)}
if (!require(readxl)){install.packages("readxl");library(readxl)}
if (!require(dplyr)){install.packages("dplyr");library(dplyr)}

# Fijar Directorio y Carga de Base de Datos -------------------------------
=======
# Taller Estadística Espacial - Test de Mantel
# Nombre: John Anderson Guarin Lopez - German Camilo Vasquez Herrera
if (!require(ade4)){install.packages("ade4");library(ade4)}
if (!require(readr)){install.packages("readr");library(readr)}
if (!require(rstudioapi)){install.packages("rstudioapi");library(rstudioapi)}
if (!require(tictoc)){install.packages("tictoc");library(tictoc)}
>>>>>>> 5a52a1f1c0157eaafe6fbfca703e4620de0b1201

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
temp<-read.csv("temperatura.csv", sep=",", header=T)
temp<-temp[,c(3,4,18)]

est <- read_excel("CNE_IDEAM.xls", col_names = TRUE)
est <- est[, 9:12]
est[, 1:2] <- lapply(est[, 1:2], function(x) as.numeric(as.character(x)))
est_1 <- est %>% filter(est[,3] == "Bogota")

# Verificación de Datos Faltantes -----------------------------------------

missings <- function(x) return(sum(is.na(x)))
apply(temp,2,missings)
apply(est_1,2,missings)

# Mapa de Estaciones en Bogotá --------------------------------------------

plot(x = est_1$LONGITUD, y = est_1$LATITUD, xlab = "Longitud", ylab = "Latitud",
     type = "l", col = 4, lwd = 2, main = "Estaciones en Bogotá")
points(temp[,2], temp[,1], col=2, pch=16, 
       main="Estaciones de Monitoreo de Temperatura en Bogotá",
       xlab="Longitud", ylab="Latitud")

# Test de Mantel ----------------------------------------------------------

coord_dists <- dist(cbind(temp$Longitud, temp$Latitud))
valor_dists <- dist(temp$Valor)
tictoc::tic()
res<-mantel.rtest(coord_dists, valor_dists, nrepet = 1000)
tictoc::toc()
res

plot(res, xlab="M", main="Distribución de Montecarlo Test de Mantel")

#Ya que el p-valor es tan pequeño rechazamos la hipótesis nula
#y decimos que si hay correlación espacial


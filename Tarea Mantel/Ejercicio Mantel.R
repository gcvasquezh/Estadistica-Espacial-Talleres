# Taller Estadística Espacial - Test de Mantel
# Nombre: John Anderson Guarin Lopez - German Camilo Vasquez Herrera
if (!require(ade4)){install.packages("ade4");library(ade4)}
if (!require(readr)){install.packages("readr");library(readr)}
if (!require(rstudioapi)){install.packages("rstudioapi");library(rstudioapi)}
if (!require(tictoc)){install.packages("tictoc");library(tictoc)}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
temp<-read.csv("temperatura.csv", sep=",", header=T)
temp<-temp[,c(3,4,18)]

missings <- function(x) return(sum(is.na(x)))
apply(temp,2,missings)

# Mantel test
?mantel.rtest
coord_dists <- dist(cbind(temp$Longitud, temp$Latitud))
valor_dists <- dist(temp$Valor)
tictoc::tic()
res<-mantel.rtest(coord_dists, valor_dists, nrepet = 1000)
tictoc::toc()
res
plot(res, xlab="M", main="Montecarlo distribution")

#Ya que el p-vaor es tan pequeño rechazamos la hipótesis nula
#y decimos que si hay aleatoriedad espacial


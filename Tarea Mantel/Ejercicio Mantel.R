library (ade4)
library(readr)

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


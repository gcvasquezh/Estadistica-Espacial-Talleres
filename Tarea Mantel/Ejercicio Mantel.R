library (ade4)
library(readr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
temp <- read.csv("C:\\Users\\gcvh2\\Documents\\UNAL\\8. 2024 -1S\\Estadística Espacial\\Estadistica-Espacial-Talleres\\Tarea Mantel\\temperatura.csv", sep=",", header=T)
temp


# Map of stations
LA <- read.table ("LA.txt", head=TRUE)
plot(LA$Longitude, LA$Latitude, xlab="Longitude", ylab="Latitude",
     type="l", col=4, lwd=2, xlim=c(-119, -116), main = "Stations in the Los Angeles area ")
points(ozone$Lon, ozone$Lat, col=2, pch=16, 
     main="Location of Ozone monitoring stations in Los Angeles",
     xlab="Longitude", ylab="Latitude")


# Mantel test
?mantel.rtest
station.dists <- dist(cbind(ozone$Lon, ozone$Lat))
ozone.dists <- dist(ozone$Av8top)
res=mantel.rtest(station.dists, ozone.dists, nrepet = 9999)
res
plot(res, xlab="M", main="Montecarlo distribution, P-value=0.0295")

res2=mantel.rtest(station.dists, alter=two-sided, ozone.dists, nrepet = 9999)

  

?mantel.rtest
?rtest


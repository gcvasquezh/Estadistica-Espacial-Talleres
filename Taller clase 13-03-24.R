# Taller Estadística Espacial - Geoestadística
# Nombre: John Anderson Guarin Lopez - German Camilo Vasquez Herrera


library(geoR)
library(MASS)
library(gstat)
library(readxl)

#Punto 1##############
setwd("C:/Users/gcvh2/Documents/UNAL/8. 2024 -1S/Estadística Espacial/Estadistica-Espacial-Talleres")
superficie<-read_excel("Superficie.xlsx")
fondo<-read_excel("Fondo.xlsx")
summary(superficie)
cv<-apply(superficie,2,sd)/apply(superficie,2,mean);cv

summary(fondo)
cv<-apply(fondo,2,sd)/apply(fondo,2,mean);cv

#Punto 2##############
datos_fondo<-cbind(fondo[,1],fondo[,2],fondo[,3])
datos_fondo<-as.geodata(datos_fondo, coords=1:2, var=3)
plot(datos_fondo)


variograma=variog(datos_fondo)
plot(variograma)
variograma.env=variog.mc.env(datos_fondo, obj=variograma)
plot(variograma, env=variograma.env)

?variofit

variofit(variograma, ini.cov.pars = )






gamma_exp <- variograma$gamma
gamma_modelo <- variograma$np[1] * (1 - exp(-variograma$dist/rango)) + nugget
R2_manual <- cor(gamma_exp, gamma_modelo)^2

# Paso 3: Calcular el nugget como la intersección del variograma experimental con el eje y
nugget <- variograma$ini[1]

# Paso 4: Calcular el psill como la meseta del variograma experimental
psill <- variograma$var[1]

# Paso 5: Calcular el rango como la distancia donde el variograma alcanza el 95% del psill
rango <- max(variograma$dist[which(gamma_exp <= 0.95 * psill)])


variogram_model <- fit.variogram(variograma, 
                                 vgm(psill = 5, range = 3, nugget = 1))

# Obtener los parámetros del modelo ajustado
pepita <- variogram_model$psill - variogram_model$nugget
rango <- variogram_model$range
meseta <- variogram_model$psill

# Calcular R^2
modelo_prediccion <- variogram_model$fit
R2 <- cor(variograma$gamma, modelo_prediccion)^2

# Imprimir los resultados
cat("Pepita:", pepita, "\n")
cat("Rango:", rango, "\n")
cat("Meseta:", meseta, "\n")
cat("R^2:", R2, "\n")

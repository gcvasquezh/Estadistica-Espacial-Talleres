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


# Fondo - Salinidad ####
vario_fsali<-variog(coords=Fondo[,c("X","Y")],data=Fondo$Sali,uvec=seq(0,1000,length=50),estimator.type="class")
plot(vario_fsali)

fsali_esferico<-variofit(vario_fsali,ini=c(sigmasq=varianza_fsali,phi=100),fix.nugget=FALSE,cov.model="spherical")
fsali_lineal<-variofit(vario_fsali,ini=c(sigmasq=varianza_fsali,phi=100),fix.nugget=FALSE,cov.model="linear")
fsali_exponencial<-variofit(vario_fsali,ini=c(sigmasq=varianza_fsali,phi=100),fix.nugget=FALSE,cov.model="exponential")

R2_fsali_esferico<-1-summary(fsali_esferico)$sum.of.squares/varianza_fsali
R2_fsali_lineal<-1-summary(fsali_lineal)$sum.of.squares/varianza_fsali
R2_fsali_exponencial<-1-summary(fsali_exponencial)$sum.of.squares/varianza_fsali

print(paste("R^2 del modelo esférico:", R2_fsali_esferico))
print(paste("R^2 del modelo lineal:", R2_fsali_lineal)) # Mejor de todos
print(paste("R^2 del modelo exponencial:", R2_fsali_exponencial))

fsali_pepita<-fsali_lineal$nugget;fsali_pepita
fsali_meseta<-fsali_lineal$nugget+summary(fsali_lineal)$estimated.pars[2];fsali_meseta
fsali_rango<-summary(fsali_lineal)$estimated.pars[3];fsali_rango



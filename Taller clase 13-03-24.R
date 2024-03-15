# Taller Estadística Espacial - Geoestadística
# Nombre: John Anderson Guarin Lopez - German Camilo Vasquez Herrera

library(geoR)
library(MASS)
library(gstat)
library(readxl)

#Punto 1##############
setwd("C:/Users/ander/OneDrive - Universidad Nacional de Colombia/Documentos/(2024-01) Octavo Semestre/Estadística Espacial/Estadistica-Espacial-Talleres")
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
vario_fsali<-variog(coords=fondo[,c("X","Y")],data=fondo$Sali,uvec=seq(0,1000,length=50),estimator.type="class")
plot(vario_fsali)

fsali_esferico<-variofit(vario_fsali,ini=c(sigmasq=var(fondo[,3]),phi=0),fix.nugget=FALSE,cov.model="spherical")
fsali_lineal<-variofit(vario_fsali,ini=c(sigmasq=var(fondo[,3]),phi=0),fix.nugget=FALSE,cov.model="linear")
fsali_exponencial<-variofit(vario_fsali,ini=c(sigmasq=var(fondo[,3]),phi=0),fix.nugget=FALSE,cov.model="exponential")

R2_fsali_esferico<-1-summary(fsali_esferico)$sum.of.squares/var(fondo[,3])
R2_fsali_lineal<-1-summary(fsali_lineal)$sum.of.squares/var(fondo[,3])
R2_fsali_exponencial<-1-summary(fsali_exponencial)$sum.of.squares/var(fondo[,3])

print(paste("R^2 del modelo esférico:", R2_fsali_esferico))
print(paste("R^2 del modelo lineal:", R2_fsali_lineal)) # Mejor de todos
print(paste("R^2 del modelo exponencial:", R2_fsali_exponencial))

fsali_pepita<-fsali_lineal$nugget;fsali_pepita
fsali_meseta<-fsali_lineal$nugget+summary(fsali_lineal)$estimated.pars[2];fsali_meseta
fsali_rango<-summary(fsali_lineal)$estimated.pars[3];fsali_rango

# Fondo - Oxigeno ####
vario_foxi<-variog(coords=fondo[,c("X","Y")],data=fondo$oxig,uvec=seq(0,1000,length=50),estimator.type="class")
plot(vario_foxi)

foxi_esferico<-variofit(vario_foxi,ini=c(sigmasq=var(fondo[,4]),phi=0),fix.nugget=FALSE,cov.model="spherical")
foxi_lineal<-variofit(vario_foxi,ini=c(sigmasq=var(fondo[,4]),phi=0),fix.nugget=FALSE,cov.model="linear")
foxi_exponencial<-variofit(vario_foxi,ini=c(sigmasq=var(fondo[,4]),phi=0),fix.nugget=FALSE,cov.model="exponential")

R2_foxi_esferico<-1-summary(foxi_esferico)$sum.of.squares/var(fondo[,4])
R2_foxi_lineal<-1-summary(foxi_lineal)$sum.of.squares/var(fondo[,4])
R2_foxi_exponencial<-1-summary(foxi_exponencial)$sum.of.squares/var(fondo[,4])

print(paste("R^2 del modelo esférico:", R2_foxi_esferico))
print(paste("R^2 del modelo lineal:", R2_foxi_lineal)) # Mejor de todos
print(paste("R^2 del modelo exponencial:", R2_foxi_exponencial))

foxi_pepita<-foxi_lineal$nugget;foxi_pepita
foxi_meseta<-foxi_lineal$nugget+summary(foxi_lineal)$estimated.pars[2];foxi_meseta
foxi_rango<-summary(foxi_lineal)$estimated.pars[3];foxi_rango

# Fondo - Temperatura ####
vario_ftemp<-variog(coords=fondo[,c("X","Y")],data=fondo$temp,uvec=seq(0,1000,length=50),estimator.type="class")
plot(vario_ftemp)

ftemp_esferico<-variofit(vario_ftemp,ini=c(sigmasq=var(fondo[,5]),phi=0),fix.nugget=FALSE,cov.model="spherical")
ftemp_lineal<-variofit(vario_ftemp,ini=c(sigmasq=var(fondo[,5]),phi=0),fix.nugget=FALSE,cov.model="linear")
ftemp_exponencial<-variofit(vario_ftemp,ini=c(sigmasq=var(fondo[,5]),phi=0),fix.nugget=FALSE,cov.model="exponential")

R2_ftemp_esferico<-1-summary(ftemp_esferico)$sum.of.squares/var(fondo[,5])
R2_ftemp_lineal<-1-summary(ftemp_lineal)$sum.of.squares/var(fondo[,5])
R2_ftemp_exponencial<-1-summary(ftemp_exponencial)$sum.of.squares/var(fondo[,5])

print(paste("R^2 del modelo esférico:", R2_ftemp_esferico))
print(paste("R^2 del modelo lineal:", R2_ftemp_lineal)) # Mejor de todos
print(paste("R^2 del modelo exponencial:", R2_ftemp_exponencial))

ftemp_pepita<-ftemp_lineal$nugget;ftemp_pepita
ftemp_meseta<-ftemp_lineal$nugget+summary(ftemp_lineal)$estimated.pars[2];ftemp_meseta
ftemp_rango<-summary(ftemp_lineal)$estimated.pars[3];ftemp_rango

# Fondo - PH ####
vario_fph<-variog(coords=fondo[,c("X","Y")],data=fondo$pH,uvec=seq(0,1000,length=50),estimator.type="class")
plot(vario_fph)

fph_esferico<-variofit(vario_fph,ini=c(sigmasq=var(fondo[,6]),phi=0),fix.nugget=FALSE,cov.model="spherical")
fph_lineal<-variofit(vario_fph,ini=c(sigmasq=var(fondo[,6]),phi=0),fix.nugget=FALSE,cov.model="linear")
fph_exponencial<-variofit(vario_fph,ini=c(sigmasq=var(fondo[,6]),phi=0),fix.nugget=FALSE,cov.model="exponential")

R2_fph_esferico<-1-summary(fph_esferico)$sum.of.squares/var(fondo[,6])
R2_fph_lineal<-1-summary(fph_lineal)$sum.of.squares/var(fondo[,6])
R2_fph_exponencial<-1-summary(fph_exponencial)$sum.of.squares/var(fondo[,6])

print(paste("R^2 del modelo esférico:", R2_fph_esferico))
print(paste("R^2 del modelo lineal:", R2_fph_lineal)) # Mejor de todos
print(paste("R^2 del modelo exponencial:", R2_fph_exponencial))

fph_pepita<-fph_lineal$nugget;fph_pepita
fph_meseta<-fph_lineal$nugget+summary(fph_lineal)$estimated.pars[2];fph_meseta
fph_rango<-summary(fph_lineal)$estimated.pars[3];fph_rango



















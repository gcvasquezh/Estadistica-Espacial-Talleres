# Taller Estadística Espacial - Geoestadística
# Nombre: John Anderson Guarin Lopez - German Camilo Vasquez Herrera

library(geoR)
library(MASS)
library(gstat)
library(readxl)

#Punto 1##############
setwd("C:/Users/ander/OneDrive - Universidad Nacional de Colombia/Documentos/(2024-01) Octavo Semestre/Estadística Espacial/Estadistica-Espacial-Talleres")
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

# Superficie - Salinidad ####
vario_ssali<-variog(coords=superficie[,c("X","Y")],data=superficie$salinid,uvec=seq(0,1000,length=50),estimator.type="class")
plot(vario_ssali)

ssali_esferico<-variofit(vario_ssali,ini=c(sigmasq=var(superficie[,3]),phi=0),fix.nugget=FALSE,cov.model="spherical")
ssali_lineal<-variofit(vario_ssali,ini=c(sigmasq=var(superficie[,3]),phi=0),fix.nugget=FALSE,cov.model="linear")
ssali_exponencial<-variofit(vario_ssali,ini=c(sigmasq=var(superficie[,3]),phi=0),fix.nugget=FALSE,cov.model="exponential")

R2_ssali_esferico<-1-summary(ssali_esferico)$sum.of.squares/var(superficie[,3])
R2_ssali_lineal<-1-summary(ssali_lineal)$sum.of.squares/var(superficie[,3])
R2_ssali_exponencial<-1-summary(ssali_exponencial)$sum.of.squares/var(superficie[,3])

print(paste("R^2 del modelo esférico:", R2_ssali_esferico))
print(paste("R^2 del modelo lineal:", R2_ssali_lineal)) # Mejor de todos
print(paste("R^2 del modelo exponencial:", R2_ssali_exponencial))

ssali_pepita<-ssali_lineal$nugget;ssali_pepita
ssali_meseta<-ssali_lineal$nugget+summary(ssali_lineal)$estimated.pars[2];ssali_meseta
ssali_rango<-summary(ssali_lineal)$estimated.pars[3];ssali_rango

# Superficie - Oxigeno ####
vario_soxig<-variog(coords=superficie[,c("X","Y")],data=superficie$oxigeno,uvec=seq(0,1000,length=50),estimator.type="class")
plot(vario_soxig)

soxig_esferico<-variofit(vario_soxig,ini=c(sigmasq=var(superficie[,4]),phi=0),fix.nugget=FALSE,cov.model="spherical")
soxig_lineal<-variofit(vario_soxig,ini=c(sigmasq=var(superficie[,4]),phi=0),fix.nugget=FALSE,cov.model="linear")
soxig_exponencial<-variofit(vario_soxig,ini=c(sigmasq=var(superficie[,4]),phi=0),fix.nugget=FALSE,cov.model="exponential")

R2_soxig_esferico<-1-summary(soxig_esferico)$sum.of.squares/var(superficie[,4])
R2_soxig_lineal<-1-summary(soxig_lineal)$sum.of.squares/var(superficie[,4])
R2_soxig_exponencial<-1-summary(soxig_exponencial)$sum.of.squares/var(superficie[,4])

print(paste("R^2 del modelo esférico:", R2_soxig_esferico))
print(paste("R^2 del modelo lineal:", R2_soxig_lineal)) # Mejor de todos
print(paste("R^2 del modelo exponencial:", R2_soxig_exponencial))

soxig_pepita<-soxig_lineal$nugget;soxig_pepita
soxig_meseta<-soxig_lineal$nugget+summary(soxig_lineal)$estimated.pars[2];soxig_meseta
soxig_rango<-summary(soxig_lineal)$estimated.pars[3];soxig_rango

# Superficie - Temperatura ####
vario_stemp<-variog(coords=superficie[,c("X","Y")],data=superficie$temp,uvec=seq(0,1000,length=50),estimator.type="class")
plot(vario_stemp)

stemp_esferico<-variofit(vario_stemp,ini=c(sigmasq=var(superficie[,5]),phi=0),fix.nugget=FALSE,cov.model="spherical")
stemp_lineal<-variofit(vario_stemp,ini=c(sigmasq=var(superficie[,5]),phi=0),fix.nugget=FALSE,cov.model="linear")
stemp_exponencial<-variofit(vario_stemp,ini=c(sigmasq=var(superficie[,5]),phi=0),fix.nugget=FALSE,cov.model="exponential")

R2_stemp_esferico<-1-summary(stemp_esferico)$sum.of.squares/var(superficie[,5])
R2_stemp_lineal<-1-summary(stemp_lineal)$sum.of.squares/var(superficie[,5])
R2_stemp_exponencial<-1-summary(stemp_exponencial)$sum.of.squares/var(superficie[,5])

print(paste("R^2 del modelo esférico:", R2_stemp_esferico))
print(paste("R^2 del modelo lineal:", R2_stemp_lineal)) # Mejor de todos
print(paste("R^2 del modelo exponencial:", R2_stemp_exponencial))

stemp_pepita<-stemp_esferico$nugget;stemp_pepita
stemp_meseta<-stemp_esferico$nugget+summary(stemp_esferico)$estimated.pars[2];stemp_meseta
stemp_rango<-summary(stemp_esferico)$estimated.pars[3];stemp_rango


# Superficie - Ph ####
vario_sph<-variog(coords=superficie[,c("X","Y")],data=superficie$ph,uvec=seq(0,1000,length=50),estimator.type="class")
plot(vario_sph)

sph_esferico<-variofit(vario_sph,ini=c(sigmasq=var(superficie[,6]),phi=0),fix.nugget=FALSE,cov.model="spherical")
sph_lineal<-variofit(vario_sph,ini=c(sigmasq=var(superficie[,6]),phi=0),fix.nugget=FALSE,cov.model="linear")
sph_exponencial<-variofit(vario_sph,ini=c(sigmasq=var(superficie[,6]),phi=0),fix.nugget=FALSE,cov.model="exponential")

R2_sph_esferico<-1-summary(sph_esferico)$sum.of.squares/var(superficie[,6])
R2_sph_lineal<-1-summary(sph_lineal)$sum.of.squares/var(superficie[,6])
R2_sph_exponencial<-1-summary(sph_exponencial)$sum.of.squares/var(superficie[,6])

print(paste("R^2 del modelo esférico:", R2_sph_esferico))
print(paste("R^2 del modelo lineal:", R2_sph_lineal)) # Mejor de todos
print(paste("R^2 del modelo exponencial:", R2_sph_exponencial))

sph_pepita<-sph_lineal$nugget;sph_pepita
sph_meseta<-sph_lineal$nugget+summary(sph_lineal)$estimated.pars[2];sph_meseta
sph_rango<-summary(sph_lineal)$estimated.pars[3];sph_rango

# Superficie - Sechi ####
vario_ssechi<-variog(coords=superficie[,c("X","Y")],data=superficie$secchi,uvec=seq(0,1000,length=50),estimator.type="class")
plot(vario_ssechi)

ssechi_esferico<-variofit(vario_ssechi,ini=c(sigmasq=var(superficie[,7]),phi=0),fix.nugget=FALSE,cov.model="spherical")
ssechi_lineal<-variofit(vario_ssechi,ini=c(sigmasq=var(superficie[,7]),phi=0),fix.nugget=FALSE,cov.model="linear")
ssechi_exponencial<-variofit(vario_ssechi,ini=c(sigmasq=var(superficie[,7]),phi=0),fix.nugget=FALSE,cov.model="exponential")

R2_ssechi_esferico<-1-summary(ssechi_esferico)$sum.of.squares/var(superficie[,7])
R2_ssechi_lineal<-1-summary(ssechi_lineal)$sum.of.squares/var(superficie[,7])
R2_ssechi_exponencial<-1-summary(ssechi_exponencial)$sum.of.squares/var(superficie[,7])

print(paste("R^2 del modelo esférico:", R2_ssechi_esferico))
print(paste("R^2 del modelo lineal:", R2_ssechi_lineal)) # Mejor de todos
print(paste("R^2 del modelo exponencial:", R2_ssechi_exponencial))

ssechi_pepita<-ssechi_lineal$nugget;ssechi_pepita
ssechi_meseta<-ssechi_lineal$nugget+summary(ssechi_lineal)$estimated.pars[2];ssechi_meseta
ssechi_rango<-summary(ssechi_lineal)$estimated.pars[3];ssechi_rango




















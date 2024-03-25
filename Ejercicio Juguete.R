library(geoR)
library (gstat)
library(MASS)
coordenadas<-cbind(c(1,1,1,2,2,3,3),c(1,2,3,1,3,2,3),c(12,15,10,14,NA,12,NA,14,13))
colnames(coordenadas)<-c("X","Y","Datos")
daticos<-c(12,15,10,14,12,14,13)
grid<-expand.grid(seq(0,2),seq(0,2))
plot(grid)
distancia<-dist(grid)
distancia<-as.matrix(distancia)
sigma<-var(daticos)
covarianza<-sigma-2.81*(1-exp(-distancia/2))
covarianza<-as.matrix(covarianza)
matplot(distancia,covarianza)

medias.cte<-rep(mean(daticos),9)
normal.cte<-mvrnorm(1,medias.cte,covarianza)
datoscte<-cbind(grid[,1],grid[,2],normal.cte)

x<-c(0,2,2,0,0)
y<-c(0,0,2,2,0)
datoscte.borde<-cbind(x,y)
datoscte.grid<-expand.grid(Este=seq(0,2), Norte=seq(0,2))
plot(datoscte.grid)

vario_toy<-variog(coords=coordenadas[,c("X","Y")],data=coordenadas[,3],estimator.type="class")
plot(vario_toy)
lineal<-variofit(vario_toy,ini=c(sigmasq=var(daticos),phi=1),fix.nugget=FALSE,cov.model="linear")
pepita<-lineal$nugget
meseta<-lineal$nugget+summary(lineal)$estimated.pars[2]
rango<-summary(lineal)$estimated.pars[3]

coord<-cbind(c(1,1,1,2,2,2,3,3,3),c(1,2,3,1,2,3,1,2,3),c(12,15,10,14,NA,12,NA,14,13))
datoscte.kc<-krige.conv(datoscte,coords=datoscte[,c(1,2)], 
                        data = datoscte[,3] ,locations=datoscte.grid, 
                        bord=datoscte.borde, krige= krige.control(nugget=pepita,
                        cov.pars=c(sigmasq=meseta, phi= rango)))


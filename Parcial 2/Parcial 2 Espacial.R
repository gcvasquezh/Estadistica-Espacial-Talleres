#rm(list=ls(all=TRUE)) # Remueve de la consola todo
# CARGA LIBRERÍAS ####
lista_librerías <- c("xlsx","tools","gtools","stringr","readr","haven",
                     "dplyr","tidyr","tidyverse","WriteXLS","spatialEco",
                     "spdep","gstat","raster","sf","sp","readxl","pacman",
                     "sfheaders","foreign", "spatialreg","ggplot2")

no_installs <- lista_librerías[!lista_librerías %in% installed.packages()]

if(length(no_installs) > 0) {
  cat("Los siguientes paquetes no están instalados :\n")
  cat(no_installs, sep = "\n")
  install.packages(no_installs)
} else {
  cat("Todos los paquetes están instalados. \n")
}

sapply(lista_librerías, require, character = TRUE)

# CARGA BASES DE DATOS ####
directorio<-setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Base_bog <- st_read(paste0(directorio,"/mapa_Bog_localidades/mapa_Bog_localidades.shp"))
Base_col <- st_read(paste0(directorio,"/Mapa_Municipios_politico/MGN_ADM_MPIO_GRAFICO.shp"))
Base_datos <- read_excel(paste0(directorio,"/datos_parcial2.xlsx"))
Base_datos <- Base_datos[-c((ncol(Base_datos)-1),ncol(Base_datos))]

Base_col <- Base_col %>% 
  filter(mpio_cnmbr %in% c("CHÍA","LA CALERA","CHOACHÍ","UBAQUE","CHIPAQUE",
                           "UNE","GUTIÉRREZ","GUAMAL","CUBARRAL","URIBE",
                           "COLOMBIA","COTA","FUNZA","MOSQUERA","SOACHA",
                           "PASCA","ARBELÁEZ","SAN BERNARDO","CABRERA")) %>% 
  filter(dpto_cnmbr %in% c("CUNDINAMARCA","HUILA","META"))
Base_bog$LOCCODIGO<-as.numeric(Base_bog$LOCCODIGO)

Base_final <- Base_datos %>% left_join(Base_bog,by=c("Area"="LOCCODIGO"))
Base_final <- st_as_sf(Base_final, crs = 4326)


# MODELO SAR ####
## Coordenadas y función ####
coordenadas_localidades <- coordinates(as((Base_final), 'Spatial'))

nb_to_df <- function(nb, coords) {
  x <- coords[, 1]
  y <- coords[, 2]
  n <- length(nb)
  
  cardnb <- card(nb)
  i <- rep(1:n, cardnb)
  j <- unlist(nb)
  
  if (length(cardnb) == 0) {
    return(data.frame(x = numeric(0), xend = numeric(0), y = numeric(0), yend = numeric(0)))
  }
  
  return(data.frame(x = x[i], xend = x[j], y = y[i], yend = y[j]))
}

## CRITERIO DE QUEEN ####

queen <- poly2nb(as(Base_final, 'Spatial'), row.names = Base_final$Localidad, queen = T)
queen_lw <- nb2listw(queen, style="W", zero.policy=TRUE)

queen_bogota_df <- nb_to_df(queen, coordenadas_localidades)

box <- st_bbox(Base_final)
box

## Gráfico para vecinos Queen ####
ggplot() +
  geom_sf(data=Base_col, fill="cyan") +
  geom_sf(data = Base_final) +
  geom_point(data = queen_bogota_df, aes(x = x, y = y), color = "blue") +
  geom_segment(data = queen_bogota_df,
               aes(x = x, xend = xend, y = y, yend = yend), color = "blue") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  labs(x = "", y = "", title = "Vecinos Queen") +
  theme(panel.background=element_rect(fill = NA, color = NA))

## Construye el modelo SAR ####
sar_model <- spautolm(Casos ~ Localidad, data = Base_final, listw = queen_lw)
summary(sar_model)

## Agregando pesos tamaño población ####
poblacion_bog2023 <- read_excel("Población_Bogotá_2023.xlsx", 
                                sheet = "Localidades",range = "A12:KX1092")

poblacion_bog2023 <- poblacion_bog2023 %>% 
  filter(AREA=="Total" & AÑO=="2023") %>% 
  dplyr::select(COD_LOC,`TOTAL HOMBRES`,`TOTAL MUJERES`,TOTAL) %>% 
  rename(Poblacion_hombres=`TOTAL HOMBRES`,
         Poblacion_mujeres=`TOTAL MUJERES`,
         Poblacion_total=TOTAL)
poblacion_bog2023$COD_LOC <- as.numeric(poblacion_bog2023$COD_LOC) 

Base_final_localidades <- Base_final %>% left_join(poblacion_bog2023,by=c("Area"="COD_LOC"))
Base_final_localidades <- st_as_sf(Base_final_localidades, crs = 4326)

#nylmw <- glm(Casos ~ Localidad, family = poisson(), data = Base_final_localidades, weights = Poblacion_total)
#summary(nylmw)

# residuales

#Base_final_localidades$lmwresid <- residuals(nylmw) 

#a falta de especificación detectada por Moran's I 
#está relacionada de hecho con la heterocedasticidad 
#más que con la autocorrelación espacial. Podemos verificar
# esto también para el modelo SAR, ya que spautolm también acepta un argumento de pesos:

nysarw <- spautolm(Casos ~ Localidad, data = Base_final_localidades,
                   listw = queen_lw, weights = Poblacion_total)
summary(nysarw)

residuales <- residuals(nysarw)

moran_test_residuales <- moran.test(residuales, queen_lw) 
moran_test_residuales
summary(moran_test_residuales)


plot(Base_final_localidades["geometry"], col = "white", main = "Estimaciones del Modelo SAR")  ### Revisar código

# Graficar con límites automáticos para el eje x
plot(fit_values, col = "blue", ylim = range(fit_values))



#MODELO CAR ####
## CRITERIO DE QUEEN ####
queen <- poly2nb(as(Base_final, 'Spatial'), row.names = Base_final$Localidad, queen = T)
queen_lw <- nb2listw(queen, style="B", zero.policy=TRUE)
#queen_lw <- nb2listw(queen, style="W", zero.policy=TRUE)
#queen_lw <- nb2listw(queen, style="C", zero.policy=TRUE)
#queen_lw <- nb2listw(queen, style="U", zero.policy=TRUE)
#queen_lw <- nb2listw(queen, style="minmax", zero.policy=TRUE)
#queen_lw <- nb2listw(queen, style="S", zero.policy=TRUE)
#Con cualquiera el modelo con pesos falla xd xd xd

queen_bogota_df <- nb_to_df(queen, coordenadas_localidades)

box <- st_bbox(Base_final)
box

## Gráfico para vecinos Queen ####
ggplot() +
  geom_sf(data=Base_col, fill="cyan") +
  geom_sf(data = Base_final) +
  geom_point(data = queen_bogota_df, aes(x = x, y = y), color = "blue") +
  geom_segment(data = queen_bogota_df,
               aes(x = x, xend = xend, y = y, yend = yend), color = "blue") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  labs(x = "", y = "", title = "Vecinos Queen") +
  theme(panel.background=element_rect(fill = NA, color = NA))

## Construye el modelo CAR ####
car_model <- spautolm(Casos ~ Localidad, data = Base_final, 
                      family = "CAR", listw = queen_lw)
summary(car_model)

## Agregando pesos tamaño población ####
nysarw <- spautolm(Casos ~ Localidad, data = Base_final_localidades,
                   family = "CAR", listw = queen_lw, weights = Poblacion_total)
#summary(nysarw)

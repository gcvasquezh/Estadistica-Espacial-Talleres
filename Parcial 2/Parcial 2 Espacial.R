rm(list=ls(all=TRUE)) # Remueve de la consola todo
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
Base_datos <- read_delim(paste0(directorio,"/datos_parcial2.csv"), delim = ";", locale = locale(encoding = "Latin1"))
Base_datos$Población <- as.numeric(gsub("\\.", "", Base_datos$Población))

Base_col <- Base_col %>% 
  filter(mpio_cnmbr %in% c("CHÍA","LA CALERA","CHOACHÍ","UBAQUE","CHIPAQUE",
                           "UNE","GUTIÉRREZ","GUAMAL","CUBARRAL","URIBE",
                           "COLOMBIA","COTA","FUNZA","MOSQUERA","SOACHA",
                           "PASCA","ARBELÁEZ","SAN BERNARDO","CABRERA")) %>% 
  filter(dpto_cnmbr %in% c("CUNDINAMARCA","HUILA","META"))
Base_bog$LOCCODIGO<-as.numeric(Base_bog$LOCCODIGO)

Base_final <- Base_datos %>% left_join(Base_bog,by=c("Area"="LOCCODIGO"))
Base_final <- st_as_sf(Base_final, crs = 4326)
Base_final$Año <- as.factor(Base_final$Año)

# MODELO SAR ####
## Coordenadas y función ####
coordenadas_localidades <- coordinates(as((Base_final[Base_final$Año==2023,]), 'Spatial'))

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

queen <- poly2nb(as(Base_final, 'Spatial'), row.names = 1:nrow(Base_final), queen = T)
queen_lw <- nb2listw(queen, style="W", zero.policy=TRUE)

queen_bogota_df <- nb_to_df(queen, coordenadas_localidades)

legend_labels <- Base_final %>%
  distinct(Area, Localidad) %>%
  arrange(Area)

box <- st_bbox(Base_final)
box

## Gráfico para vecinos Queen ####
ggplot() +
  geom_sf(data=Base_col, fill="cyan") +
  geom_sf(data = Base_final, aes(fill = Casos)) +
  scale_fill_gradient(low = "pink", high = "purple", name = "Casos") +
  geom_point(data = queen_bogota_df, aes(x = x, y = y), color = "blue") +
  geom_segment(data = queen_bogota_df,
               aes(x = x, xend = xend, y = y, yend = yend), color = "blue") +
  geom_sf_text(data=Base_final,aes(label=Area),col="black",
               fontface="bold",size=3,fun.geometry=function(x) sf::st_centroid(x)) +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  labs(x = "", y = "", title = "Vecinos Queen") +
  theme(panel.background=element_rect(fill = NA, color = NA))

## Construye el modelo SAR ####
sar_model <- spautolm(Casos ~ Localidad + Año, data = Base_final, listw = queen_lw)
summary(sar_model)

## Agregando pesos tamaño población ####
nysarw_sar <- spautolm(Casos ~ Localidad + Año, data = Base_final,
                   listw = queen_lw, weights = Población)
summary(nysarw_sar)

## Residuales ####
residuales_sar <- residuals(nysarw_sar)

moran_test_residuales_sar <- moran.test(residuales_sar, queen_lw) 
moran_test_residuales_sar
summary(moran_test_residuales_sar)

#MODELO CAR ####
## CRITERIO DE QUEEN ####
queen <- poly2nb(as(Base_final, 'Spatial'), row.names = 1:nrow(Base_final), queen = T)
queen_lw <- nb2listw(queen, style="B", zero.policy=TRUE)

queen_bogota_df <- nb_to_df(queen, coordenadas_localidades)

box <- st_bbox(Base_final)
box

## Gráfico para vecinos Queen ####
ggplot() +
  geom_sf(data=Base_col, fill="cyan") +
  geom_sf(data = Base_final, aes(fill = Casos)) +
  scale_fill_gradient(low = "pink", high = "purple", name = "Casos") + 
  geom_point(data = queen_bogota_df, aes(x = x, y = y), color = "blue") +
  geom_segment(data = queen_bogota_df,
               aes(x = x, xend = xend, y = y, yend = yend), color = "blue") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  labs(x = "", y = "", title = "Vecinos Queen") +
  theme(panel.background=element_rect(fill = NA, color = NA))

## Construye el modelo CAR ####
car_model <- spautolm(Casos ~ Localidad + Año, data = Base_final, 
                      family = "CAR", listw = queen_lw)
summary(car_model)

## Agregando pesos tamaño población ####
nysarw_car <- spautolm(Casos ~ Localidad + Año, data = Base_final,
                   family = "CAR", listw = queen_lw, weights = Población)
summary(nysarw_car)

## Residuales ####
residuales_car <- residuals(nysarw_car)

moran_test_residuales_car <- moran.test(residuales_car, queen_lw) 
moran_test_residuales_car
summary(moran_test_residuales_car)

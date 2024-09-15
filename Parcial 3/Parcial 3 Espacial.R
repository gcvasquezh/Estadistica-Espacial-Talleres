rm(list=ls(all=TRUE)) # Remueve de la consola todo
# CARGA LIBRERÍAS ####
lista_librerías <- c("xlsx","tools","gtools","stringr","readr","haven","ggspatial",
                     "dplyr","tidyr","tidyverse","WriteXLS","spatialEco", "viridis",
                     "spdep","gstat","raster","sf","sp","readxl","pacman", "MASS",
                     "sfheaders","foreign", "spatialreg","ggplot2","spatstat")

no_installs <- lista_librerías[!lista_librerías %in% installed.packages()]

if(length(no_installs) > 0) {
  cat("Los siguientes paquetes no están instalados :\n")
  cat(no_installs, sep = "\n")
  install.packages(no_installs)
} else {
  cat("Todos los paquetes están instalados. \n")
}

sapply(lista_librerías, require, character = TRUE)


# PUNTO 3 ####
## a ####
### SIMULACIÓN Y ENMALLADO ####
ventana <- owin(c(0, 1), c(0, 1))
lambda <- 100
set.seed(123)
aleatorio <- rpoispp(lambda, win = ventana)
n <- 4
grid_cells <- seq(0, 1, length.out = n + 1)

# Crear una ventana de subregiones
ventanas_subregiones <- lapply(1:n, function(i) {
  lapply(1:n, function(j) {
    x_range <- c(grid_cells[i], grid_cells[i+1])
    y_range <- c(grid_cells[j], grid_cells[j+1])
    owin(xrange = x_range, yrange = y_range)
  })
})

ventanas_subregiones <- do.call(c, ventanas_subregiones)

# Contar los puntos en cada subregión
conteo_puntos <- sapply(ventanas_subregiones, function(w) {
  sum(inside.owin(aleatorio$x, aleatorio$y, w))
})

# Imprimir el conteo de puntos por subregión
print(conteo_puntos)
# Número esperado de puntos por subregión
esperado <- lambda / (n * n);esperado
sum(conteo_puntos)

# Paso 3: Prueba de bondad de ajuste para la aleatoriedad (χ²)
# Realizar la prueba de bondad de ajuste χ²
chisq_test <- chisq.test(conteo_puntos, p = rep(1/(n*n), n*n))
print(chisq_test)
# H0: Los puntos están distribuidos aleatoriamente y uniformemente en la región 
#(proceso de Poisson homogéneo).
# H1: Los puntos no siguen una distribución completamente aleatoria o uniforme,
#lo que indicaría algún tipo de estructura o patrón en la distribución espacial.

### FUNCIÓN G ####
G1<-Gest(aleatorio)
plot(G1)
plot(envelope(aleatorio, Gest), main=" Función G con bandas de confianza")

### FUNCIÓN F ####
F1<-Fest(aleatorio)
plot(F1)
plot(envelope(aleatorio, Fest), main=" Función F con bandas de confianza")

### FUNCIÓN k ####
K1<-Kest(aleatorio)
plot(K1)
plot(envelope(aleatorio, Kest), main=" Función K con bandas de confianza")

## b ####
### SIMULACIÓN Y ENMALLADO ####
ventana <- owin(c(0, 1), c(0, 1))
lambda_function <- function(x, y) {
  100 * exp(-2 * x + 5 * y)
}
set.seed(123)
aleatorio <- rpoispp(lambda_function, win = ventana)
n <- 4
grid_cells <- seq(0, 1, length.out = n + 1)

# Crear una ventana de subregiones
ventanas_subregiones <- lapply(1:n, function(i) {
  lapply(1:n, function(j) {
    x_range <- c(grid_cells[i], grid_cells[i+1])
    y_range <- c(grid_cells[j], grid_cells[j+1])
    owin(xrange = x_range, yrange = y_range)
  })
})

ventanas_subregiones <- do.call(c, ventanas_subregiones)

# Contar los puntos en cada subregión
conteo_puntos <- sapply(ventanas_subregiones, function(w) {
  sum(inside.owin(aleatorio$x, aleatorio$y, w))
})

# Imprimir el conteo de puntos por subregión
print(conteo_puntos)
sum(conteo_puntos)

# Paso 3: Prueba de bondad de ajuste para la aleatoriedad (χ²)
# Realizar la prueba de bondad de ajuste χ²
chisq_test <- chisq.test(conteo_puntos, p = rep(1/(n*n), n*n))
print(chisq_test)
# H0: Los puntos están distribuidos aleatoriamente y uniformemente en la región 
#(proceso de Poisson homogéneo).
# H1: Los puntos no siguen una distribución completamente aleatoria o uniforme,
#lo que indicaría algún tipo de estructura o patrón en la distribución espacial.

### FUNCIÓN G ####
G1<-Gest(aleatorio)
plot(G1)
plot(envelope(aleatorio, Gest), main=" Función G con bandas de confianza")

### FUNCIÓN F ####
F1<-Fest(aleatorio)
plot(F1)
plot(envelope(aleatorio, Fest), main=" Función F con bandas de confianza")

### FUNCIÓN k ####
K1<-Kest(aleatorio)
plot(K1)
plot(envelope(aleatorio, Kest), main=" Función K con bandas de confianza")

# PUNTO 4 ####
## CARGA BASES DE DATOS ####
directorio<-setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Base_col <- st_read(paste0(directorio,"/Mapa_Dep_politico/MGN_ADM_DPTO_POLITICO.shp"))
Base_datos <- read_excel(paste0(directorio,"/sismicidad_historica.xls"),range="A8:P91")
Base_datos <- Base_datos[c(4,5,16)]

sep_mun <- with(Base_datos,str_split_fixed(`Area epicentral`,",",n=2))
Base_datos <- within(Base_datos,{
  departamento <- str_to_title(str_squish(str_to_lower(sep_mun[,2])))
  rm(`Area epicentral`)
})
Base_datos$departamento <- str_to_upper(Base_datos$departamento)

Base_final <- Base_col %>% left_join(Base_datos,by=c("dpto_cnmbr"="departamento"))
Base_final <- cbind(Base_final[10],Base_final[9],Base_final[c(1:8)])
Base_final <- Base_final[-c(12:13)] 
colnames(Base_final)[1:2] <- c("Latitud","Longitud")
Base_final$Latitud <- as.numeric(Base_final$Latitud)
Base_final$Longitud <- as.numeric(Base_final$Longitud)
Base_final <- st_as_sf(Base_final, crs = 4326)
## ESTIMACIÓN KERNEL FUNCIÓN DE INTENSIDAD ####
bbox <- st_bbox(Base_final)
ventana <- owin(c(bbox["xmin"], bbox["xmax"]), c(bbox["ymin"], bbox["ymax"]))
puntos <- ppp(Base_final$Latitud, Base_final$Longitud, window = ventana)
densidad <- density(puntos)
densidad_raster <- raster::raster(densidad)
mask <- raster::rasterize(st_as_sf(Base_final), densidad_raster, field = 1)
densidad_raster <- raster::mask(densidad_raster, mask)
df_densidad <- as.data.frame(rasterToPoints(densidad_raster), xy = TRUE)
colnames(df_densidad) <- c("x", "y", "densidad")
contornos <- rasterToContour(densidad_raster)
df_contorno <- do.call(rbind, lapply(contornos@lines, function(lines_object) {
  coords_list <- lapply(lines_object@Lines, function(line) {
    coords <- as.data.frame(line@coords)
    coords
  })
  coords_df <- do.call(rbind, coords_list)
  level <- rep(lines_object@ID, nrow(coords_df))
  data.frame(x = coords_df[,1], y = coords_df[,2], level = level)
}))

graf_contor <- kde2d(df_contorno$x, df_contorno$y, n = 100)
df_graf_contor <- expand.grid(x = graf_contor$x, y = graf_contor$y)
df_graf_contor$z <- as.vector(graf_contor$z)

ggplot() +
  geom_sf(data = Base_final, fill = NA, color = "black") +
  geom_raster(data = df_densidad, aes(x = x, y = y, fill = densidad), alpha = 0.6) +
  geom_contour(data = df_graf_contor, aes(x = x, y = y, z = z), color = "black") +
  geom_point(data = Base_final, aes(x = Latitud, y = Longitud), color = "blue", size = 2) +
  coord_sf() +
  labs(title = "Estimación Kernel de la función de intensidad", fill = "Densidad",
       x = "",  y = "",) +
  scale_fill_viridis_c(option = "plasma", begin = 0, end = 1, name = "Densidad") +
  theme_minimal()

## FUNCIÓN G ####
G1<-Gest(puntos)
plot(G1, main="Función G - Presencia de Sismos en Colombia")
plot(envelope(puntos, Gest), main=" Función G con bandas de confianza - Presencia de Sismos en Colombia")

## FUNCIÓN F ####
F1<-Fest(puntos)
plot(F1, main="Función F - Presencia de Sismos en Colombia")
plot(envelope(puntos, Fest), main=" Función F con bandas de confianza - Presencia de Sismos en Colombia")

## FUNCIÓN k ####
K1<-Kest(puntos)
plot(K1, main="Función K - Presencia de Sismos en Colombia")
plot(envelope(puntos, Kest), main=" Función K con bandas de confianza - Presencia de Sismos en Colombia")

## Test de aleatoriedad basado en cuadrantes ####
Q<-quadratcount(puntos, nx=6,ny=3)
plot(Q, main = "Conteo de puntos por cuadrantes")

M<-quadrat.test(puntos, nx=3, ny=3)
plot(M, main = "Test de Aleatoriedad por cuadrantes")
M

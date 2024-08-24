rm(list=ls(all=TRUE)) # Remueve de la consola todo

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


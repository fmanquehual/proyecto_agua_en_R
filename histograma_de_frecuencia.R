library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/')
cuenca <- readOGR('.', 'VectorCuencaPadreCasas')

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/DEM/')
dem <- raster('DEM_CuencaPadreCasas.tif')




# Reclasificacion ----

# DEM 
minValue(dem)
maxValue(dem)

intervalo <- 50
rangos <- seq(0, maxValue(dem), by=intervalo) ; rangos
if(rangos[length(rangos)]<maxValue(dem)){rangos <- c(rangos, rangos[length(rangos)]+intervalo)}

# (minimo, maximo, nuevo valor) -> (no se incluye, <=, nuevo valor) por default
pre.m1 <- c(rangos[1], rangos[2], intervalo*1,
            rangos[2], rangos[3], intervalo*2,
            rangos[3], rangos[4], intervalo*3,
            rangos[4], rangos[5], intervalo*4,
            rangos[5], rangos[6], intervalo*5,
            rangos[6], rangos[7], intervalo*6,
            rangos[7], rangos[8], intervalo*7,
            rangos[8], rangos[9], intervalo*8,
            rangos[9], rangos[10], intervalo*9,
            rangos[10], rangos[11], intervalo*10)
pre.m1 

m1 <- matrix(pre.m1, ncol=3, byrow=TRUE) ; m1
reclas.dem <- reclassify(dem, m1)

plot(reclas.dem)
hist(reclas.dem, xlab = 'Altitud')

# fin ---




# Raster a poligonos ----

dem.poligonos <- rasterToPolygons(reclas.dem, dissolve = TRUE)
plot(dem.poligonos, axes = TRUE)

head(dem.poligonos@data)
dim(dem.poligonos@data)

area.total <- gArea(dem.poligonos)/1000000
dem.poligonos@data$area_km2 <- gArea(dem.poligonos, byid=TRUE)/1000000
dem.poligonos@data$area_en_porcentaje <- (dem.poligonos@data$area_km2*100)/area.total

# fin ---




# Histograma de frecuancia altimetrica ----

setwd('C:/Users/Usuario/Dropbox/Proyecto_agua/plots/')
png('histograma_de_frecuencias_altimetricas_CuencaPadreCasas.png')

ggplot(dem.poligonos@data, aes(x=DEM_CuencaPadreCasas, y=area_en_porcentaje))+
  geom_col() +
  labs(x='Altitud', y='Área de la cuenca (%)') +
  #scale_x_discrete(limits = rangos) +
  #coord_flip() +
  theme_bw()

dev.off()

# fin ---
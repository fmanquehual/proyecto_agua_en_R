library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)

rm(list=ls())
dev.off()

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/')
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')
nombre.archivo.cuenca <- 'poligono_cuenca_Estero_Neicuf_utm18s'
cuenca <- readOGR('.', nombre.archivo.cuenca)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')
dem <- raster('DEM_marco_trabajo_Teodoro_Schmidt_utm18s.tif')
# dem[dem%in%c(-32768, 32767)] <- NA

dem <- mask(dem, cuenca)
plot(dem)


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
            rangos[7], rangos[8], intervalo*7)#,
            # rangos[8], rangos[9], intervalo*8,
            # rangos[9], rangos[10], intervalo*9,
            # rangos[10], rangos[11], intervalo*10,
            # rangos[11], rangos[12], intervalo*11,
            # rangos[12], rangos[13], intervalo*12,
            # rangos[13], rangos[14], intervalo*13,
            # rangos[14], rangos[15], intervalo*14)
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
colnames(dem.poligonos@data)[1] <- 'altitud'
head(dem.poligonos@data)

area.total <- gArea(dem.poligonos)/1000000
dem.poligonos@data$area_km2 <- gArea(dem.poligonos, byid=TRUE)/1000000
dem.poligonos@data$area_en_porcentaje <- (dem.poligonos@data$area_km2*100)/area.total

# fin ---




# Histograma de frecuancia altimetrica ----

setwd('C:/Users/Usuario/Dropbox/Proyecto_agua/plots/')
nombre.plot <- paste('histograma_de_frecuencia_', nombre.archivo.cuenca, '.png', sep = '') ; nombre.plot
# png(nombre.plot)

ggplot(dem.poligonos@data, aes(x=altitud, y=area_en_porcentaje))+
  geom_col() +
  labs(x='Altitud', y='Área de la cuenca (%)') +
  #scale_x_discrete(limits = rangos) +
  #coord_flip() +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"))

dev.off()

# fin ---
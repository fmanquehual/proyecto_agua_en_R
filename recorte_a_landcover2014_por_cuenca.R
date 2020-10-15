library(raster)
library(rgdal)
library(rgeos)
library(dplyr) # join entre db de cobertura y data frame

rm(list=ls())
dev.off()


# Lectura de archivos ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')

db.land.cover.clases <- read.csv2('clases_land_cover.csv')  ; db.land.cover.clases
nombre.archivo.land.cover <- 'LC_CHILE_2014_b_marco_Araucania_utm18s.tif'
land.cover <- raster(nombre.archivo.land.cover)


setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/cuencas_buffer_y_clips/')

nombre.archivo.cuenca.puren <- 'poligono_cuenca_Rio_Puren_utm18s_buffer_de_1000_m'
cuenca.puren <- readOGR('.', nombre.archivo.cuenca.puren)

nombre.archivo.cuenca.neicuf <- 'poligono_cuenca_Estero_Neicuf_utm18s_buffer_de_1000_m'
cuenca.neicuf <- readOGR('.', nombre.archivo.cuenca.neicuf)

nombre.archivo.cuenca.poleco <- 'poligono_cuenca_Estero_Poleco_utm18s_buffer_de_1000_m'
cuenca.poleco <- readOGR('.', nombre.archivo.cuenca.poleco)

nombre.archivo.cuenca.pLasCasas <- 'VectorCuencaPadreCasas_buffer_de_1000_m'
cuenca.pLasCasas <- readOGR('.', nombre.archivo.cuenca.pLasCasas)

# fin ---




# Clip ----

land.cover.puren <- crop(land.cover, cuenca.puren)
land.cover.neicuf <- crop(land.cover, cuenca.neicuf)
land.cover.poleco <- crop(land.cover, cuenca.poleco)
land.cover.pLasCasas <- crop(land.cover, cuenca.pLasCasas)

land.cover.puren <- mask(land.cover.puren, cuenca.puren)
land.cover.neicuf <- mask(land.cover.neicuf, cuenca.neicuf)
land.cover.poleco <- mask(land.cover.poleco, cuenca.poleco)
land.cover.pLasCasas <- mask(land.cover.pLasCasas, cuenca.pLasCasas)

plot(land.cover.pLasCasas)

# fin ---




# Disolver ----

land.cover.puren.poligono <- rasterToPolygons(land.cover.puren, dissolve = TRUE)
colnames(land.cover.puren.poligono@data) <- 'codigo_nivel_2_y_3'

land.cover.neicuf.poligono <- rasterToPolygons(land.cover.neicuf, dissolve = TRUE)
colnames(land.cover.neicuf.poligono@data) <- 'codigo_nivel_2_y_3'

land.cover.poleco.poligono <- rasterToPolygons(land.cover.poleco, dissolve = TRUE)
colnames(land.cover.poleco.poligono@data) <- 'codigo_nivel_2_y_3'

land.cover.pLasCasas.poligono <- rasterToPolygons(land.cover.pLasCasas, dissolve = TRUE)
colnames(land.cover.pLasCasas.poligono@data) <- 'codigo_nivel_2_y_3'

# fin ---




# Join ---

land.cover.puren.poligono@data <- left_join(land.cover.puren.poligono@data, 
                                            db.land.cover.clases, by='codigo_nivel_2_y_3')

land.cover.neicuf.poligono@data <- left_join(land.cover.neicuf.poligono@data, 
                                            db.land.cover.clases, by='codigo_nivel_2_y_3')

land.cover.poleco.poligono@data <- left_join(land.cover.poleco.poligono@data, 
                                            db.land.cover.clases, by='codigo_nivel_2_y_3')

land.cover.pLasCasas.poligono@data <- left_join(land.cover.pLasCasas.poligono@data, 
                                            db.land.cover.clases, by='codigo_nivel_2_y_3')

# fin ---




# Guardando capas ---

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/cuencas_buffer_y_clips/')

# guardando cuencas
nombre.archivo.cuenca.puren.salida <- paste('landcover', nombre.archivo.cuenca.puren, sep = '_')
nombre.archivo.cuenca.neicuf.salida <- paste('landcover', nombre.archivo.cuenca.neicuf, sep = '_')
nombre.archivo.cuenca.poleco.salida <- paste('landcover', nombre.archivo.cuenca.poleco, sep = '_')
nombre.archivo.cuenca.pLasCasas.salida <- paste('landcover', nombre.archivo.cuenca.pLasCasas, sep = '_')

# writeOGR(land.cover.puren.poligono, ".", nombre.archivo.cuenca.puren.salida, driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(land.cover.neicuf.poligono, ".", nombre.archivo.cuenca.neicuf.salida, driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(land.cover.poleco.poligono, ".", nombre.archivo.cuenca.poleco.salida, driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(land.cover.pLasCasas.poligono, ".", nombre.archivo.cuenca.pLasCasas.salida, driver="ESRI Shapefile", overwrite_layer = TRUE)

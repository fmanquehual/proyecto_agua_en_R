library(raster)
library(rgdal)
library(rgeos)

rm(list=ls())
dev.off()


# Lectura de archivos ----

setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/Catastro_uso_suelo_y_vegetacion/Catastro_uso_suelo_y_vegetacion')
cbn <- readOGR('.', 'Catastro_RV_R09_2014')


setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')

nombre.archivo.cuenca.puren <- 'poligono_cuenca_Rio_Puren_utm18s'
cuenca.puren <- readOGR('.', nombre.archivo.cuenca.puren)

nombre.archivo.cuenca.neicuf <- 'poligono_cuenca_Estero_Neicuf_utm18s'
cuenca.neicuf <- readOGR('.', nombre.archivo.cuenca.neicuf)

nombre.archivo.cuenca.poleco <- 'poligono_cuenca_Estero_Poleco_utm18s'
cuenca.poleco <- readOGR('.', nombre.archivo.cuenca.poleco)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/')  # padre las casas
nombre.archivo.cuenca.pLasCasas <- 'VectorCuencaPadreCasas'
cuenca.pLasCasas <- readOGR('.', nombre.archivo.cuenca.pLasCasas)

# fin ---




# Buffer ----

buffer.i <- 1000

cuenca.puren.buffer <- raster::buffer(cuenca.puren, width=buffer.i, dissolve=FALSE)
cuenca.neicuf.buffer <- raster::buffer(cuenca.neicuf, width=buffer.i, dissolve=TRUE)
cuenca.poleco.buffer <- raster::buffer(cuenca.poleco, width=buffer.i, dissolve=FALSE)
cuenca.pLasCasas.buffer <- raster::buffer(cuenca.pLasCasas, width=buffer.i, dissolve=FALSE)

cbn.buffer <- raster::buffer(cbn, width=0, dissolve=FALSE) # corrige errores

db.cuenca.neicuf.buffer <- data.frame(id = 1)
row.names(db.cuenca.neicuf.buffer) <- 'buffer'
cuenca.neicuf.buffer <- SpatialPolygonsDataFrame(cuenca.neicuf.buffer, data = db.cuenca.neicuf.buffer, match.ID = TRUE)

plot(cuenca.neicuf.buffer)
plot(cuenca.neicuf, add=TRUE, lty=2)

# fin ---




# clip ----

head(cbn.buffer@data)
colnames(cbn.buffer@data)

cbn.buffer@data <- cbn.buffer@data[,-c(1:19, 25, 29:ncol(cbn.buffer@data))]
head(cbn.buffer@data)

cbn.puren <- crop(cbn.buffer, cuenca.puren.buffer)
cbn.neicuf <- crop(cbn.buffer, cuenca.neicuf.buffer)
cbn.poleco <- crop(cbn.buffer, cuenca.poleco.buffer)
cbn.pLasCasas <- crop(cbn.buffer, cuenca.pLasCasas.buffer)

# fin ---




# Guardando capas ---

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')
# dir.create('cuencas_buffer_y_clips')
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/cuencas_buffer_y_clips/')


# guardando cuencas
nombre.archivo.cuenca.puren.salida <- paste(nombre.archivo.cuenca.puren, 'buffer_de', buffer.i, 'm', sep = '_')
nombre.archivo.cuenca.neicuf.salida <- paste(nombre.archivo.cuenca.neicuf, 'buffer_de', buffer.i, 'm', sep = '_')
nombre.archivo.cuenca.poleco.salida <- paste(nombre.archivo.cuenca.poleco, 'buffer_de', buffer.i, 'm', sep = '_')
nombre.archivo.cuenca.pLasCasas.salida <- paste(nombre.archivo.cuenca.pLasCasas, 'buffer_de', buffer.i, 'm', sep = '_')

# writeOGR(cuenca.puren.buffer, ".", nombre.archivo.cuenca.puren.salida, driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(cuenca.neicuf.buffer, ".", nombre.archivo.cuenca.neicuf.salida, driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(cuenca.poleco.buffer, ".", nombre.archivo.cuenca.poleco.salida, driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(cuenca.pLasCasas.buffer, ".", nombre.archivo.cuenca.pLasCasas.salida, driver="ESRI Shapefile", overwrite_layer = TRUE)


# guardando clip cbn
nombre.archivo.cuenca.puren.cbn.salida <- paste('cbn', nombre.archivo.cuenca.puren, 'buffer_de', buffer.i, 'm', sep = '_')
nombre.archivo.cuenca.neicuf.cbn.salida <- paste('cbn', nombre.archivo.cuenca.neicuf, 'buffer_de', buffer.i, 'm', sep = '_')
nombre.archivo.cuenca.poleco.cbn.salida <- paste('cbn', nombre.archivo.cuenca.poleco, 'buffer_de', buffer.i, 'm', sep = '_')
nombre.archivo.cuenca.pLasCasas.cbn.salida <- paste('cbn', nombre.archivo.cuenca.pLasCasas, 'buffer_de', buffer.i, 'm', sep = '_')

# writeOGR(cbn.puren, ".", nombre.archivo.cuenca.puren.cbn.salida, driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(cbn.neicuf, ".", nombre.archivo.cuenca.neicuf.cbn.salida, driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(cbn.poleco, ".", nombre.archivo.cuenca.poleco.cbn.salida, driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(cbn.pLasCasas, ".", nombre.archivo.cuenca.pLasCasas.cbn.salida, driver="ESRI Shapefile", overwrite_layer = TRUE)

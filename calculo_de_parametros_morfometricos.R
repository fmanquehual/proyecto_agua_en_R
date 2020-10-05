library(raster)
library(rgdal)
library(rgeos)

rm(list=ls())
dev.off()




# Lectura de capas ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/')
cuenca <- readOGR('.', 'VectorCuencaPadreCasas')
red.hidrica <- readOGR('.', 'VectorRedPadreCasas')

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/DEM/')
dem.09 <- raster('9.jp2')

# ---





# Preparacion capas ----

crs(cuenca)
crs(red.hidrica)
crs(dem.09)

# Homogeneizar sistema de coordenadas

# sistema.coordenadas.referencia <- crs(dem.09)
# cuenca <- spTransform(cuenca, sistema.coordenadas.referencia)


# Recorte DEM

buffer.i <- res(dem.09)[1] ; buffer.i
cuenca.buffer <- buffer(cuenca, width=buffer.i)

dem.cuenca0 <- crop(dem.09, cuenca.buffer)
dem.cuenca <- mask(dem.cuenca0, cuenca.buffer)


# Plots

plot(dem.cuenca)
plot(cuenca.buffer, border='red', lwd=2, add=TRUE)
plot(red.hidrica, lty=2, add=TRUE)

# fin ---




# Calculo parametros Red de Drenaje ----

# 1. Orden de drenaje (Strahler) hecho en QGIS
red.hidrica@data$strahler <- as.numeric(red.hidrica@data$strahler)
orden.de.drenaje <- unique(red.hidrica@data$strahler) ; orden.de.drenaje


# 2. Numero de cursos de agua (FALTA)


# 3. Longitud del curso de agua principal
orden.maximo <- max(red.hidrica@data$strahler) ; orden.maximo

id.con.orden.maximo <- which(red.hidrica@data$strahler==orden.maximo)
curso.de.agua.principal <- red.hidrica[id.con.orden.maximo,]
plot(curso.de.agua.principal, col='green', add=TRUE)

longitud.curso.de.agua.principal <- gLength(curso.de.agua.principal)/1000 ; longitud.curso.de.agua.principal


# 4. Longitud de cursos de agua
longitud.de.cursos.de.agua <- gLength(red.hidrica)/1000 ; longitud.de.cursos.de.agua


# 5. Longitud promedio de cursos (FALTA)


# 6. Radio de longitud de cursos
db <- data.frame(strahler=orden.de.drenaje) ; db

orden.i <- which(red.hidrica@data$strahler==db$strahler[1])
curso.de.agua.de.orden.i <- red.hidrica[orden.i,]

plot(dem.cuenca)
plot(red.hidrica, add=TRUE)
plot(curso.de.agua.de.orden.i, col='green', add=TRUE)

longitud.curso.de.agua.de.orden.i <- gLength(curso.de.agua.de.orden.i)

# es lu.i <- longitud.curso.de.agua.de.orden.2/numero.cursos.de.agua.de.orden.2
# es lu.i-1 <- longitud.curso.de.agua.de.orden.1/numero.cursos.de.agua.de.orden.1
# Rl <- lu.i/lu.i-1
# fin ---
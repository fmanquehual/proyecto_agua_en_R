library(raster)
library(rgdal)
library(rgeos)

rm(list=ls())
dev.off()




# Lectura de capas ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/')
cuenca <- readOGR('.', 'VectorCuencaPadreCasas')
red.hidrica <- readOGR('.', 'VectorRedPadreCasas')

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_strahler/')
curso.de.agua.de.orden.1 <- readOGR('.', 'VectorRedPadreCasas_strahler_orden_1_disuelto')
curso.de.agua.de.orden.2 <- readOGR('.', 'VectorRedPadreCasas_strahler_orden_2_disuelto')
curso.de.agua.de.orden.3 <- readOGR('.', 'VectorRedPadreCasas_strahler_orden_3_disuelto')
curso.de.agua.de.orden.4 <- readOGR('.', 'VectorRedPadreCasas_strahler_orden_4_disuelto')
curso.de.agua.de.orden.5 <- readOGR('.', 'VectorRedPadreCasas_strahler_orden_5_disuelto')
curso.de.agua.de.orden.6 <- readOGR('.', 'VectorRedPadreCasas_strahler_orden_6_disuelto')
  
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
plot(red.hidrica, lwd=2, add=TRUE)

# fin ---




# Calculo parametros Red de Drenaje ----

# 1. Orden de drenaje (Strahler) hecho en QGIS
red.hidrica@data$strahler <- as.numeric(red.hidrica@data$strahler)
U <- unique(red.hidrica@data$strahler) ; U


# 2. Numero de cursos de agua

# Proceso hecho en script 'preparacion_de_capas_para_identificar_numero_de_cursos_de_agua_segun_orden_de_strahler':
# - seleccionar solo a cursos de agua de orden i
# - buffer (5 m por lado)

# Proceso hecho en QGIS:
# - ocupar v.dissolve de GRASS, y listo!

N1 <- length(curso.de.agua.de.orden.1)
N2 <- length(curso.de.agua.de.orden.2)
N3 <- length(curso.de.agua.de.orden.3)
N4 <- length(curso.de.agua.de.orden.4)
N5 <- length(curso.de.agua.de.orden.5)
N6 <- length(curso.de.agua.de.orden.6)

Nu <- sum(N1, N2, N3, N4, N5, N6)
Nu


# 3. Longitud del curso de agua principal
L <- gLength(curso.de.agua.de.orden.6)/1000
L


# 4. Longitud de cursos de agua

L1 <- gLength(curso.de.agua.de.orden.1)/1000
L2 <- gLength(curso.de.agua.de.orden.2)/1000
L3 <- gLength(curso.de.agua.de.orden.3)/1000
L4 <- gLength(curso.de.agua.de.orden.4)/1000
L5 <- gLength(curso.de.agua.de.orden.5)/1000
L6 <- gLength(curso.de.agua.de.orden.6)/1000

Lu <- sum(L1, L2, L3, L4, L5, L6)
Lu


# 5. Longitud promedio de cursos
Lm1 <- L1/N1
Lm2 <- L2/N2
Lm3 <- L3/N3
Lm4 <- L4/N4
Lm5 <- L5/N5
Lm6 <- L6/N6

Lm <- Lu/Nu
Lm


# 6. Radio de longitud de cursos
RL2 <- Lm2/Lm1
RL3 <- Lm3/Lm2
RL4 <- Lm4/Lm3
RL5 <- Lm5/Lm4
RL6 <- Lm6/Lm5

RL <- mean(RL2, RL3, RL4, RL5, RL6)
RL


# 7. Radio de bifurcacion
Rb1 <- N1/N2
Rb2 <- N2/N3
Rb3 <- N3/N4
Rb4 <- N4/N5
Rb5 <- N5/N6

Rb <- mean(Rb1, Rb2, Rb3, Rb4, Rb5)
Rb


# 8. Coeficiente Rho
Rho <- RL/Rb
Rho

# fin ---




# Calculo parametros Forma de la Cuenca ----

# 9. Longitud


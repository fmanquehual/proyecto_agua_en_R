library(raster)
library(rgdal)
library(rgeos)

rm(list=ls())
dev.off()


setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/proyecto_agua_en_R/')
source('funcion_pendiente_media_para_curso_de_agua_principal.R')
source('funcion_db_con_pixeles_identificados_y_ordenados.R')
source('funcion_filtro_curso_de_agua.R')

# Lectura de capas ----

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/')  # padre las casas
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')

cuenca <- readOGR('.', 'poligono_cuenca_Rio_Puren_utm18s')
red.hidrica <- readOGR('.', 'linea_red_hidrografica_Rio_Puren_utm18s')
linea.longitud.de.la.cuenca <- readOGR('.', 'linea_longitud_de_la_cuenca_Puren_utm18s')
punto.desembocadura <- readOGR('.', 'punto_desembocadura_de_interes_en_Rio_Puren_utm18s')


setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_strahler/')

curso.de.agua.de.orden.1 <- readOGR('.', 'linea_red_hidrografica_Rio_Puren_utm18s_strahler_orden_1_disuelto')
curso.de.agua.de.orden.2 <- readOGR('.', 'linea_red_hidrografica_Rio_Puren_utm18s_strahler_orden_2_disuelto')
curso.de.agua.de.orden.3 <- readOGR('.', 'linea_red_hidrografica_Rio_Puren_utm18s_strahler_orden_3_disuelto')
# curso.de.agua.de.orden.4 <- readOGR('.', 'linea_red_hidrografica_Estero_Poleco_utm18s_strahler_orden_4_disuelto')
# curso.de.agua.de.orden.5 <- readOGR('.', 'VectorRedPadreCasas_strahler_orden_5_disuelto')
# curso.de.agua.de.orden.6 <- readOGR('.', 'VectorRedPadreCasas_strahler_orden_6_disuelto')

  
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')

dem.09 <- raster('DEM_marco_trabajo_Rio_Puren_utm18s.tif')
pendiente.cuenca <- raster('pendiente_marco_trabajo_Rio_Puren_utm18s.tif')

# ---





# Preparacion capas ----

crs(cuenca)
crs(red.hidrica)
crs(dem.09)

# Homogeneizar sistema de coordenadas

# sistema.coordenadas.referencia <- crs(dem.09)
# cuenca <- spTransform(cuenca, sistema.coordenadas.referencia)


# Recorte DEM

#buffer.i <- res(dem.09)[1] ; buffer.i
buffer.i <- 0
cuenca.buffer <- buffer(cuenca, width=buffer.i)

dem.cuenca0 <- crop(dem.09, cuenca.buffer)
dem.cuenca <- mask(dem.cuenca0, cuenca.buffer)

# writeRaster(dem.cuenca, filename="DEM_CuencaPadreCasas.tif",
#             format="GTiff", overwrite=TRUE)


# Plots

plot(dem.cuenca)
plot(cuenca.buffer, border='red', lwd=2, add=TRUE)
plot(red.hidrica, lwd=2, add=TRUE)
plot(punto.desembocadura, pch=16, col='cyan', cex=2, add=TRUE)

# fin ---




# Calculo parametros Red de Drenaje ----

# 1. Orden de drenaje (Strahler) hecho en QGIS
red.hidrica@data$strahler <- as.numeric(red.hidrica@data$strahler)
U <- max(red.hidrica@data$strahler) ; U


# 2. Numero de cursos de agua

# Proceso hecho en script 'preparacion_de_capas_para_identificar_numero_de_cursos_de_agua_segun_orden_de_strahler':
# - seleccionar solo a cursos de agua de orden i
# - buffer (5 m por lado)

# Proceso hecho en QGIS:
# - ocupar v.dissolve de GRASS, y listo!

N1 <- length(curso.de.agua.de.orden.1) ; N1
N2 <- length(curso.de.agua.de.orden.2) ; N2
N3 <- length(curso.de.agua.de.orden.3) ; N3
# N4 <- length(curso.de.agua.de.orden.4) ; N4
# N5 <- length(curso.de.agua.de.orden.5) ; N5
# N6 <- length(curso.de.agua.de.orden.6) ; N6

Nu <- sum(N1, N2, N3)#, N4)#, N5, N6)
Nu


# 3. Longitud del curso de agua principal
curso.de.agua.principal <- filtro_curso_de_agua(red.hidrica, U)

L <- gLength(curso.de.agua.principal)/1000
round(L, 2)


# 4. Longitud de cursos de agua
L1 <- gLength(filtro_curso_de_agua(red.hidrica, 1))/1000 ; round(L1, 2)
L2 <- gLength(filtro_curso_de_agua(red.hidrica, 2))/1000 ; round(L2, 2)
L3 <- gLength(filtro_curso_de_agua(red.hidrica, 3))/1000 ; round(L3, 2)
# L4 <- gLength(filtro_curso_de_agua(red.hidrica, 4))/1000 ; round(L4, 2)
# L5 <- gLength(filtro_curso_de_agua(red.hidrica, 5))/1000 ; L5
# L6 <- gLength(filtro_curso_de_agua(red.hidrica, 6))/1000 ; L6

Lu <- sum(L1, L2, L3)#, L4)#, L5, L6)
round(Lu, 2)


# 5. Longitud promedio de cursos
Lm1 <- L1/N1 ; round(Lm1, 2)
Lm2 <- L2/N2 ; round(Lm2, 2)
Lm3 <- L3/N3 ; round(Lm3, 2)
# Lm4 <- L4/N4 ; round(Lm4, 2)
# Lm5 <- L5/N5 ; round(Lm5, 2)
# Lm6 <- L6/N6 ; round(Lm6, 2)

Lm <- Lu/Nu
round(Lm, 2)


# 6. Radio de longitud de cursos
RL2 <- Lm2/Lm1 ; round(RL2, 2)
RL3 <- Lm3/Lm2 ; round(RL3, 2)
# RL4 <- Lm4/Lm3 ; round(RL4, 2)
# RL5 <- Lm5/Lm4 ; round(RL5, 2)
# RL6 <- Lm6/Lm5 ; round(RL6, 2)

RL <- mean(c(RL2, RL3))#, RL4))#, RL5, RL6)
round(RL, 2)


# 7. Radio de bifurcacion
Rb1 <- N1/N2 ; round(Rb1, 2)
Rb2 <- N2/N3 ; round(Rb2, 2)
# Rb3 <- N3/N4 ; round(Rb3, 2)
# Rb4 <- N4/N5 ; round(Rb4, 2)
# Rb5 <- N5/N6 ; round(Rb5, 2)

Rb <- mean(c(Rb1, Rb2))#, Rb3))#, Rb4, Rb5)
round(Rb, 2)


# 8. Coeficiente Rho
Rho1 <- RL2/Rb1 ; round(Rho1, 2)
Rho2 <- RL3/Rb2 ; round(Rho2, 2)
Rho3 <- RL4/Rb3 ; round(Rho3, 2)
# Rho4 <- RL5/Rb4 ; round(Rho4, 2)
# Rho5 <- RL6/Rb5 ; round(Rho5, 2)

Rho <- RL/Rb
round(Rho, 2)

# fin ---




# Calculo parametros Forma de la Cuenca ----

# 9. Longitud
Lb <- gLength(linea.longitud.de.la.cuenca)/1000
round(Lb, 2)


# 10. Area
A <- gArea(cuenca)/1000000
round(A, 2)


# 11. Ancho promedio 
Ap <- A/Lb
round(Ap, 2)


# 12. Perimetro
P <- gLength(cuenca)/1000
round(P, 2)


# 13. Diametro
D <- sqrt(4*A/pi)
round(D, 2)


# 14. Factor de forma
Ff <- A/(Lb^2)
round(Ff, 2)


# 15. Radio de elongacion
Re <- (2/Lb)*(A/pi)^0.5
round(Re, 2)


# 16. Radio de circularidad
Rc <- (4*pi*A)/(P^2)
round(Rc, 2)


# 17. Coeficiente de compacidad
Cc <- P/(2*sqrt(pi*A))
round(Cc, 2)

# fin ---




# Calculo parametros Analisis de textura de drenaje ----

# 18. Densidad o frecuencia de cursos de agua
Fs <- Nu/A
round(Fs, 2)


# 19. Textura de drenaje 
Dt <- Nu/P
round(Dt, 2)


# 20. Densidad de drenaje
Dd <- Lu/A
round(Dd, 2)


# 21. Intensidad de drenaje
Di <- Fs/Dd
round(Di, 2)


# 22. Numero de infiltración
If <- Fs*Dd
round(If, 2)


# 23. Longitud del flujo terrestre
Lo <- 1/(2*Dd)
round(Lo, 2)


# 24. Coeficiente de masividad
Zm <- cellStats(dem.cuenca, stat='mean', na.rm=TRUE)
Cm <- Zm/A
round(Cm, 2)


# 25. Constante de mantenimiento del canal
# Eliminado por: Some of the morphometric parameters were excluded...
# as they depend totally on some other parameters which are already...
# included (e.g., constant of channel maintenance is the inverse of ...
# drainage density and was therefore excluded) (Fenta et al., 2017).

# C <- 1/Dd
# round(C, 2)

# fin ---




# Calculo parametros Relieve ----

# 26. Altura media (Calculada en parametro 24)
round(Zm, 2)


# 27. Altura de la salida de la cuenca 
Zmin <- extract(dem.cuenca, punto.desembocadura)
round(Zmin, 2)


# 28. Máxima altura de la cuenca
Zmax <- maxValue(dem.cuenca)
round(Zmax, 2)


# 29. Relieve total de la cuenca 
H <- Zmax-Zmin
round(H, 2)


# 30. Pendiente media del curso de agua principal
db <- db_con_pixeles_identificados_y_ordenados(pendiente.cuenca, red.hidrica, # escoger un pixel de la parte inferior!
                                               error_de_distancia_respecto_a_pixel_inicial = 20,
                                               mostrar_plot_con_ordenamiento_de_pixeles=TRUE)

SL <- pendiente_media_para_curso_de_agua_principal(db, numero_de_pixeles_a_promediar = 4)
round(SL, 2)


# 31. Pendiente media de la cuenca
Sm <- cellStats(pendiente.cuenca, stat='mean', na.rm=TRUE)
round(Sm, 2)


# 32. Radio del relieve
Rh <- (H/1000)/Lb
round(Rh, 2)


# 33. Relieve relativo
Rhp <- (H/1000)*(100/P)
round(Rhp, 2)

# 34. Numero de resistencia
Rn <- (H/1000)*Dd
round(Rn, 2)


# 35. Indice de diseccion
Dis <- H/Zmax
round(Dis, 2)

# fin ---




# Calculo Tiempo de Concentracion ----

# Modelo de William (1922): para cuencas con superficie menor a 129.5 km2
Tc <- (60*Lb*A^0.4)/(D*Sm^0.2)
round(Tc, 2)

# fin ---
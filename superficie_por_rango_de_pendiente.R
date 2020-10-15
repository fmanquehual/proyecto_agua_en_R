library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(cowplot)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/proyecto_agua_en_R/')
source('funcion_filtro_curso_de_agua.R')
source('funcion_db_con_pixeles_identificados_y_ordenados.R')


# Lectura de capas ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/')
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')

nombre.archivo.cuenca <- 'VectorCuencaPadreCasas'
cuenca <- readOGR('.', nombre.archivo.cuenca)
red.hidrica <- readOGR('.', 'VectorRedPadreCasas')

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')
punto.desembocadura <- readOGR('.', 'punto_desembocadura_de_interes_en_Rio_Quepe_utm18s')

orden.maximo <- max(red.hidrica@data$strahler) ; orden.maximo
curso.de.agua.principal <- filtro_curso_de_agua(red.hidrica, orden.maximo)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')
pendiente <- raster('pendiente_marco_trabajo_Padre_Las_Casas_utm18s.tif')
pendiente.clip0 <- crop(pendiente, cuenca)
pendiente.clip <- mask(pendiente.clip0, cuenca)

# fin ---




# Reclasificacion ----

# pendiente 
minValue(pendiente.clip)
maxValue(pendiente.clip)

intervalo <- 5
rangos <- seq(0, maxValue(pendiente.clip), by=5) ; rangos
if(rangos[length(rangos)]<maxValue(pendiente.clip)){rangos <- c(rangos, rangos[length(rangos)]+intervalo)}

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
            rangos[10], rangos[11], intervalo*10)#,
            # rangos[11], rangos[12], intervalo*11,
            # rangos[12], rangos[13], intervalo*12)
pre.m1 

m1 <- matrix(pre.m1, ncol=3, byrow=TRUE) ; m1
reclas.pendiente <- reclassify(pendiente.clip, m1, include.lowest=TRUE)

plot(reclas.pendiente)
hist(reclas.pendiente, xlab = 'Pendiente (%)')

# fin ---




# Raster a poligonos ----

maxValue(reclas.pendiente)
pendiente.poligonos <- rasterToPolygons(reclas.pendiente, dissolve = TRUE)
colnames(pendiente.poligonos@data) <- 'rango_pendiente'
plot(pendiente.poligonos, axes = TRUE)

head(pendiente.poligonos@data)
dim(pendiente.poligonos@data)

area.total <- gArea(pendiente.poligonos)/1000000
pendiente.poligonos@data$area_km2 <- gArea(pendiente.poligonos, byid=TRUE)/1000000
pendiente.poligonos@data$area_en_porcentaje <- (pendiente.poligonos@data$area_km2*100)/area.total

# fin ---




# Plot ----

# pendiente de la cuenca
db <- pendiente.poligonos@data
db <- db[order(db$rango_pendiente),]

db$nombre.rangos <- paste(m1[,1], m1[,3], sep = ' a ')
db

area_vs_pendiente <- ggplot(db, aes(x=rango_pendiente, y=area_en_porcentaje))+
  geom_col() +
  labs(x='Pendiente (%)', y='Área (%)', title='Cuenca') +
  scale_x_discrete(limit = db$rango_pendiente,
                   labels = db$nombre.rangos) +
  scale_y_continuous(breaks=seq(0, 100, 10)) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"))


# pendiente del curso principal
plot(reclas.pendiente)
plot(red.hidrica, add=TRUE)
plot(punto.desembocadura, pch=16, col='red', add=TRUE)

db2 <- db_con_pixeles_identificados_y_ordenados(pendiente, red.hidrica, # comenzar con un pixel inferior
                                         error_de_distancia_respecto_a_pixel_inicial = 20, 
                                         mostrar_plot_con_ordenamiento_de_pixeles = TRUE)

if(nombre.archivo.cuenca=='poligono_cuenca_Rio_Puren_utm18s'){
  id.x <- which(db2$id%in%1:nrow(db2))
  db2 <- db2[id.x,]
  db2$id <- nrow(db2):1
}

colnames(db2)[2] <- 'pendiente'
db2$distancia.respecto.a.desembocadura <- (gLength(curso.de.agua.principal)/nrow(db2))*db2$id
db2

pendiente_y_distancia <- ggplot(db2) +
  geom_line(aes(x=distancia.respecto.a.desembocadura, y=pendiente)) +
  labs(y='Pendiente (%)', x='Distancia respecto a la desembocadura (m)', title='Curso de agua principal') +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"))


# salida
nombre.plot <- paste('area_distancia_y_pendiente_en_porcentaje_', nombre.archivo.cuenca, '.png', sep = '') ; nombre.plot

setwd('C:/Users/Usuario/Dropbox/Proyecto_agua/plots/')

histogramas <- plot_grid(area_vs_pendiente, 
                         pendiente_y_distancia,
                         ncol = 1, nrow = 2)

# png(nombre.plot, width = 720, height = 500, units = "px")
histogramas
# dev.off()

# fin ---
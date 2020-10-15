library(raster)
library(rgdal)
library(rgeos)
library(prettymapr)
library(rasterVis)
library(ggplot2)
library(cowplot)
library(grid)

rm(list=ls())
dev.off()


# Lectura de archivos ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC')
pendiente <- raster('pendiente_marco_trabajo_Rio_Imperial_utm18s.tif')
plot(pendiente)

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/')  # padre las casas
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')

nombre.archivo.cuenca <- 'poligono_cuenca_Estero_Poleco_utm18s'
cuenca <- readOGR('.', nombre.archivo.cuenca)
plot(cuenca, add=TRUE)

red.hidrica <- readOGR('.', 'linea_red_hidrografica_Estero_Poleco_utm18s')
plot(red.hidrica, add=TRUE)

orden.maximo <- max(red.hidrica@data$strahler)
id <- which(red.hidrica@data$strahler==orden.maximo)
curso.de.agua.principal <- red.hidrica[id,]
plot(curso.de.agua.principal, col='cyan', lwd=2, add=TRUE)

punto.desembocadura <- readOGR('.', 'punto_desembocadura_de_interes_en_Rio_Imperial_utm18s')
plot(punto.desembocadura, pch=16, col='red', add=TRUE)

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/cuencas_buffer_y_clips/')
# cuenca.buffer <- readOGR('.', 'VectorCuencaPadreCasas_buffer_de_1000_m')
# plot(cuenca.buffer, add=TRUE)

# fin ---




# clip ----

pendiente.clip0 <- crop(pendiente, cuenca)
pendiente.clip <- mask(pendiente.clip0, cuenca)
plot(pendiente.clip)

# pendiente.curso.de.agua.principal <- mask(pendiente, curso.de.agua.principal)
# plot(pendiente.curso.de.agua.principal)
# plot(curso.de.agua.principal, col='cyan', lwd=2, add=TRUE)

# zoom(pendiente.curso.de.agua.principal, ext=drawExtent())

# fin ---




# plot ----

setwd('C:/Users/Usuario/Dropbox/Proyecto_agua/mapas/')

# mapa
nombre.mapa <- paste('mapa_de_pendiente_', nombre.archivo.cuenca, '.png', sep = '') ; nombre.mapa

paleta.colores <- hcl.colors(12, palette = "inferno")
myTheme <- rasterTheme(region = paleta.colores)

mapa <- levelplot(pendiente.clip, 
                  margin = list(FUN = median, axis = gpar(col = 'black', fontsize = 10)),
                   colorkey=myTheme) +
         layer(sp.polygons(red.hidrica, lwd=2, col='#0fd8ee')) +
         layer(sp.polygons(punto.desembocadura, cex=1.5, pch=16, col='red'))

# png(nombre.mapa, width = 720, height = 720, units = "px")

mapa

dev.off()


# # histogramas
# setwd('C:/Users/Usuario/Dropbox/Proyecto_agua/plots/')
# 
# 
# nombre.histograma <- paste('histogramas_de_pendiente_', nombre.archivo.cuenca, '.png', sep = '') ; nombre.histograma
# 
# histograma.cuenca0 <- hist(pendiente.clip, breaks=30)
# histograma.cuenca1 <- data.frame(counts= histograma.cuenca0$counts,breaks = histograma.cuenca0$mids)
# histograma.cuenca <- ggplot(histograma.cuenca1, aes(x = breaks, y = counts)) + 
#         geom_bar(stat = "identity") +
#         labs(y='Frecuencia', x='Pendiente (%)', title='Cuenca') +
#         theme_bw() +
#         theme(text = element_text(size=14), panel.spacing = unit(1, "lines"))
# 
# histograma.curso.de.agua.principal0 <- hist(pendiente.curso.de.agua.principal, breaks=30)
# histograma.curso.de.agua.principal1 <- data.frame(counts= histograma.curso.de.agua.principal0$counts, breaks = histograma.curso.de.agua.principal0$mids)
# histograma.curso.de.agua.principal <- ggplot(histograma.curso.de.agua.principal1, aes(x = breaks, y = counts)) + 
#         geom_bar(stat = "identity") +
#         labs(y='Frecuencia', x='Pendiente (%)', title = 'Curso de agua principal') +
#         theme_bw() +
#         theme(text = element_text(size=14), panel.spacing = unit(1, "lines"))
# 
# 
# 
# histogramas <- plot_grid(histograma.cuenca, 
#                          histograma.curso.de.agua.principal,
#                          ncol = 1, nrow = 2)
# 
# # png(nombre.mapa, width = 720, height = 500, units = "px")
# 
# histogramas
# 
# dev.off()
# 
# # fin ---
# 
# 
# 
# 
# # otros ----
# 
# # plot(pendiente.clip, col=hcl.colors(12, palette = "inferno"))
# # plot(cuenca, border='red', add=TRUE)
# # plot(red.hidrica, col='#0fd8ee', add=TRUE)
# # plot(punto.desembocadura, pch=16, cex=1.5, col='#0fd8ee', add=TRUE)
# # addscalebar(style = 'ticks', linecol = 'black', label.col = 'black', pos = 'bottomleft', plotepsg = 32718) 
# # addnortharrow(pos = "topright", cols = c("black", "black"), border = 'black', text.col = 'black', scale = 0.7) 
# # 
# # legend("bottomright", title=NULL, text.font = 2, 
# #        legend = c('Cuenca', 'Red hidrográfica', 'Desembocadura'), 
# #        fill=c('transparent', NA, NA), 
# #        border = c('red', NA, NA),
# #        lty=c(NA, 1, NA),
# #        pch = c(NA, NA, 16),
# #        col = c(NA, '#0fd8ee', '#0fd8ee'),
# #        merge = TRUE,
# #        horiz=FALSE, cex=0.8, ncol = 1, bg='transparent') 

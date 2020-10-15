library(rgdal)
library(rgeos)

rm(list=ls())
dev.off()

#setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/') # padre las casas
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')

valor.buffer <- 5
nombre.capa.i <- 'linea_red_hidrografica_Rio_Puren_utm18s'
red_hidrica_i <- readOGR('.', nombre.capa.i)

orden.maximo <- max(red_hidrica_i@data$strahler)

for (i in 1:orden.maximo) {

  id <- which(red_hidrica_i@data$strahler==i)
  red_hidrica <- red_hidrica_i[id,]
  red_hidrica_buffer <- gBuffer(red_hidrica, width=valor.buffer, byid=TRUE)
  
  nombre.capa.salida <- paste(nombre.capa.i, 'strahler_orden', i, sep = '_')
  
  setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_strahler/')
  writeOGR(red_hidrica_buffer, ".", nombre.capa.salida, driver="ESRI Shapefile",
          overwrite_layer = TRUE)
}

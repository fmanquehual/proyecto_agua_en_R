# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/proyecto_agua_en_R/')
# source('funcion_hipsometria.R') # MEJOR CORRELO MANUALMENTE PARA QUE NO APARESCAN SIMBOLOS RAROS POR LOS TILDES

rm(list=ls())
dev.off()

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/')  # padre las casas
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')
nombre.archivo.cuenca <- 'poligono_cuenca_Estero_Neicuf_utm18s'
cuenca <- readOGR('.', nombre.archivo.cuenca)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')
dem <- raster('DEM_marco_trabajo_Teodoro_Schmidt_utm18s.tif')

dem.cuenca <- mask(dem, cuenca)
plot(dem.cuenca)

setwd('C:/Users/Usuario/Dropbox/Proyecto_agua/plots/')
nombre.plot <- paste('curva_hipsometrica_', nombre.archivo.cuenca, '.png', sep = '') ; nombre.plot

# png(nombre.plot)

hipsometria(dem.cuenca, ylab = "Altitud relativa (m.s.n.m.)", xlab ="Área relativa",
            AddDen = FALSE, absolute = FALSE, col='red', lwd=2)

dev.off()


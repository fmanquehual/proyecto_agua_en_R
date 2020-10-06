# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/proyecto_agua_en_R/')
# source('funcion_hipsometria.R') # MEJOR CORRELO MANUALMENTE PARA QUE NO APARESCAN SIMBOLOS RAROS POR LOS TILDES

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/DEM/')
dem <- raster('DEM_CuencaPadreCasas.tif')

setwd('C:/Users/Usuario/Dropbox/Proyecto_agua/plots/')
png('curva_hipsometrica_CuencaPadreCasas.png')

hipsometria(dem, ylab = "Elevación absoluta (m.s.n.m.)", xlab =expression("Área absoluta" ~ km^2),
            AddDen = TRUE)

dev.off()


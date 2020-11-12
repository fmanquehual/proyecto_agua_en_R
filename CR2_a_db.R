library(raster)
library(rgdal)
library(rgeos)
library(loadeR)
library(visualizeR)
library(convertR)

rm(list=ls())
dev.off()




# Lectura de CR2 ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/CR2/')

variable <- 'tmax' # 't2m', 'pr', 'tmin', 'tmax
# nombre.archivo <- 'CR2MET_pr_v2.0_day_1979_2018_005deg.nc' # (pr)
# nombre.archivo <- 'CR2MET_t2m_v2.0_day_1979_2018_005deg.nc' # (t2m)
# nombre.archivo <- 'CR2MET_tmin_v2.0_day_1979_2018_005deg.nc' # (tmin)
nombre.archivo <- 'CR2MET_tmax_v2.0_day_1979_2020_005deg.nc' # (tmax)

cr2 <- stack(nombre.archivo, varname=variable)
cr2

plot(cr2, 1)

# fin ---




# Lectura capa de cuenca ----

nombre.comuna <- 'Puren' # 'Padre_Las_Casas' # 'Imperial' # 'Teodoro_Schmidt' # 'Puren'
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/') # 'Imperial' # 'Teodoro_Schmidt' # puren
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/') # 'Padre_Las_Casas'

nombre.archivo.cuenca.puren <- 'poligono_cuenca_Rio_Puren_utm18s'
cuenca <- readOGR('.', nombre.archivo.cuenca.puren)
cuenca.wgs84 <- spTransform(cuenca, crs(cr2)) 

plot(cuenca.wgs84, axes=TRUE)

# fin ---




# Clip a cr2-LAND (para verificar lo de loadGridData) ----

cr2.clip <- crop(cr2, cuenca.wgs84, snap='out')

plot(cr2.clip, 12)
plot(cuenca.wgs84, border='red', add=TRUE)

# fin ---



# Preparando datos mensuales ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/CR2/')

latitud <- c(-38.1, -38) # c(-38.89, -38.75) # Padre Las Casas; c(-38.69, -38.55) # Imperial; c(-38.99, -38.85) # Teodoro Schmidt #; c(-38.1, -38) # puren
longitud <- c(-73.09, -72.95) # c(-72.64, -72.45) # Padre Las Casas; c(-73.04, -72.9) # Imperial; c(-73, -72.86) # Teodoro Schmidt #; c(-73.14, -72.95) # puren

# C4R.vocabulary()
er5.datos.mensuales <- loadGridData(dataset = nombre.archivo, 
                                     var = variable,
                                     aggr.d = 'mean',
                                     #aggr.m = "mean",
                                     lonLim = longitud,
                                     latLim= latitud, 
                                     season= 1:12, 
                                     years = 1979:2018,
                                     time = 'DD')

# attr(er5.datos.mensuales0$Variable, "units") <- 'm' # 'evavt'
# er5.datos.mensuales <- udConvertGrid(er5.datos.mensuales0, new.units = "mm") # 'mm' para 'tp' y 'evavt'
# er5.datos.mensuales <- udConvertGrid(er5.datos.mensuales0, new.units = "celsius") # 'C' para 't2m'

spatialPlot(climatology(er5.datos.mensuales, list(FUN = mean, na.rm = T)),
            scales = list(draw = T))

raster.cr2.datos.mensuales <- grid2sp(er5.datos.mensuales)
plot(raster.cr2.datos.mensuales)

# fin ---




# Raster a data frame ----


db.cr2.0 <- raster::as.data.frame(raster.cr2.datos.mensuales, xy=TRUE)
head(db.cr2.0[(1:5)])
dim(db.cr2.0)

cr2.promedio <- colMeans(db.cr2.0)
cr2.promedio[1:5]

db.cr2.00 <- rbind(db.cr2.0, cr2.promedio)
dim(db.cr2.00)

db.cr2 <- db.cr2.00[-(1:(nrow(db.cr2.00)-1)),]
head(db.cr2[(1:5)])
dim(db.cr2)

# fin ---




# Guardando base de datos ----

directorio.principal <- 'C:/Users/Usuario/Documents/Francisco/proyecto_agua/CR2/base_de_datos/'
carpeta.de.interes <- paste0(directorio.principal, nombre.comuna, '/')

setwd(carpeta.de.interes)

nombre.archivo.salida <- paste0(variable, '_', nombre.comuna, '_cr2_bruto.csv')
nombre.archivo.salida

# write.csv(db.cr2, nombre.archivo.salida, row.names = FALSE)

# fin ---
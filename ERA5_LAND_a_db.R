library(raster)
library(rgdal)
library(rgeos)
library(loadeR)
library(visualizeR)
library(convertR)

rm(list=ls())
dev.off()




# Lectura de ERA5-LAND ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/ERA_LAND/')

variable <- 'tp' # 2m temperature (t2m), total precipitation (tp), potential evaporation (pev)
nombre.archivo <- 'adaptor.mars.internal-1603772847.3626816-2977-32-dd28a542-ac6e-4a60-a62d-111c003c5901.nc' # (tp, pev)
# nombre.archivo <- 'adaptor.mars.internal-1603776645.8702145-9028-19-8c2dd33e-218f-4735-8a94-1c57e2e7b114.nc' # (t2m, evavt)
era5 <- stack(nombre.archivo, varname=variable)
era5

plot(era5, 1)

# fin ---




# Lectura capa de cuenca ----

nombre.comuna <- 'Padre_Las_Casas' # 'Imperial' # 'Teodoro_Schmidt' # puren
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/') # 'Imperial' # 'Teodoro_Schmidt' # puren
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/') # 'Padre_Las_Casas'

nombre.archivo.cuenca.puren <- 'VectorCuencaPadreCasas'
cuenca <- readOGR('.', nombre.archivo.cuenca.puren)
cuenca.wgs84 <- spTransform(cuenca, crs(era5)) 

plot(cuenca.wgs84, axes=TRUE)

# fin ---




# Clip a ERA5-LAND (para verificar lo de loadGridData) ----

era5.clip <- crop(era5, cuenca.wgs84, snap='out')

plot(era5.clip, 1)
plot(cuenca.wgs84, border='red', add=TRUE)

# fin ---



# Preparando datos mensuales ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/ERA_LAND/')

latitud <- c(-38.94, -38.75) #; c(-38.74, -38.56) #; c(-39.05, -38.85) # Teodoro Schmidt #; c(-38.1, -38) # puren
longitud <- c(-72.64, -72.45) #; c(-73.05, -72.86) #; c(-73.05, -72.86) # Teodoro Schmidt #; c(-73.14, -72.95) # puren

# C4R.vocabulary()
er5.datos.mensuales0 <- loadGridData(dataset = nombre.archivo, 
                                 var = variable,
                                 aggr.d = 'mean',
                                 aggr.m = "mean",
                                 lonLim = longitud,
                                 latLim= latitud, 
                                 season= 1:12, 
                                 years = 1981:2020,
                                 time = 'DD')

er5.datos.mensuales <- udConvertGrid(er5.datos.mensuales0, new.units = "mm") # 'mm' para 'tp' y 'pev'
# er5.datos.mensuales <- udConvertGrid(er5.datos.mensuales0, new.units = "celsius") # 'C' para 't2m'

spatialPlot(climatology(er5.datos.mensuales, list(FUN = mean, na.rm = T)),
            scales = list(draw = T))

raster.er5.datos.mensuales <- grid2sp(er5.datos.mensuales)
plot(raster.er5.datos.mensuales)

# fin ---




# Raster a data frame ----


db.era5 <- raster::as.data.frame(raster.er5.datos.mensuales, xy=TRUE)
head(db.era5[,(1:5)])
dim(db.era5)

# fin ---




# Guardando base de datos ----

directorio.principal <- 'C:/Users/Usuario/Documents/Francisco/proyecto_agua/ERA_LAND/bases_de_datos/'
carpeta.de.interes <- paste0(directorio.principal, nombre.comuna, '/')

setwd(carpeta.de.interes)

nombre.archivo.salida <- paste0(variable, '_', nombre.comuna, '_era5_land_bruto.csv')
nombre.archivo.salida

# write.csv(db.era5, nombre.archivo.salida, row.names = FALSE)

# fin ---
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

variable <- 't2m' # 2m temperature (t2m), total precipitation (tp), potential evaporation (pev)
# nombre.archivo <- 'adaptor.mars.internal-1603772847.3626816-2977-32-dd28a542-ac6e-4a60-a62d-111c003c5901.nc' # (tp, pev)
nombre.archivo <- 'adaptor.mars.internal-1603776645.8702145-9028-19-8c2dd33e-218f-4735-8a94-1c57e2e7b114.nc' # (t2m, evavt)
era5 <- stack(nombre.archivo, varname=variable)
era5

plot(era5, 1)

# fin ---




# Lectura capa de cuenca ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_FFMC/')

nombre.archivo.cuenca.puren <- 'poligono_cuenca_Rio_Puren_utm18s'
cuenca.puren <- readOGR('.', nombre.archivo.cuenca.puren)
cuenca.puren.wgs84 <- spTransform(cuenca.puren, crs(era5)) 

plot(cuenca.puren.wgs84, axes=TRUE)

# fin ---




# Clip a ERA5-LAND (para verificar lo de loadGridData) ----

era5.clip <- crop(era5, cuenca.puren.wgs84, snap='out')

plot(era5.clip, 1)
plot(cuenca.puren.wgs84, border='red', add=TRUE)

# fin ---



# Preparando datos mensuales ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/ERA_LAND/')

latitud <- c(-48.5, -45.5) # puren
longitud <- c(-74, -71) # puren

# C4R.vocabulary()
er5.datos.mensuales0 <- loadGridData(dataset = nombre.archivo, 
                                 var = variable,
                                 aggr.d = 'mean',
                                 aggr.m = "mean",
                                 lonLim = c(-73.14, -72.95),
                                 latLim= c(-38.1, -38), 
                                 season= 1:12, 
                                 years = 1981:2020,
                                 time = 'DD')

# er5.datos.mensuales <- udConvertGrid(er5.datos.mensuales0, new.units = "mm") # 'mm' para 'tp' y 'pev'
er5.datos.mensuales <- udConvertGrid(er5.datos.mensuales0, new.units = "celsius") # 'C' para 't2m'

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

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/ERA_LAND/bases_de_datos/')
nombre.archivo.salida <- paste0(variable, '_puren_era5_land_bruto.csv')

# write.csv(db.era5, nombre.archivo.salida, row.names = FALSE)

# fin ---
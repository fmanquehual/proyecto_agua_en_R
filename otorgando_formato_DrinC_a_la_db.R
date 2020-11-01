library(xlsx)
library(lubridate)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/proyecto_agua_en_R/')
source('funcion_nombre_de_columnas_a_fechas.R')




# Lectura de datos ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/ERA_LAND/bases_de_datos/')

variable <- 'pev'
nombre.archivo <- 'pev_puren_era5_land_bruto.csv'
db.completo <- read.csv(nombre.archivo)
head(db.completo)

# fin ---




for (k in 1:nrow(db.completo)) {
# k <- 1  
  formato.matriz <- c()
  db <- db.completo[k,]
  
  # Conservando anhos completos ----
  
  nombres.como.fechas <- nombre_de_columnas_a_fechas(db, eliminar_ultimas_dos_columnas = TRUE)
  anhos <- year(nombres.como.fechas)
  anhos.unicos <- unique(year(nombres.como.fechas))
  
  anhos.completos <- c()
  
  for(i in 1:length(anhos.unicos)) {
      
    longitud.anho.i <- anhos[anhos==anhos.unicos[i]]
    
    if(length(longitud.anho.i)==12){anhos.completos <- c(anhos.completos, anhos.unicos[i])
      } else(next)
    
  }
  
  columnas.a.conservar <- which(anhos%in%anhos.completos)
  db2 <- db[,columnas.a.conservar]
  
  # fin ---
  
  
  
  
  # Identificando columnas 
  
  nombres.como.fechas.2 <- nombre_de_columnas_a_fechas(db2)
  anhos.2 <- year(nombres.como.fechas.2)
  anhos.unicos.2 <- unique(year(nombres.como.fechas.2))
  
  
  for (j in 2:length(anhos.unicos.2)) {
    # j <- 2
      
    anho.i.2 <- anhos.unicos.2[((j-1):j)]
    id.columnas <- which(anhos.2%in%anho.i.2)
    
    # fin ---
    
    
    
    
    # Elaborando matriz de datos ----
    
    meses.anho.hidrologico <- c(10:(10+11)) # septiembre a agosto
    
    db.anho.i <- db2[,id.columnas]
    db.anho.hidrologico <- db.anho.i[,meses.anho.hidrologico]
    
    nombre.meses.anho.hidrologico.hemisferio.norte <- c(
      'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May',
      'Jun', 'Jul', 'Aug', 'Sep')
    
    colnames(db.anho.hidrologico) <- nombre.meses.anho.hidrologico.hemisferio.norte
    db.anho.hidrologico <- cbind(db.anho.hidrologico, Annual = rowSums(db.anho.hidrologico))
    
    anho.1 <- anho.i.2[1]
    anho.2 <- anho.i.2[2]
    
    anho.2.split <- unlist(strsplit(as.character(anho.2), ""))[3:4]
    anho.2.abreviado <- paste0('-', anho.2.split[1], anho.2.split[2])
    formato.fecha <- paste0(anho.1, anho.2.abreviado)
    
    db.anho.hidrologico$'Hydrological year' <- formato.fecha
    formato.matriz0 <- db.anho.hidrologico[,c(ncol(db.anho.hidrologico), 1:13)]
  
    formato.matriz <- rbind(formato.matriz, formato.matriz0)
  }

  # Salida
  
  formato <- gsub('csv', 'xlsx', nombre.archivo)
  nombre.archivo.de.salida0 <- paste0('depurado_pixel_', k)
  nombre.archivo.de.salida <- gsub('bruto', nombre.archivo.de.salida0, formato)
  
  write.xlsx(formato.matriz, file = nombre.archivo.de.salida,
             sheetName = variable, row.names = FALSE)
  
}

# fin ---
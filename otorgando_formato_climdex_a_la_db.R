library(xlsx)
library(lubridate)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/proyecto_agua_en_R/')
source('funcion_nombre_de_columnas_a_fechas.R')




# Lectura de datos ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/CR2/base_de_datos/Puren/temperatura_minima/')

variable <- 'tmin' # 'tmin' # 'tmax' # 't2m' # 'pr'
nombre.archivo <- 'tmin_Puren_cr2_bruto.csv'
db <- read.csv(nombre.archivo)
# head(db)
dim(db)

# fin ---




# Preparando db ----

for (k in 1:nrow(db)) {
  # k <- 1
  
  # Filtro por pixel (fila)
  
  formato.matriz <- c()
  db.k <- db[k,]
  
  
  # Conservando anhos completos 
  
  nombres.como.fechas <- nombre_de_columnas_a_fechas(db.k, eliminar_ultimas_dos_columnas = TRUE)
  anhos <- year(nombres.como.fechas)
  anhos.unicos <- unique(year(nombres.como.fechas))
  
  anhos.completos <- c()
  
  for(i in 1:length(anhos.unicos)) {
    # i <- 1  
    longitud.anho.i <- anhos[anhos==anhos.unicos[i]]
    
    if(length(longitud.anho.i)==365 | length(longitud.anho.i)==366){anhos.completos <- c(anhos.completos, anhos.unicos[i])
    } else(next)
    
  }
  
  columnas.a.conservar <- which(anhos%in%anhos.completos)
  db.k2 <- db.k[,columnas.a.conservar]
  
  
  # Identificando columnas 
  
  nombres.como.fechas.2 <- nombre_de_columnas_a_fechas(db.k2)
  anhos.2 <- year(nombres.como.fechas.2)
  anhos.unicos.2 <- unique(year(nombres.como.fechas.2))
  
  db.depurado <- c()
  for (j in 1:length(anhos.unicos.2)) {
    # j <- 1
    
    # Filtro 
    anho.i.2 <- anhos.unicos.2[j]
    id.columnas <- which(anhos.2%in%anho.i.2)
    
    
    # Elaborando  base de datos 
    db.anho.i <- db.k2[,id.columnas]
    valores <- as.numeric(db.anho.i)
    
    nombre.fechas.depurada <- nombre_de_columnas_a_fechas(db.anho.i, eliminar_ultimas_dos_columnas = FALSE)
    db.depurado0 <- data.frame(fecha=nombre.fechas.depurada, valor=valores)
    
    db.depurado <- rbind(db.depurado, db.depurado0)
  }
  
  dim(db.k2)
  dim(db.depurado)

    
  # Salida
  
  nombre.archivo.salida0 <- paste0('depurado')
  nombre.archivo.salida <- gsub('bruto', nombre.archivo.salida0, nombre.archivo)
  
  write.csv(db.depurado, file = nombre.archivo.salida, row.names = FALSE)

}

# fin ---
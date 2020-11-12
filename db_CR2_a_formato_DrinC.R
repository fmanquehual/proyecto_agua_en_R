rm(list=ls())
dev.off()


# Lectura de datos ----

variable <- 'pr' # 't2m', 'pr'
comuna <- 'Puren' # 'Teodoro_Schmidt' # 'Padre_Las_Casas' # 'Imperial'
directorio.principal <- 'C:/Users/Usuario/Documents/Francisco/proyecto_agua/CR2/base_de_datos/'

setwd(paste0(directorio.principal, comuna, '/', 'precipitacion/'))
db <- read.csv(list.files(pattern = 'pr')[2])

# setwd(paste0(directorio.principal, comuna, '/', 'temperatura_media/'))
# db <- read.csv(list.files(pattern = variable)[2])

# setwd(paste0(directorio.principal, comuna, '/', 'temperatura_maxima/'))
# db <- read.csv(list.files(pattern = 'tmax')[2])
 
# setwd(paste0(directorio.principal, comuna, '/', 'temperatura_minima/'))
# db <- read.csv(list.files(pattern = 'tmin')[2])

# fin ---


# Conservando anhos completos 

nombres.como.fechas <- as.Date(db$fecha) 
anhos <- year(nombres.como.fechas)
anhos.unicos <- unique(year(nombres.como.fechas))

anhos.completos <- c()

for(i in 1:length(anhos.unicos)) {
  # i <- 1  
  longitud.anho.i <- anhos[anhos==anhos.unicos[i]]
  
  if(length(longitud.anho.i)==365 | length(longitud.anho.i)==366){anhos.completos <- c(anhos.completos, anhos.unicos[i])
  } else(next)
  
}

filas.a.conservar <- which(anhos%in%anhos.completos)
db2 <- db[filas.a.conservar,]


# Identificando columnas 

nombres.como.fechas.2 <- as.Date(db2$fecha) 
db2$anho <- year(db2$fecha)
db2$mes <- month(db2$fecha)
anhos.2 <- year(nombres.como.fechas.2)
anhos.unicos.2 <- unique(year(nombres.como.fechas.2))

formato.matriz <- c()  
for (j in 2:length(anhos.unicos.2)) {
    # j <- 2
    
    anho.i.2 <- anhos.unicos.2[((j-1):j)]
    id.filas <- which(anhos.2%in%anho.i.2)
    
    db.anho.i <- db2[id.filas,]
    db.anho.i$id.anho[db.anho.i$anho==anho.i.2[1]] <- 1
    db.anho.i$id.anho[db.anho.i$anho==anho.i.2[2]] <- 2
    db.anho.i$id <- paste(db.anho.i$mes, db.anho.i$id.anho, sep = '_')
    
    id.anho.a.descartar.1 <- paste(1:9, '1', sep = '_')
    id.anho.a.descartar.2 <- paste(10:12, '2', sep = '_')
    id.anho.a.descartar <- c(id.anho.a.descartar.1, id.anho.a.descartar.2)
    
    id.filas.a.descartar <- which(db.anho.i$id%in%id.anho.a.descartar)
    db.anho.j <- db.anho.i[-id.filas.a.descartar,]
    
    # fin ---
    
    
    
    
    # Elaborando matriz de datos ----
    
    nombre.meses.anho.hidrologico <- c('Oct', 'Nov', 'Dec', 'Jan',
                                       'Feb', 'Mar', 'Apr', 'May',
                                       'Jun', 'Jul', 'Aug', 'Sep')
    
    db.anho.hidrologico <- data.frame(mes=unique(db.anho.j$mes), nombre.mes=nombre.meses.anho.hidrologico)
    db.anho.j.2 <- merge(db.anho.j, db.anho.hidrologico, by='mes')
    
    valores.j <- as.numeric(tapply(db.anho.j.2$valor, db.anho.j.2$nombre.mes, sum))
    
    if(variable == 'tmax' | variable == 'tmin' | variable == 't2m'){c(
        
      mes.j <- names(tapply(db.anho.j.2$valor, db.anho.j.2$nombre.mes, mean)),
      valores.j <- as.numeric(tapply(db.anho.j.2$valor, db.anho.j.2$nombre.mes, mean))
      
      )} else(c(
        
        mes.j <- names(tapply(db.anho.j.2$valor, db.anho.j.2$nombre.mes, sum)),
        valores.j <- as.numeric(tapply(db.anho.j.2$valor, db.anho.j.2$nombre.mes, sum))
        
        ))
    
    db.anho.hidrologico <- as.data.frame(rbind(valores.j))
    colnames(db.anho.hidrologico) <- nombre.meses.anho.hidrologico
    row.names(db.anho.hidrologico) <- 1
    
    if(variable == 'tmax' | variable == 'tmin' | variable == 't2m'){ 
      
      db.anho.hidrologico <- cbind(db.anho.hidrologico, Annual = rowMeans(db.anho.hidrologico))
      
    } else( db.anho.hidrologico <- cbind(db.anho.hidrologico, Annual = rowSums(db.anho.hidrologico)) )
    
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
  
  nombre.archivo.de.salida <- paste0(variable, '_', comuna, '_formato_DrinC.xlsx')
  nombre.archivo.de.salida
  
  write.xlsx(formato.matriz, file = nombre.archivo.de.salida,
             sheetName = variable, row.names = FALSE)
  
  
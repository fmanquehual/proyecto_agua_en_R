library(xlsx)

rm(list=ls())
dev.off()


setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR')
source('funcion_incorporacion_de_fechas_perdidas.R')
source('funcion_fechas_del_anho.R')



# Lectura de datos ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/ERA_LAND/bases_de_datos/')

variable <- 'caudal'
rango.de.anhos.de.interes <- 1981:2019

nombre.archivo <- 'caudal_diario_Rio_Puren_en_Tranaman.csv'
db <- read.csv(nombre.archivo)
head(db)
dim(db)

menores.a.10 <- 1:9

id.menores.a.10 <- which(db$dia%in%menores.a.10)
db$dia[id.menores.a.10] <- paste0('0', db$dia[id.menores.a.10])

id.menores.a.10 <- which(db$mes%in%menores.a.10)
db$mes[id.menores.a.10] <- paste0('0', db$mes[id.menores.a.10])
db$fecha <- paste(db$agno, db$mes, db$dia, sep = '-')

head(db)

# fin ---




# Incorporando fechas faltantes ----

db.inicio <- data.frame(agno=rango.de.anhos.de.interes[1], mes='01', dia='01', valor=NA)
db.inicio$fecha <- paste(db.inicio$agno, db.inicio$mes, db.inicio$dia, sep = '-')

db.fin <- data.frame(agno=rango.de.anhos.de.interes[length(rango.de.anhos.de.interes)], 
                     mes='12', dia='31', valor=NA)
db.fin$fecha <- paste(db.fin$agno, db.fin$mes, db.fin$dia, sep = '-')

id.fecha.inicio <- which(db$fecha==db.inicio$fecha)
id.fecha.fin <- which(db$fecha==db.fin$fecha)

if(length(id.fecha.inicio)==0){db <- rbind(db, db.inicio)}
if(length(id.fecha.fin)==0){db <- rbind(db, db.fin)}

tail(db)

db <- db[,c('fecha', 'agno', 'mes', 'dia', 'valor')]
db <- incorporacion_de_fechas_perdidas(db)
db$agno <- year(db$fecha)
db$mes <- month(db$fecha)
db$dia <- day(db$fecha)

id.menores.a.10 <- which(db$dia%in%menores.a.10)
db$dia[id.menores.a.10] <- paste0('0', db$dia[id.menores.a.10])

id.menores.a.10 <- which(db$mes%in%menores.a.10)
db$mes[id.menores.a.10] <- paste0('0', db$mes[id.menores.a.10])
db$fecha <- paste(db$agno, db$mes, db$dia, sep = '-')

head(db)
# View(db)

# fin ---




# Calculando Q mensual ----

db.QMM <- c()
formato.matriz <- c()
anhos.unicos <- unique(db$agno)
meses <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

for (i in 1:length(anhos.unicos)) {
  # i <- 1
  anho.i <- anhos.unicos[i]
  
  db.QMM0 <- data.frame(agno=anho.i, mes=meses, 
                                     mean=c(mean(db$valor[db$agno==anho.i & db$mes=='01'], na.rm = TRUE),
                                            mean(db$valor[db$agno==anho.i & db$mes=='02'], na.rm = TRUE),
                                            mean(db$valor[db$agno==anho.i & db$mes=='03'], na.rm = TRUE),
                                            mean(db$valor[db$agno==anho.i & db$mes=='04'], na.rm = TRUE),
                                            mean(db$valor[db$agno==anho.i & db$mes=='05'], na.rm = TRUE),
                                            mean(db$valor[db$agno==anho.i & db$mes=='06'], na.rm = TRUE),
                                            mean(db$valor[db$agno==anho.i & db$mes=='07'], na.rm = TRUE),
                                            mean(db$valor[db$agno==anho.i & db$mes=='08'], na.rm = TRUE),
                                            mean(db$valor[db$agno==anho.i & db$mes=='09'], na.rm = TRUE),
                                            mean(db$valor[db$agno==anho.i & db$mes=='10'], na.rm = TRUE),
                                            mean(db$valor[db$agno==anho.i & db$mes=='11'], na.rm = TRUE),
                                            mean(db$valor[db$agno==anho.i & db$mes=='12'], na.rm = TRUE)))
  
  db.QMM <- rbind(db.QMM, db.QMM0)
}

head(db.QMM)

id.anhos.de.interes <- which(db.QMM$agno%in%rango.de.anhos.de.interes)
db.QMM.anhos.de.interes <- db.QMM[id.anhos.de.interes,]

head(db.QMM.anhos.de.interes)
tail(db.QMM.anhos.de.interes)

# fin ---




# Identificando columnas 

anhos.2 <- db.QMM.anhos.de.interes$agno
anhos.unicos.2 <- unique(db.QMM.anhos.de.interes$agno)


for (j in 2:length(anhos.unicos.2)) {
  # j <- 2
  
  anho.i.2 <- anhos.unicos.2[((j-1):j)]
  id.filas <- which(anhos.2%in%anho.i.2)
  
  # fin ---
  
  
  
  
  # Elaborando matriz de datos ----
  
  meses.anho.hidrologico <- c(10:(10+11)) # septiembre a agosto
  
  db.anho.i <- db.QMM.anhos.de.interes[id.filas,]
  db.anho.hidrologico0 <- db.anho.i[meses.anho.hidrologico,]
  valores.anho.hidrologico <- db.anho.hidrologico0$mean
    
  nombre.meses.anho.hidrologico.hemisferio.norte <- c(
    'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May',
    'Jun', 'Jul', 'Aug', 'Sep')
  
  db.anho.hidrologico <- data.frame(Oct=valores.anho.hidrologico[1], Nov=valores.anho.hidrologico[2],
                                    Dec=valores.anho.hidrologico[3], Jan=valores.anho.hidrologico[4],
                                    Feb=valores.anho.hidrologico[5], Mar=valores.anho.hidrologico[6],
                                    Apr=valores.anho.hidrologico[7], May=valores.anho.hidrologico[8],
                                    Jun=valores.anho.hidrologico[9], Jul=valores.anho.hidrologico[10],
                                    Aug=valores.anho.hidrologico[11], Sep=valores.anho.hidrologico[12])
  
  if(variable == 't2m' | variable == 'caudal'){ db.anho.hidrologico <- cbind(db.anho.hidrologico, Annual = rowMeans(db.anho.hidrologico, na.rm = TRUE))
  } else( db.anho.hidrologico <- cbind(db.anho.hidrologico, Annual = rowSums(db.anho.hidrologico, na.rm = TRUE)) )
  
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

nombre.archivo.de.salida <- gsub('.csv', '_depurado.xlsx', nombre.archivo)

write.xlsx(formato.matriz, file = nombre.archivo.de.salida,
           sheetName = variable, row.names = FALSE, showNA = FALSE)

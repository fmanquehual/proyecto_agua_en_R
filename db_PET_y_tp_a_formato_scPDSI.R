library(xlsx)

rm(list=ls())
dev.off()




# Definiendo direcciones de carpetas ---- 

numero.de.pixel <- 1
comuna <- 'Imperial'
directorio.principal <- 'C:/Users/Usuario/Documents/Francisco/proyecto_agua/ERA_LAND/bases_de_datos/'
carpeta.de.archivos <- paste0(directorio.principal, comuna, '/', 'pixel_', numero.de.pixel, '/')

setwd(carpeta.de.archivos)

# fin ---




# PET mensual ----

# Lectura de datos

nombre.archivo.parte.1 <- 'PET_'
nombre.archivo.parte.3 <- '.xlsx'

nombre.archivo <- paste0(nombre.archivo.parte.1, 'pixel_', numero.de.pixel, nombre.archivo.parte.3)

pet0 <- read.xlsx(nombre.archivo, 1)
head(pet0)
dim(pet0)
str(pet0)


# Depuracion db

nombre.de.columnas <- pet0[1,]
colnames(pet0) <- nombre.de.columnas
pet <- pet0[-1,-ncol(pet0)]

head(pet)
str(pet)

nombre.meses <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 
                  'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')

pet.mensual0 <- pet[-c(nrow(pet), nrow(pet)-1, 1:8), -c(nrow(pet0)-1, ncol(pet))]
pet.mensual0

colnames(pet.mensual0)
nuevo.orden.columnas <- c('Year', 'Jan.', 'Feb.', 'Mar.', 'Apr.', 'May', 'Jun.',
                          'Jul.', 'Aug.', 'Sep.', 'Oct.', 'Nov.', 'Dec.')
pet.mensual <- pet.mensual0[,nuevo.orden.columnas]
colnames(pet.mensual)[2:ncol(pet.mensual)] <- nombre.meses
head(pet.mensual)
str(pet.mensual)

pet.mensual[,c(2:ncol(pet.mensual))] <- sapply(pet.mensual[,c(2:ncol(pet.mensual))], as.numeric)
pet.mensual$Year <- 1990:2018

head(pet.mensual)


# Formato data frame

db.pet.mensual <- c()
for (i in 2:ncol(pet.mensual)) {
  # i <- ncol(pet.mensual)
  
  pet.mensual.i <- pet.mensual[,c(1, i)]
  colnames(pet.mensual.i)[2] <- 'PE'
  
  if((i-1) <= 9){pet.mensual.i$mes.i <- paste0('0', (i-1))} else(pet.mensual.i$mes.i <- (i-1))
  pet.mensual.i$dia.i <- '01'
  
  pet.mensual.i$fecha <- paste(pet.mensual.i$Year, pet.mensual.i$mes.i, pet.mensual.i$dia.i, sep = '-')
  
  db.pet.mensual <- rbind(db.pet.mensual, pet.mensual.i)

}

db.pet.mensual <- db.pet.mensual[,c('fecha', 'PE')]
db.pet.mensual$fecha <- as.Date(db.pet.mensual$fecha)
db.pet.mensual.ordenado <- db.pet.mensual[order(db.pet.mensual$fecha),]
row.names(db.pet.mensual.ordenado) <- 1:nrow(db.pet.mensual.ordenado)
head(db.pet.mensual.ordenado)

# fin ---




# Precipitacion mensual ----

# Lectura de datos

nombre.archivo.parte.1 <- 'tp_'

nombre.archivo <- paste0(nombre.archivo.parte.1, comuna, '_era5_land_depurado_pixel_', 
                         numero.de.pixel, nombre.archivo.parte.3)
tp <- read.xlsx(nombre.archivo, 1)
head(tp)
dim(tp)
str(tp)

# Depuracion db

tp.mensual0 <- tp[-c(nrow(tp), 1, 1:8),-ncol(tp)]
tp.mensual0

colnames(tp.mensual0)
nuevo.orden.columnas <- c('Hydrological.year', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
tp.mensual <- tp.mensual0[,nuevo.orden.columnas]
colnames(tp.mensual)[2:ncol(tp.mensual)] <- nombre.meses
head(tp.mensual)
str(tp.mensual)

head(tp.mensual)


# Formato data frame

db.tp.mensual <- c()
for (i in 2:ncol(tp.mensual)) {
  # i <- 2
  
  tp.mensual.i <- tp.mensual[,c(1, i)]
  colnames(tp.mensual.i)[2] <- 'P'
  
  tp.mensual.i$Hydrological.year <- 1990:2018
  if((i-1) <= 9){tp.mensual.i$mes.i <- paste0('0', (i-1))} else(tp.mensual.i$mes.i <- (i-1))
  tp.mensual.i$dia.i <- '01'
  
  tp.mensual.i$fecha <- paste(tp.mensual.i$Hydrological.year, tp.mensual.i$mes.i, tp.mensual.i$dia.i, sep = '-')
  
  db.tp.mensual <- rbind(db.tp.mensual, tp.mensual.i)
  
}

db.tp.mensual <- db.tp.mensual[,c('fecha', 'P')]
db.tp.mensual$fecha <- as.Date(db.tp.mensual$fecha)
db.tp.mensual.ordenado <- db.tp.mensual[order(db.tp.mensual$fecha),]
row.names(db.tp.mensual.ordenado) <- 1:nrow(db.tp.mensual.ordenado)
head(db.tp.mensual.ordenado)

# fin ---




# Union de db's ----

head(db.pet.mensual.ordenado)
head(db.tp.mensual.ordenado)

db.tp.pet <- merge(db.tp.mensual.ordenado, db.pet.mensual.ordenado, by='fecha')
dim(db.tp.pet)

nombre.archivo.salida <- paste0('PET_y_tp_pixel_', numero.de.pixel, '.csv')
write.csv(db.tp.pet, nombre.archivo.salida, row.names = FALSE)

# fin ---


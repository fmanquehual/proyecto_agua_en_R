library(xlsx)
library(PerformanceAnalytics)
library(trend)
library(lubridate)
library(ggplot2)

rm(list=ls())
dev.off()




# Definiendo direcciones de carpetas ---- 

# numero.de.pixel <- 1
comuna <- 'Teodoro_Schmidt' # 'Puren' # 'Teodoro_Schmidt' # 'Padre_Las_Casas' # 'Imperial'
directorio.principal <- 'C:/Users/Usuario/Documents/Francisco/proyecto_agua/CR2/base_de_datos/'
# carpeta.de.archivos <- paste0(directorio.principal, comuna, '/', 'pixel_', numero.de.pixel, '/')
# carpeta.de.plots <- paste0(carpeta.de.archivos, 'plots/')

carpeta.de.archivos <- paste0(directorio.principal, comuna, '/indices') # nuevo
carpeta.de.plots <- paste0(directorio.principal, comuna, '/plots/') # nuevo

# fin ---




# SPI mensual ----

setwd(carpeta.de.archivos)

# Lectura de datos

nombre.archivo.parte.1 <- 'SPI_'
# nombre.archivo.parte.2 <- '_mensual_pixel_'
# nombre.archivo.parte.3 <- '.xlsx'
# nombre.archivo <- paste0(nombre.archivo.parte.1, comuna, nombre.archivo.parte.2, numero.de.pixel, nombre.archivo.parte.3)
nombre.archivo <- list.files(patter=nombre.archivo.parte.1)

db.spi0 <- read.xlsx(nombre.archivo, 1)

head(db.spi0)
dim(db.spi0)
str(db.spi0)


# Depuracion db

db.spi00 <- db.spi0[c(2:nrow(db.spi0)), c(1, 2)]
head(db.spi00)
dim(db.spi00)
str(db.spi00)

db.spi00$X1.month.SPI. <- as.POSIXct((as.numeric(db.spi00$X1.month.SPI.)-25569)*86400, tz="GMT", origin="1970-01-01")
colnames(db.spi00) <- c('fecha', 'valor')
db.spi00$mes <- month(db.spi00$fecha)
db.spi00$anho <- year(db.spi00$fecha)

db.spi00$clasificacion <- NA
db.spi00$clasificacion[db.spi00$valor >= 2] <- 'Extremadamente húmedo'
db.spi00$clasificacion[db.spi00$valor < 2 & db.spi00$valor >= 1.5 ] <- 'Muy húmedo'
db.spi00$clasificacion[db.spi00$valor < 1.5 & db.spi00$valor >= 1] <- 'Moderadamente húmedo'
db.spi00$clasificacion[db.spi00$valor < 1 & db.spi00$valor >= 0] <- 'Normal'
db.spi00$clasificacion[db.spi00$valor < 0 & db.spi00$valor > -1] <- 'Casi normal'
db.spi00$clasificacion[db.spi00$valor <= -1 & db.spi00$valor > -1.5] <- 'Moderadamente seco'
db.spi00$clasificacion[db.spi00$valor <= -1.5 & db.spi00$valor > -2] <- 'Muy seco'
db.spi00$clasificacion[db.spi00$valor <= -2] <- 'Extremadamente seco'

niveles <- c('Extremadamente húmedo', 'Muy húmedo', 'Moderadamente húmedo', 'Normal',
             'Casi normal', 'Moderadamente seco', 'Muy seco', 'Extremadamente seco')
db.spi00$clasificacion <- factor(db.spi00$clasificacion, levels = niveles)

table(db.spi00$anho)

id.anhos.a.descartar <- which(as.numeric(table(db.spi00$anho)) != 12)
anhos.a.descartar <- as.numeric(names(table(db.spi00$anho)))[id.anhos.a.descartar]
filas.a.conservar <- which(!db.spi00$anho%in%anhos.a.descartar)
db.spi <- db.spi00[filas.a.conservar,]
head(db.spi)


# Calculo Sen's slope

nombre.meses <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 
                  'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')

db.spi.con.pendiente <- c()

for (i in 1:12) {
  # i <- 1
  
  db.spi.i <- subset(db.spi, mes==i)
  db.spi.i$nombre.mes <- nombre.meses[i]
  
  spi.ts.i <- ts(db.spi.i$valor, start = 1980, freq = 1)
  
  slope.i <- sens.slope(spi.ts.i,) ; slope.i
  valor.pendiente <- round(slope.i$estimates, 3)
  valor.p.value <- round(slope.i$p.value, 3)
  
  db.spi.i$leyenda.valor.pendiente <- paste0('Pendiente = ', valor.pendiente, 
                                             ' (', 'p-value = ', valor.p.value, ')')

  db.spi.con.pendiente <- rbind(db.spi.con.pendiente, db.spi.i)
}

db.spi.con.pendiente$nombre.mes <- factor(db.spi.con.pendiente$nombre.mes, levels = nombre.meses)
head(db.spi.con.pendiente)


# Plot

intercepto <- c(2, 1.5, 1, 0, -1, -1.5, -2)
colores <- c('#001F93', '#0087FF', '#01DBFE', '#00FF42', '#B9FF00', '#F8F402', '#FFD100', '#CA0F00')

setwd(carpeta.de.plots)

nombre.plot <- paste0('SPI_mensual', '.png')
png(nombre.plot, width = 1080, height = 720, units = "px")

ggplot(db.spi.con.pendiente, aes(x=anho, y=valor) ) +
  geom_point(aes(col=clasificacion), size=3) +
  scale_color_manual(values = colores, name='Clasificación') +
  geom_line() +
  labs(x = '', y = 'SPI') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = db.spi.con.pendiente,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
    check_overlap = TRUE,
    hjust   = -0.1,
    vjust   = -1,
    inherit.aes=FALSE
  ) +
  
  scale_y_continuous(limits = c(-4, 4), breaks=seq(-4, 4, by=1)) +
  scale_x_continuous(limits = c(1980, 2017), breaks=seq(1980, 2017, by=2)) +
  geom_hline(yintercept=intercepto, linetype="dashed", color = 'black') +
  facet_wrap(~nombre.mes, ncol = 3) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

# fin ---




# SPI estaciones ----

head(db.spi)

meses.de.verano <- 1:3
meses.de.otonho <- 4:6
meses.de.invierno <- 7:9
meses.de.primavera <- 10:12


# Verano

id.verano <- which(db.spi$mes%in%meses.de.verano)
db.spi.verano <- db.spi[id.verano,]
anhos.unicos.verano <- unique(db.spi.verano$anho)

db.spi.verano.con.pendiente <- c()
for (i in 1:(length(anhos.unicos.verano)-2) ) {
  # i <- 1
  
  anho.i <- anhos.unicos.verano[c(i, i+1)]
  db.spi.i <- db.spi.verano[db.spi.verano$anho%in%anho.i,]
  
  if(nrow(db.spi.i) > 6 & nrow(db.spi.i) < 12){db.spi.i <- db.spi.i[-c(7:nrow(db.spi.i)),]
  } else if(nrow(db.spi.i) == 12){db.spi.i <- db.spi.i[4:9,]}
  
  valor.promedio <- mean(db.spi.i$valor)
  sd <- sd(db.spi.i$valor)
  
  db.spi.mean.i <- data.frame(anho=anho.i[2], valor=valor.promedio, sd=sd)
  
  db.spi.verano.con.pendiente <- rbind(db.spi.verano.con.pendiente, db.spi.mean.i)
}

spi.ts.i <- ts(db.spi.verano.con.pendiente$valor, start = anhos.unicos.verano[2], freq = 1)

slope.i <- sens.slope(spi.ts.i,) ; slope.i
valor.pendiente <- round(slope.i$estimates, 3)
valor.p.value <- round(slope.i$p.value, 3)

db.spi.verano.con.pendiente$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

db.spi.verano.con.pendiente$estacion <- 'Verano'
db.spi.verano.con.pendiente


# Otono

id.otonho <- which(db.spi$mes%in%meses.de.otonho)
db.spi.otonho <- db.spi[id.otonho,]
anhos.unicos.otonho <- unique(db.spi.otonho$anho)

db.spi.otonho.con.pendiente <- c()
for (i in 1:(length(anhos.unicos.otonho)-2) ) {
  # i <- 1
  
  anho.i <- anhos.unicos.otonho[c(i, i+1)]
  db.spi.i <- db.spi.otonho[db.spi.otonho$anho%in%anho.i,]
  
  if(nrow(db.spi.i) > 6 & nrow(db.spi.i) < 12){db.spi.i <- db.spi.i[-c(7:nrow(db.spi.i)),]
  } else if(nrow(db.spi.i) == 12){db.spi.i <- db.spi.i[4:9,]}
  
  valor.promedio <- mean(db.spi.i$valor)
  sd <- sd(db.spi.i$valor)
  
  db.spi.mean.i <- data.frame(anho=anho.i[2], valor=valor.promedio, sd=sd)
  
  db.spi.otonho.con.pendiente <- rbind(db.spi.otonho.con.pendiente, db.spi.mean.i)
}

spi.ts.i <- ts(db.spi.otonho.con.pendiente$valor, start = anhos.unicos.otonho[2], freq = 1)

slope.i <- sens.slope(spi.ts.i,) ; slope.i
valor.pendiente <- round(slope.i$estimates, 3)
valor.p.value <- round(slope.i$p.value, 3)

db.spi.otonho.con.pendiente$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

db.spi.otonho.con.pendiente$estacion <- 'Otono'
db.spi.otonho.con.pendiente


# Invierno

id.invierno <- which(db.spi$mes%in%meses.de.invierno)
db.spi.invierno <- db.spi[id.invierno,]
anhos.unicos.invierno <- unique(db.spi.invierno$anho)

db.spi.invierno.con.pendiente <- c()
for (i in 1:length(anhos.unicos.invierno)) {
  # i <- 2
  
  anho.i <- anhos.unicos.invierno[i]
  db.spi.i <- subset(db.spi.invierno, anho==anho.i)

  valor.promedio <- mean(db.spi.i$valor)
  sd <- sd(db.spi.i$valor)
  
  db.spi.mean.i <- data.frame(anho=anho.i, valor=valor.promedio, sd=sd)
  
  db.spi.invierno.con.pendiente <- rbind(db.spi.invierno.con.pendiente, db.spi.mean.i)
}

spi.ts.i <- ts(db.spi.invierno.con.pendiente$valor, start = anhos.unicos.invierno[1], freq = 1)

slope.i <- sens.slope(spi.ts.i,) ; slope.i
valor.pendiente <- round(slope.i$estimates, 3)
valor.p.value <- round(slope.i$p.value, 3)

db.spi.invierno.con.pendiente$leyenda.valor.pendiente <- paste0(
        'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

db.spi.invierno.con.pendiente$estacion <- 'Invierno'
db.spi.invierno.con.pendiente


# Primavera

id.primavera <- which(db.spi$mes%in%meses.de.primavera)
db.spi.primavera <- db.spi[id.primavera,]
anhos.unicos.primavera <- unique(db.spi.primavera$anho)

db.spi.primavera.con.pendiente <- c()
for (i in 1:(length(anhos.unicos.primavera)-2) ) {
  # i <- length(anhos.unicos.primavera)
  
  anho.i <- anhos.unicos.primavera[c(i, i+1)]
  db.spi.i <- db.spi.primavera[db.spi.primavera$anho%in%anho.i,]
  
  if(nrow(db.spi.i) > 6 & nrow(db.spi.i) < 12){db.spi.i <- db.spi.i[-c(7:nrow(db.spi.i)),]
    } else if(nrow(db.spi.i) == 12){db.spi.i <- db.spi.i[4:9,]}
  
  valor.promedio <- mean(db.spi.i$valor)
  sd <- sd(db.spi.i$valor)
  
  db.spi.mean.i <- data.frame(anho=anho.i[2], valor=valor.promedio, sd=sd)
  
  db.spi.primavera.con.pendiente <- rbind(db.spi.primavera.con.pendiente, db.spi.mean.i)
}

spi.ts.i <- ts(db.spi.primavera.con.pendiente$valor, start = anhos.unicos.primavera[2], freq = 1)

slope.i <- sens.slope(spi.ts.i,) ; slope.i
valor.pendiente <- round(slope.i$estimates, 3)
valor.p.value <- round(slope.i$p.value, 3)

db.spi.primavera.con.pendiente$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

db.spi.primavera.con.pendiente$estacion <- 'Primavera'
db.spi.primavera.con.pendiente


# Union de db's

db.spi.estaciones <- rbind(db.spi.verano.con.pendiente, db.spi.otonho.con.pendiente, 
                           db.spi.invierno.con.pendiente, db.spi.primavera.con.pendiente)
head(db.spi.estaciones)
dim(db.spi.verano.con.pendiente)
dim(db.spi.otonho.con.pendiente)
dim(db.spi.invierno.con.pendiente)
dim(db.spi.primavera.con.pendiente)
dim(db.spi.estaciones)

db.spi.estaciones$clasificacion <- NA
db.spi.estaciones$color <- NA

db.spi.estaciones$clasificacion[db.spi.estaciones$valor >= 2] <- 'Extremadamente húmedo'
db.spi.estaciones$color[db.spi.estaciones$valor >= 2] <- '#001F93'
db.spi.estaciones$clasificacion[db.spi.estaciones$valor < 2 & db.spi.estaciones$valor >= 1.5 ] <- 'Muy húmedo'
db.spi.estaciones$color[db.spi.estaciones$valor < 2 & db.spi.estaciones$valor >= 1.5 ] <- '#0087FF'
db.spi.estaciones$clasificacion[db.spi.estaciones$valor < 1.5 & db.spi.estaciones$valor >= 1] <- 'Moderadamente húmedo'
db.spi.estaciones$color[db.spi.estaciones$valor < 1.5 & db.spi.estaciones$valor >= 1] <- '#01DBFE'

db.spi.estaciones$clasificacion[db.spi.estaciones$valor < 1 & db.spi.estaciones$valor >= 0] <- 'Normal'
db.spi.estaciones$color[db.spi.estaciones$valor < 1 & db.spi.estaciones$valor >= 0] <- '#00FF42'
db.spi.estaciones$clasificacion[db.spi.estaciones$valor < 0 & db.spi.estaciones$valor > -1] <- 'Casi normal'
db.spi.estaciones$color[db.spi.estaciones$valor < 0 & db.spi.estaciones$valor > -1] <- '#B9FF00'

db.spi.estaciones$clasificacion[db.spi.estaciones$valor <= -1 & db.spi.estaciones$valor > -1.5] <- 'Moderadamente seco'
db.spi.estaciones$color[db.spi.estaciones$valor <= -1 & db.spi.estaciones$valor > -1.5] <- '#F8F402'
db.spi.estaciones$clasificacion[db.spi.estaciones$valor <= -1.5 & db.spi.estaciones$valor > -2] <- 'Muy seco'
db.spi.estaciones$color[db.spi.estaciones$valor <= -1.5 & db.spi.estaciones$valor > -2] <- '#FFD100'
db.spi.estaciones$clasificacion[db.spi.estaciones$valor <= -2] <- 'Extremadamente seco'
db.spi.estaciones$color[db.spi.estaciones$valor <= -2] <- '#CA0F00'

db.spi.estaciones$clasificacion <- factor(db.spi.estaciones$clasificacion, levels = niveles)
db.spi.estaciones$estacion <- factor(db.spi.estaciones$estacion, levels = c('Verano', 'Otono', 'Invierno', 'Primavera'))
db.spi.estaciones <- db.spi.estaciones[order(db.spi.estaciones$clasificacion),]

# Plot

setwd(carpeta.de.plots)

nombre.plot <- paste0('SPI_estaciones', '.png')
png(nombre.plot, width = 750, height = 580, units = "px", type = 'cairo')

ggplot(db.spi.estaciones, aes(x=anho, y=valor)) +
  scale_color_manual(values = unique(db.spi.estaciones$color), name='Clasificación', aesthetics = "colour") +
  geom_line() +

  geom_errorbar(aes(ymin=valor-sd, ymax=valor+sd), width=0.5, size=0.5,
                position=position_dodge2(0.05)) +
  geom_point(aes(colour=clasificacion), size=3) +
  
  labs(x = '', y = 'SPI') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = db.spi.estaciones,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
    check_overlap = TRUE,
    hjust   = -0.1,
    vjust   = -1,
    inherit.aes=FALSE
  ) +
  
  scale_y_continuous(limits = c(-4, 4), breaks=seq(-4, 4, by=1)) +
  scale_x_continuous(limits = c(1980, 2018), breaks=seq(1980, 2018, by=2)) +
  geom_hline(yintercept=intercepto, linetype="dashed", color = 'black') +
  facet_wrap(~estacion, ncol = 2) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

# fin ---




# RDI mensual ----

setwd(carpeta.de.archivos)


# Lectura de datos

nombre.archivo.parte.1 <- 'RDI_'
nombre.archivo <- list.files(patter=nombre.archivo.parte.1)

db.rdi0 <- read.xlsx(nombre.archivo, 1)
head(db.rdi0)
dim(db.rdi0)
str(db.rdi0)


# Depuracion db

db.rdi00 <- db.rdi0[c(2:nrow(db.rdi0)), c(1, 4)]
head(db.rdi00)
dim(db.rdi00)
str(db.rdi00)

db.rdi00$X1.month.RDI. <- as.POSIXct((as.numeric(db.rdi00$X1.month.RDI.)-25569)*86400, tz="GMT", origin="1970-01-01")
colnames(db.rdi00) <- c('fecha', 'valor')
db.rdi00$mes <- month(db.rdi00$fecha)
db.rdi00$anho <- year(db.rdi00$fecha)

class(db.rdi00$valor)
db.rdi00$valor <- as.numeric(db.rdi00$valor)

db.rdi00$clasificacion <- NA
db.rdi00$clasificacion[db.rdi00$valor >= 2] <- 'Extremadamente húmedo'
db.rdi00$clasificacion[db.rdi00$valor < 2 & db.rdi00$valor >= 1.5 ] <- 'Muy húmedo'
db.rdi00$clasificacion[db.rdi00$valor < 1.5 & db.rdi00$valor >= 1] <- 'Moderadamente húmedo'
db.rdi00$clasificacion[db.rdi00$valor < 1 & db.rdi00$valor >= 0] <- 'Normal'
db.rdi00$clasificacion[db.rdi00$valor < 0 & db.rdi00$valor > -1] <- 'Casi normal'
db.rdi00$clasificacion[db.rdi00$valor <= -1 & db.rdi00$valor > -1.5] <- 'Moderadamente seco'
db.rdi00$clasificacion[db.rdi00$valor <= -1.5 & db.rdi00$valor > -2] <- 'Muy seco'
db.rdi00$clasificacion[db.rdi00$valor <= -2] <- 'Extremadamente seco'

niveles <- c('Extremadamente húmedo', 'Muy húmedo', 'Moderadamente húmedo', 'Normal',
             'Casi normal', 'Moderadamente seco', 'Muy seco', 'Extremadamente seco')
db.rdi00$clasificacion <- factor(db.rdi00$clasificacion, levels = niveles)

table(db.rdi00$anho)

id.anhos.a.descartar <- which(as.numeric(table(db.rdi00$anho)) != 12)
anhos.a.descartar <- as.numeric(names(table(db.rdi00$anho)))[id.anhos.a.descartar]
filas.a.conservar <- which(!db.rdi00$anho%in%anhos.a.descartar)
db.rdi <- db.rdi00[filas.a.conservar,]

head(db.rdi)


# Calculo Sen's slope

nombre.meses <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 
                  'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')

db.rdi.con.pendiente <- c()

for (i in 1:12) {
  # i <- 1
  
  db.rdi.i <- subset(db.rdi, mes==i)
  db.rdi.i$nombre.mes <- nombre.meses[i]
  
  id.valores.sin.NA <- which(!is.na(db.rdi.i$valor))
  valor <- db.rdi.i$valor[id.valores.sin.NA]
  rdi.ts.i <- ts(valor, start = 1980, freq = 1)
  
  slope.i <- sens.slope(rdi.ts.i,) ; slope.i
  valor.pendiente <- round(slope.i$estimates, 3)
  valor.p.value <- round(slope.i$p.value, 3)
  
  db.rdi.i$leyenda.valor.pendiente <- paste0('Pendiente = ', valor.pendiente, 
                                             ' (', 'p-value = ', valor.p.value, ')')
  
  db.rdi.con.pendiente <- rbind(db.rdi.con.pendiente, db.rdi.i)
}

db.rdi.con.pendiente$nombre.mes <- factor(db.rdi.con.pendiente$nombre.mes, levels = nombre.meses)
head(db.rdi.con.pendiente)

id.valores.sin.NA <- which(!is.na(db.rdi.con.pendiente$valor))
db.rdi.con.pendiente <- db.rdi.con.pendiente[id.valores.sin.NA,]


# Plot

intercepto <- c(2, 1.5, 1, 0, -1, -1.5, -2)
colores <- c('#001F93', '#0087FF', '#01DBFE', '#00FF42', '#B9FF00', '#F8F402', '#FFD100', '#CA0F00')

setwd(carpeta.de.plots)

nombre.plot <- paste0('RDI_estandarizado_mensual', '.png')
png(nombre.plot, width = 1080, height = 720, units = "px")

ggplot(db.rdi.con.pendiente, aes(x=anho, y=valor) ) +
  geom_point(aes(col=clasificacion), size=3) +
  scale_color_manual(values = colores, name='Clasificación') +
  geom_line() +
  labs(x = '', y = 'RDI Estandarizado') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = db.rdi.con.pendiente,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
    check_overlap = TRUE,
    hjust   = -0.1,
    vjust   = -1,
    inherit.aes=FALSE
  ) +
  
  scale_y_continuous(limits = c(-5, 5), breaks=seq(-5, 5, by=1)) +
  scale_x_continuous(limits = c(1980, 2018), breaks=seq(1980, 2018, by=2)) +
  geom_hline(yintercept=intercepto, linetype="dashed", color = 'black') +
  facet_wrap(~nombre.mes, ncol = 3) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

# fin ---




# RDI estaciones ----

head(db.rdi)

meses.de.verano <- 1:3
meses.de.otonho <- 4:6
meses.de.invierno <- 7:9
meses.de.primavera <- 10:12


# Verano

id.verano <- which(db.rdi$mes%in%meses.de.verano)
db.rdi.verano <- db.rdi[id.verano,]
anhos.unicos.verano <- unique(db.rdi.verano$anho)

db.rdi.verano.con.pendiente <- c()
for (i in 1:(length(anhos.unicos.verano)-2) ) {
  # i <- 1
  
  anho.i <- anhos.unicos.verano[c(i, i+1)]
  db.rdi.i <- db.rdi.verano[db.rdi.verano$anho%in%anho.i,]
  
  if(nrow(db.rdi.i) > 6 & nrow(db.rdi.i) < 12){db.rdi.i <- db.rdi.i[-c(7:nrow(db.rdi.i)),]
  } else if(nrow(db.rdi.i) == 12){db.rdi.i <- db.rdi.i[4:9,]}
  
  valor.promedio <- mean(db.rdi.i$valor)
  sd <- sd(db.rdi.i$valor)
  
  db.rdi.mean.i <- data.frame(anho=anho.i[2], valor=valor.promedio, sd=sd)
  
  db.rdi.verano.con.pendiente <- rbind(db.rdi.verano.con.pendiente, db.rdi.mean.i)
}

rdi.ts.i <- ts(db.rdi.verano.con.pendiente$valor, start = anhos.unicos.verano[2], freq = 1)

slope.i <- sens.slope(rdi.ts.i,) ; slope.i
valor.pendiente <- round(slope.i$estimates, 3)
valor.p.value <- round(slope.i$p.value, 3)

db.rdi.verano.con.pendiente$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

db.rdi.verano.con.pendiente$estacion <- 'Verano'
db.rdi.verano.con.pendiente


# Otono

id.otonho <- which(db.rdi$mes%in%meses.de.otonho)
db.rdi.otonho <- db.rdi[id.otonho,]
anhos.unicos.otonho <- unique(db.rdi.otonho$anho)

db.rdi.otonho.con.pendiente <- c()
for (i in 1:(length(anhos.unicos.otonho)-2) ) {
  # i <- 1
  
  anho.i <- anhos.unicos.otonho[c(i, i+1)]
  db.rdi.i <- db.rdi.otonho[db.rdi.otonho$anho%in%anho.i,]
  
  if(nrow(db.rdi.i) > 6 & nrow(db.rdi.i) < 12){db.rdi.i <- db.rdi.i[-c(7:nrow(db.rdi.i)),]
  } else if(nrow(db.rdi.i) == 12){db.rdi.i <- db.rdi.i[4:9,]}
  
  valor.promedio <- mean(db.rdi.i$valor)
  sd <- sd(db.rdi.i$valor)
  
  db.rdi.mean.i <- data.frame(anho=anho.i[2], valor=valor.promedio, sd=sd)
  
  db.rdi.otonho.con.pendiente <- rbind(db.rdi.otonho.con.pendiente, db.rdi.mean.i)
}

rdi.ts.i <- ts(db.rdi.otonho.con.pendiente$valor, start = anhos.unicos.otonho[2], freq = 1)

slope.i <- sens.slope(rdi.ts.i,) ; slope.i
valor.pendiente <- round(slope.i$estimates, 3)
valor.p.value <- round(slope.i$p.value, 3)

db.rdi.otonho.con.pendiente$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

db.rdi.otonho.con.pendiente$estacion <- 'Otono'
db.rdi.otonho.con.pendiente


# Invierno

id.invierno <- which(db.rdi$mes%in%meses.de.invierno)
db.rdi.invierno <- db.rdi[id.invierno,]
anhos.unicos.invierno <- unique(db.rdi.invierno$anho)

db.rdi.invierno.con.pendiente <- c()
for (i in 1:length(anhos.unicos.invierno)) {
  # i <- 2
  
  anho.i <- anhos.unicos.invierno[i]
  db.rdi.i <- subset(db.rdi.invierno, anho==anho.i)
  
  valor.promedio <- mean(db.rdi.i$valor)
  sd <- sd(db.rdi.i$valor)
  
  db.rdi.mean.i <- data.frame(anho=anho.i, valor=valor.promedio, sd=sd)
  
  db.rdi.invierno.con.pendiente <- rbind(db.rdi.invierno.con.pendiente, db.rdi.mean.i)
}

rdi.ts.i <- ts(db.rdi.invierno.con.pendiente$valor, start = anhos.unicos.invierno[1], freq = 1)

slope.i <- sens.slope(rdi.ts.i,) ; slope.i
valor.pendiente <- round(slope.i$estimates, 3)
valor.p.value <- round(slope.i$p.value, 3)

db.rdi.invierno.con.pendiente$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

db.rdi.invierno.con.pendiente$estacion <- 'Invierno'
db.rdi.invierno.con.pendiente


# Primavera

id.primavera <- which(db.rdi$mes%in%meses.de.primavera)
db.rdi.primavera <- db.rdi[id.primavera,]
anhos.unicos.primavera <- unique(db.rdi.primavera$anho)

db.rdi.primavera.con.pendiente <- c()
for (i in 1:(length(anhos.unicos.primavera)-2) ) {
  # i <- length(anhos.unicos.primavera)
  
  anho.i <- anhos.unicos.primavera[c(i, i+1)]
  db.rdi.i <- db.rdi.primavera[db.rdi.primavera$anho%in%anho.i,]
  
  if(nrow(db.rdi.i) > 6 & nrow(db.rdi.i) < 12){db.rdi.i <- db.rdi.i[-c(7:nrow(db.rdi.i)),]
  } else if(nrow(db.rdi.i) == 12){db.rdi.i <- db.rdi.i[4:9,]}
  
  valor.promedio <- mean(db.rdi.i$valor)
  sd <- sd(db.rdi.i$valor)
  
  db.rdi.mean.i <- data.frame(anho=anho.i[2], valor=valor.promedio, sd=sd)
  
  db.rdi.primavera.con.pendiente <- rbind(db.rdi.primavera.con.pendiente, db.rdi.mean.i)
}

rdi.ts.i <- ts(db.rdi.primavera.con.pendiente$valor, start = anhos.unicos.primavera[2], freq = 1)

slope.i <- sens.slope(rdi.ts.i,) ; slope.i
valor.pendiente <- round(slope.i$estimates, 3)
valor.p.value <- round(slope.i$p.value, 3)

db.rdi.primavera.con.pendiente$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

db.rdi.primavera.con.pendiente$estacion <- 'Primavera'
db.rdi.primavera.con.pendiente


# Union de db's

db.rdi.estaciones <- rbind(db.rdi.verano.con.pendiente, db.rdi.otonho.con.pendiente, 
                           db.rdi.invierno.con.pendiente, db.rdi.primavera.con.pendiente)
head(db.rdi.estaciones)
dim(db.rdi.verano.con.pendiente)
dim(db.rdi.otonho.con.pendiente)
dim(db.rdi.invierno.con.pendiente)
dim(db.rdi.primavera.con.pendiente)
dim(db.rdi.estaciones)

db.rdi.estaciones$clasificacion <- NA
db.rdi.estaciones$color <- NA

db.rdi.estaciones$clasificacion[db.rdi.estaciones$valor >= 2] <- 'Extremadamente húmedo'
db.rdi.estaciones$color[db.rdi.estaciones$valor >= 2] <- '#001F93'
db.rdi.estaciones$clasificacion[db.rdi.estaciones$valor < 2 & db.rdi.estaciones$valor >= 1.5 ] <- 'Muy húmedo'
db.rdi.estaciones$color[db.rdi.estaciones$valor < 2 & db.rdi.estaciones$valor >= 1.5 ] <- '#0087FF'
db.rdi.estaciones$clasificacion[db.rdi.estaciones$valor < 1.5 & db.rdi.estaciones$valor >= 1] <- 'Moderadamente húmedo'
db.rdi.estaciones$color[db.rdi.estaciones$valor < 1.5 & db.rdi.estaciones$valor >= 1] <- '#01DBFE'

db.rdi.estaciones$clasificacion[db.rdi.estaciones$valor < 1 & db.rdi.estaciones$valor >= 0] <- 'Normal'
db.rdi.estaciones$color[db.rdi.estaciones$valor < 1 & db.rdi.estaciones$valor >= 0] <- '#00FF42'
db.rdi.estaciones$clasificacion[db.rdi.estaciones$valor < 0 & db.rdi.estaciones$valor > -1] <- 'Casi normal'
db.rdi.estaciones$color[db.rdi.estaciones$valor < 0 & db.rdi.estaciones$valor > -1] <- '#B9FF00'

db.rdi.estaciones$clasificacion[db.rdi.estaciones$valor <= -1 & db.rdi.estaciones$valor > -1.5] <- 'Moderadamente seco'
db.rdi.estaciones$color[db.rdi.estaciones$valor <= -1 & db.rdi.estaciones$valor > -1.5] <- '#F8F402'
db.rdi.estaciones$clasificacion[db.rdi.estaciones$valor <= -1.5 & db.rdi.estaciones$valor > -2] <- 'Muy seco'
db.rdi.estaciones$color[db.rdi.estaciones$valor <= -1.5 & db.rdi.estaciones$valor > -2] <- '#FFD100'
db.rdi.estaciones$clasificacion[db.rdi.estaciones$valor <= -2] <- 'Extremadamente seco'
db.rdi.estaciones$color[db.rdi.estaciones$valor <= -2] <- '#CA0F00'

db.rdi.estaciones$clasificacion <- factor(db.rdi.estaciones$clasificacion, levels = niveles)
db.rdi.estaciones$estacion <- factor(db.rdi.estaciones$estacion, levels = c('Verano', 'Otono', 'Invierno', 'Primavera'))
db.rdi.estaciones <- db.rdi.estaciones[order(db.rdi.estaciones$clasificacion),]

# Plot

setwd(carpeta.de.plots)

nombre.plot <- paste0('RDI_estandarizado_estaciones', '.png')
png(nombre.plot, width = 750, height = 580, units = "px", type = 'cairo')

ggplot(db.rdi.estaciones, aes(x=anho, y=valor)) +
  scale_color_manual(values = unique(db.rdi.estaciones$color), name='Clasificación', aesthetics = "colour") +
  geom_line() +
  geom_errorbar(aes(ymin=valor-sd, ymax=valor+sd), width=0.5, size=0.5,
                position=position_dodge2(0.05)) +
  geom_point(aes(colour=clasificacion), size=3) +
  labs(x = '', y = 'RDI Estandarizado') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = db.rdi.estaciones,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
    check_overlap = TRUE,
    hjust   = -0.1,
    vjust   = -1,
    inherit.aes=FALSE
  ) +
  
  scale_y_continuous(limits = c(-4, 4), breaks=seq(-4, 4, by=1)) +
  scale_x_continuous(limits = c(1980, 2017), breaks=seq(1980, 2017, by=2)) +
  geom_hline(yintercept=intercepto, linetype="dashed", color = 'black') +
  facet_wrap(~estacion, ncol = 2) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1))


dev.off()

# fin ---




# PET mensual ----

setwd(carpeta.de.archivos)


# Lectura de datos

nombre.archivo.parte.1 <- 'PET_'
nombre.archivo <- list.files(pattern = nombre.archivo.parte.1)

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

pet.mensual0 <- pet[-c(nrow(pet)-1, nrow(pet)), -c(nrow(pet0)-1, ncol(pet))]
pet.mensual0

colnames(pet.mensual0)
nuevo.orden.columnas <- c('Year', 'Jan.', 'Feb.', 'Mar.', 'Apr.', 'May', 'Jun.',
                          'Jul.', 'Aug.', 'Sep.', 'Oct.', 'Nov.', 'Dec.')
pet.mensual <- pet.mensual0[,nuevo.orden.columnas]
colnames(pet.mensual)[2:ncol(pet.mensual)] <- nombre.meses
head(pet.mensual)
str(pet.mensual)

pet.mensual[,c(2:ncol(pet.mensual))] <- sapply(pet.mensual[,c(2:ncol(pet.mensual))], as.numeric)
pet.mensual$Year <- 1980:2017

head(pet.mensual)
str(pet.mensual)


# Calculo Sen's slope

pet.mensual.con.pendiente <- c()

for (i in 1:12) {
  # i <- 1
  
  anho.pet.mensual.i <- pet.mensual[,1]
  pet.mensual.i <- pet.mensual[,i+1]
  nombre.mes.i <- nombre.meses[i]
  
  spi.ts.i <- ts(pet.mensual.i, start = 1980, freq = 1)
  
  slope.i <- sens.slope(spi.ts.i,) ; slope.i
  valor.pendiente <- round(slope.i$estimates, 3)
  valor.p.value <- round(slope.i$p.value, 3)
  
  leyenda.valor.pendiente.i <- paste0('Pendiente = ', valor.pendiente, 
                                    ' (', 'p-value = ', valor.p.value, ')')
  
  pet.mensual.con.pendiente0 <- data.frame(anho=anho.pet.mensual.i, nombre.mes=nombre.mes.i, 
                                           valor=pet.mensual.i, 
                                           leyenda.valor.pendiente=leyenda.valor.pendiente.i)
  
  pet.mensual.con.pendiente <- rbind(pet.mensual.con.pendiente, pet.mensual.con.pendiente0)
}

pet.mensual.con.pendiente$nombre.mes <- factor(pet.mensual.con.pendiente$nombre.mes, levels = nombre.meses)
head(pet.mensual.con.pendiente)


# Plot

setwd(carpeta.de.plots)

nombre.plot <- paste0('PET_mensual', '.png')
png(nombre.plot, width = 1080, height = 720, units = "px")

ggplot(pet.mensual.con.pendiente, aes(x=anho, y=valor) ) +
  geom_point(size=1.5) +
  # scale_color_manual(values = colores, name='Clasificación') +
  geom_line() +
  labs(x = '', y = 'Evapotranspiración Potencial (mm)') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = pet.mensual.con.pendiente,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
    check_overlap = TRUE,
    hjust   = -0.1,
    vjust   = -1,
    inherit.aes=FALSE
  ) +
  
  scale_x_continuous(limits = c(1980, 2017), breaks=seq(1980, 2017, by=2)) +
  facet_wrap(~nombre.mes, ncol = 3) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

# fin ---




# PET estaciones ----

head(pet.mensual)


# Verano

pet.verano <- pet.mensual[,c(1, meses.de.verano+1)]
colnames(pet.verano)[1] <- 'anho'
head(pet.verano)

pet.verano$valor.promedio <- rowMeans(pet.verano[,2:ncol(pet.verano)])

pet.verano.sd <- c()
for (i in 1:nrow(pet.verano)) {
  pet.verano.sd0 <- sd(pet.verano[i, 2:(ncol(pet.verano)-1)])  
  pet.verano.sd <- c(pet.verano.sd, pet.verano.sd0)
}

pet.verano$sd <- pet.verano.sd
head(pet.verano)

pet.ts <- ts(pet.verano$valor.promedio, start = pet.verano$anho[1], freq = 1)

slope <- sens.slope(pet.ts,) ; slope
valor.pendiente <- round(slope$estimates, 3)
valor.p.value <- round(slope$p.value, 3)

pet.verano$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

pet.verano$estacion <- 'Verano'

pet.verano.depurado <- pet.verano[,c('anho', 'estacion', 'valor.promedio', 'sd', 'leyenda.valor.pendiente')]
head(pet.verano.depurado)


# Otonho

pet.otonho <- pet.mensual[,c(1, meses.de.otonho+1)]
colnames(pet.otonho)[1] <- 'anho'
head(pet.otonho)

pet.otonho$valor.promedio <- rowMeans(pet.otonho[,2:ncol(pet.otonho)])

pet.otonho.sd <- c()
for (i in 1:nrow(pet.otonho)) {
  pet.otonho.sd0 <- sd(pet.otonho[i, 2:(ncol(pet.otonho)-1)])  
  pet.otonho.sd <- c(pet.otonho.sd, pet.otonho.sd0)
}

pet.otonho$sd <- pet.otonho.sd
head(pet.otonho)

pet.ts <- ts(pet.otonho$valor.promedio, start = pet.otonho$anho[1], freq = 1)

slope <- sens.slope(pet.ts,) ; slope
valor.pendiente <- round(slope$estimates, 3)
valor.p.value <- round(slope$p.value, 3)

pet.otonho$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

pet.otonho$estacion <- 'Otono'

pet.otonho.depurado <- pet.otonho[,c('anho', 'estacion', 'valor.promedio', 'sd', 'leyenda.valor.pendiente')]
head(pet.otonho.depurado)


# Invierno

pet.invierno <- pet.mensual[,c(1, meses.de.invierno+1)]
colnames(pet.invierno)[1] <- 'anho'
head(pet.invierno)

pet.invierno$valor.promedio <- rowMeans(pet.invierno[,2:ncol(pet.invierno)])

pet.invierno.sd <- c()
for (i in 1:nrow(pet.invierno)) {
  pet.invierno.sd0 <- sd(pet.invierno[i, 2:(ncol(pet.invierno)-1)])  
  pet.invierno.sd <- c(pet.invierno.sd, pet.invierno.sd0)
}

pet.invierno$sd <- pet.invierno.sd
head(pet.invierno)

pet.ts <- ts(pet.invierno$valor.promedio, start = pet.invierno$anho[1], freq = 1)

slope <- sens.slope(pet.ts,) ; slope
valor.pendiente <- round(slope$estimates, 3)
valor.p.value <- round(slope$p.value, 3)

pet.invierno$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

pet.invierno$estacion <- 'Invierno'

pet.invierno.depurado <- pet.invierno[,c('anho', 'estacion', 'valor.promedio', 'sd', 'leyenda.valor.pendiente')]
head(pet.invierno.depurado)


# Primavera

pet.primavera <- pet.mensual[,c(1, meses.de.primavera+1)]
colnames(pet.primavera)[1] <- 'anho'
head(pet.primavera)

pet.primavera$valor.promedio <- rowMeans(pet.primavera[,2:ncol(pet.primavera)])

pet.primavera.sd <- c()
for (i in 1:nrow(pet.primavera)) {
  pet.primavera.sd0 <- sd(pet.primavera[i, 2:(ncol(pet.primavera)-1)])  
  pet.primavera.sd <- c(pet.primavera.sd, pet.primavera.sd0)
}

pet.primavera$sd <- pet.primavera.sd
head(pet.primavera)

pet.ts <- ts(pet.primavera$valor.promedio, start = pet.primavera$anho[1], freq = 1)

slope <- sens.slope(pet.ts,) ; slope
valor.pendiente <- round(slope$estimates, 3)
valor.p.value <- round(slope$p.value, 3)

pet.primavera$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

pet.primavera$estacion <- 'Primavera'

pet.primavera.depurado <- pet.primavera[,c('anho', 'estacion', 'valor.promedio', 'sd', 'leyenda.valor.pendiente')]
head(pet.primavera.depurado)


# Union de db's

db.pet.estaciones <- rbind(pet.verano.depurado, pet.otonho.depurado, 
                           pet.invierno.depurado, pet.primavera.depurado)

db.pet.estaciones$estacion <- factor(db.pet.estaciones$estacion, levels = c('Verano', 'Otono', 'Invierno', 'Primavera'))

head(db.pet.estaciones)

dim(pet.verano.depurado)
dim(pet.otonho.depurado)
dim(pet.invierno.depurado)
dim(pet.primavera.depurado)
dim(db.pet.estaciones)


# Plot

setwd(carpeta.de.plots)

nombre.plot <- paste0('PET_estaciones','.png')
png(nombre.plot, width = 750, height = 580, units = "px", type = 'cairo')

ggplot(db.pet.estaciones, aes(x=anho, y=valor.promedio)) +
  geom_point(size=1.5) +
  geom_line() +
  
  geom_errorbar(aes(ymin=valor.promedio-sd, ymax=valor.promedio+sd), width=0.5, size=0.5,
                position=position_dodge2(0.05)) +
  
  labs(x = '', y = 'Evapotranspiración Potencial (mm)') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = db.pet.estaciones,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
    check_overlap = TRUE,
    hjust   = -0.1,
    vjust   = -1,
    inherit.aes=FALSE
  ) +
  
  scale_x_continuous(limits = c(1980, 2017), breaks=seq(1980, 2017, by=2)) +
  facet_wrap(~estacion, ncol = 2) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

# fin ---




# SDI mensual ----

setwd(carpeta.de.archivos)


# Lectura de datos

nombre.archivo.parte.1 <- 'SDI_'
nombre.archivo <- list.files(pattern = nombre.archivo.parte.1)

db.sdi0 <- read.xlsx(nombre.archivo, 1)
head(db.sdi0)
dim(db.sdi0)
str(db.sdi0)


# Depuracion db

db.sdi00 <- db.sdi0[c(2:nrow(db.sdi0)), c(1, 2)]
head(db.sdi00)
dim(db.sdi00)
str(db.sdi00)

db.sdi00$X1.month.SDI <- as.POSIXct((as.numeric(db.sdi00$X1.month.SDI)-25569)*86400, tz="GMT", origin="1970-01-01")
colnames(db.sdi00) <- c('fecha', 'valor')
db.sdi00$mes <- month(db.sdi00$fecha)
db.sdi00$anho <- year(db.sdi00$fecha)

class(db.sdi00$valor)
db.sdi00$valor <- as.numeric(db.sdi00$valor)

db.sdi00$clasificacion <- NA
db.sdi00$clasificacion[db.sdi00$valor >= 0] <- 'No sequía'
db.sdi00$clasificacion[db.sdi00$valor >= -1 & db.sdi00$valor < 0] <- 'Sequía leve'
db.sdi00$clasificacion[db.sdi00$valor >= -1.5 & db.sdi00$valor < -1] <- 'Sequía moderada'
db.sdi00$clasificacion[db.sdi00$valor >= -2 & db.sdi00$valor < -1.5] <- 'Sequía severa'
db.sdi00$clasificacion[db.sdi00$valor < -2] <- 'Sequía extrema'

niveles <- c('No sequía', 'Sequía leve', 'Sequía moderada', 
             'Sequía severa', 'Sequía extrema')
db.sdi00$clasificacion <- factor(db.sdi00$clasificacion, levels = niveles)

table(db.sdi00$anho)

id.anhos.a.descartar <- which(as.numeric(table(db.sdi00$anho)) != 12)
anhos.a.descartar <- as.numeric(names(table(db.sdi00$anho)))[id.anhos.a.descartar]
filas.a.conservar <- which(!db.sdi00$anho%in%anhos.a.descartar)
db.sdi <- db.sdi00[filas.a.conservar,]

head(db.sdi)


# Calculo Sen's slope

nombre.meses <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 
                  'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')

db.sdi.con.pendiente <- c()

for (i in 1:12) {
  # i <- 1
  
  db.sdi.i <- subset(db.sdi, mes==i)
  db.sdi.i$nombre.mes <- nombre.meses[i]
  
  id.valores.sin.NA <- which(!is.na(db.sdi.i$valor))
  valor <- db.sdi.i$valor[id.valores.sin.NA]
  sdi.ts.i <- ts(valor, start = 1980, freq = 1)
  
  slope.i <- sens.slope(sdi.ts.i,) ; slope.i
  valor.pendiente <- round(slope.i$estimates, 3)
  valor.p.value <- round(slope.i$p.value, 3)
  
  db.sdi.i$leyenda.valor.pendiente <- paste0('Pendiente = ', valor.pendiente, 
                                             ' (', 'p-value = ', valor.p.value, ')')
  
  db.sdi.con.pendiente <- rbind(db.sdi.con.pendiente, db.sdi.i)
}

db.sdi.con.pendiente$nombre.mes <- factor(db.sdi.con.pendiente$nombre.mes, levels = nombre.meses)
head(db.sdi.con.pendiente)

id.valores.sin.NA <- which(!is.na(db.sdi.con.pendiente$valor))
db.sdi.con.pendiente <- db.sdi.con.pendiente[id.valores.sin.NA,]


# Plot

intercepto <- c(0, -1, -1.51, -2)
colores <- c('#87CB2F', '#F8F402', '#FFD100', '#FF8000', '#FC1402')

setwd(carpeta.de.plots)

nombre.plot <- paste0('SDI_mensual', '.png')
png(nombre.plot, width = 1080, height = 720, units = "px")

ggplot(db.sdi.con.pendiente, aes(x=anho, y=valor) ) +
  geom_point(aes(col=clasificacion), size=3) +
  scale_color_manual(values = colores, name='Clasificación') +
  geom_line() +
  labs(x = '', y = 'SDI') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = db.sdi.con.pendiente,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
    check_overlap = TRUE,
    hjust   = -0.1,
    vjust   = -1,
    inherit.aes=FALSE
  ) +
  
  scale_y_continuous(limits = c(-5, 5), breaks=seq(-5, 5, by=1)) +
  scale_x_continuous(limits = c(1980, 2017), breaks=seq(1980, 2017, by=2)) +
  geom_hline(yintercept=intercepto, linetype="dashed", color = 'black') +
  facet_wrap(~nombre.mes, ncol = 3) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

# fin ---




# SDI estaciones ----

head(db.sdi)

meses.de.verano <- 1:3
meses.de.otonho <- 4:6
meses.de.invierno <- 7:9
meses.de.primavera <- 10:12


# Verano

id.verano <- which(db.sdi$mes%in%meses.de.verano)
db.sdi.verano <- db.sdi[id.verano,]
anhos.unicos.verano <- unique(db.sdi.verano$anho)

db.sdi.verano.con.pendiente <- c()
for (i in 1:length(anhos.unicos.verano)) {
  # i <- 1
  
  anho.i <- anhos.unicos.verano[i]
  db.sdi.i <- subset(db.sdi.verano, anho==anho.i)
  
  valor.promedio <- mean(db.sdi.i$valor)
  sd <- sd(db.sdi.i$valor)
  
  db.sdi.mean.i <- data.frame(anho=anho.i, valor=valor.promedio, sd=sd)
  
  db.sdi.verano.con.pendiente <- rbind(db.sdi.verano.con.pendiente, db.sdi.mean.i)
}

id.valores.sin.NA <- which(!is.na(db.sdi.verano.con.pendiente$valor))
db.sdi.verano.con.pendiente <- db.sdi.verano.con.pendiente[id.valores.sin.NA,]

sdi.ts.i <- ts(db.sdi.verano.con.pendiente$valor, start = anhos.unicos.verano[1], freq = 1)

slope.i <- sens.slope(sdi.ts.i,) ; slope.i
valor.pendiente <- round(slope.i$estimates, 3)
valor.p.value <- round(slope.i$p.value, 3)

db.sdi.verano.con.pendiente$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

db.sdi.verano.con.pendiente$estacion <- 'Verano'
db.sdi.verano.con.pendiente


# Otonho

id.otonho <- which(db.sdi$mes%in%meses.de.otonho)
db.sdi.otonho <- db.sdi[id.otonho,]
anhos.unicos.otonho <- unique(db.sdi.otonho$anho)

db.sdi.otonho.con.pendiente <- c()
for (i in 1:length(anhos.unicos.otonho)) {
  # i <- 1
  
  anho.i <- anhos.unicos.otonho[i]
  db.sdi.i <- subset(db.sdi.otonho, anho==anho.i)
  
  valor.promedio <- mean(db.sdi.i$valor)
  sd <- sd(db.sdi.i$valor)
  
  db.sdi.mean.i <- data.frame(anho=anho.i, valor=valor.promedio, sd=sd)
  
  db.sdi.otonho.con.pendiente <- rbind(db.sdi.otonho.con.pendiente, db.sdi.mean.i)
}

id.valores.sin.NA <- which(!is.na(db.sdi.otonho.con.pendiente$valor))
db.sdi.otonho.con.pendiente <- db.sdi.otonho.con.pendiente[id.valores.sin.NA,]

sdi.ts.i <- ts(db.sdi.otonho.con.pendiente$valor, start = anhos.unicos.otonho[1], freq = 1)

slope.i <- sens.slope(sdi.ts.i,) ; slope.i
valor.pendiente <- round(slope.i$estimates, 3)
valor.p.value <- round(slope.i$p.value, 3)

db.sdi.otonho.con.pendiente$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

db.sdi.otonho.con.pendiente$estacion <- 'Otono'
db.sdi.otonho.con.pendiente


# Invierno

id.invierno <- which(db.sdi$mes%in%meses.de.invierno)
db.sdi.invierno <- db.sdi[id.invierno,]
anhos.unicos.invierno <- unique(db.sdi.invierno$anho)

db.sdi.invierno.con.pendiente <- c()
for (i in 1:length(anhos.unicos.invierno)) {
  # i <- 1
  
  anho.i <- anhos.unicos.invierno[i]
  db.sdi.i <- subset(db.sdi.invierno, anho==anho.i)
  
  valor.promedio <- mean(db.sdi.i$valor)
  sd <- sd(db.sdi.i$valor)
  
  db.sdi.mean.i <- data.frame(anho=anho.i, valor=valor.promedio, sd=sd)
  
  db.sdi.invierno.con.pendiente <- rbind(db.sdi.invierno.con.pendiente, db.sdi.mean.i)
}

id.valores.sin.NA <- which(!is.na(db.sdi.invierno.con.pendiente$valor))
db.sdi.invierno.con.pendiente <- db.sdi.invierno.con.pendiente[id.valores.sin.NA,]

sdi.ts.i <- ts(db.sdi.invierno.con.pendiente$valor, start = anhos.unicos.invierno[1], freq = 1)

slope.i <- sens.slope(sdi.ts.i,) ; slope.i
valor.pendiente <- round(slope.i$estimates, 3)
valor.p.value <- round(slope.i$p.value, 3)

db.sdi.invierno.con.pendiente$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

db.sdi.invierno.con.pendiente$estacion <- 'Invierno'
db.sdi.invierno.con.pendiente


# Primavera

id.primavera <- which(db.sdi$mes%in%meses.de.primavera)
db.sdi.primavera <- db.sdi[id.primavera,]
anhos.unicos.primavera <- unique(db.sdi.primavera$anho)

db.sdi.primavera.con.pendiente <- c()
for (i in 1:length(anhos.unicos.primavera)) {
  # i <- 1
  
  anho.i <- anhos.unicos.primavera[i]
  db.sdi.i <- subset(db.sdi.primavera, anho==anho.i)
  
  valor.promedio <- mean(db.sdi.i$valor)
  sd <- sd(db.sdi.i$valor)
  
  db.sdi.mean.i <- data.frame(anho=anho.i, valor=valor.promedio, sd=sd)
  
  db.sdi.primavera.con.pendiente <- rbind(db.sdi.primavera.con.pendiente, db.sdi.mean.i)
}

id.valores.sin.NA <- which(!is.na(db.sdi.primavera.con.pendiente$valor))
db.sdi.primavera.con.pendiente <- db.sdi.primavera.con.pendiente[id.valores.sin.NA,]

sdi.ts.i <- ts(db.sdi.primavera.con.pendiente$valor, start = anhos.unicos.primavera[1], freq = 1)

slope.i <- sens.slope(sdi.ts.i,) ; slope.i
valor.pendiente <- round(slope.i$estimates, 3)
valor.p.value <- round(slope.i$p.value, 3)

db.sdi.primavera.con.pendiente$leyenda.valor.pendiente <- paste0(
  'Pendiente = ', valor.pendiente, ' (', 'p-value = ', valor.p.value, ')')

db.sdi.primavera.con.pendiente$estacion <- 'Primavera'
db.sdi.primavera.con.pendiente


# Union de db's

db.sdi.estaciones <- rbind(db.sdi.verano.con.pendiente, db.sdi.otonho.con.pendiente,
                           db.sdi.invierno.con.pendiente, db.sdi.primavera.con.pendiente)
head(db.sdi.estaciones)

dim(db.sdi.verano.con.pendiente)
dim(db.sdi.otonho.con.pendiente)
dim(db.sdi.invierno.con.pendiente)
dim(db.sdi.primavera.con.pendiente)
dim(db.sdi.estaciones)

db.sdi.estaciones$clasificacion <- NA
db.sdi.estaciones$color <- NA
db.sdi.estaciones$clasificacion[db.sdi.estaciones$valor >= 0] <- 'No sequía'
db.sdi.estaciones$color[db.sdi.estaciones$valor >= 0] <- '#87CB2F'
db.sdi.estaciones$clasificacion[db.sdi.estaciones$valor >= -1 & db.sdi.estaciones$valor < 0] <- 'Sequía leve'
db.sdi.estaciones$color[db.sdi.estaciones$valor >= -1 & db.sdi.estaciones$valor < 0] <- '#F8F402'
db.sdi.estaciones$clasificacion[db.sdi.estaciones$valor >= -1.5 & db.sdi.estaciones$valor < -1] <- 'Sequía moderada'
db.sdi.estaciones$color[db.sdi.estaciones$valor >= -1.5 & db.sdi.estaciones$valor < -1] <- '#FFD100'
db.sdi.estaciones$clasificacion[db.sdi.estaciones$valor >= -2 & db.sdi.estaciones$valor < -1.5] <- 'Sequía severa'
db.sdi.estaciones$color[db.sdi.estaciones$valor >= -2 & db.sdi.estaciones$valor < -1.5] <- '#FF8000'
db.sdi.estaciones$clasificacion[db.sdi.estaciones$valor < -2] <- 'Sequía extrema'
db.sdi.estaciones$color[db.sdi.estaciones$valor < -2] <- '#FC1402'

db.sdi.estaciones$clasificacion <- factor(db.sdi.estaciones$clasificacion, levels = niveles)
db.sdi.estaciones$estacion <- factor(db.sdi.estaciones$estacion, levels = c('Verano', 'Otono', 'Invierno', 'Primavera'))
db.sdi.estaciones <- db.sdi.estaciones[order(db.sdi.estaciones$clasificacion),]

# Plot

intercepto <- c(0, -1, -1.51, -2)

setwd(carpeta.de.plots)

nombre.plot <- paste0('SDI_estaciones', '.png')
png(nombre.plot, width = 750, height = 580, units = "px", type = 'cairo')

ggplot(db.sdi.estaciones, aes(x=anho, y=valor)) +
  scale_color_manual(values = unique(db.sdi.estaciones$color), 
                     name='Clasificación', aesthetics = "colour") +
  geom_line() +
  geom_errorbar(aes(ymin=valor-sd, ymax=valor+sd), width=0.5, size=0.5,
                position=position_dodge2(0.05)) +
  geom_point(aes(colour=clasificacion), size=3) +
  labs(x = '', y = 'SDI') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = db.sdi.estaciones,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
    check_overlap = TRUE,
    hjust   = -0.1,
    vjust   = -1,
    inherit.aes=FALSE
  ) +
  
  scale_y_continuous(limits = c(-5, 4), breaks=seq(-5, 4, by=1)) +
  scale_x_continuous(limits = c(1980, 2017), breaks=seq(1980, 2017, by=2)) +
  geom_hline(yintercept=intercepto, linetype="dashed", color = 'black') +
  facet_wrap(~estacion, ncol = 2) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

# fin ---

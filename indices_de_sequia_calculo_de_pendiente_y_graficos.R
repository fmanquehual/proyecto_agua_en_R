library(xlsx)
library(PerformanceAnalytics)
library(trend)
library(lubridate)
library(ggplot2)

rm(list=ls())
dev.off()




# SPI mensual ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/ERA_LAND/bases_de_datos/Puren/pixel_1/')

# Lectura de datos
db.spi0 <- read.xlsx('SPI_puren_mensual_pixel_1.xlsx', 1)
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
db.spi00$clasificacion[db.spi00$valor < 1 & db.spi00$valor > -1] <- 'Casi normal'
db.spi00$clasificacion[db.spi00$valor <= -1 & db.spi00$valor > -1.5] <- 'Moderadamente seco'
db.spi00$clasificacion[db.spi00$valor <= -1.5 & db.spi00$valor > -2] <- 'Muy seco'
db.spi00$clasificacion[db.spi00$valor <= -2] <- 'Extremadamente seco'

niveles <- c('Extremadamente húmedo', 'Muy húmedo', 'Moderadamente húmedo',
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
  
  spi.ts.i <- ts(db.spi.i$valor, start = 1982, freq = 1)
  
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

intercepto <- c(2, 1.5, 1, -1, -1.5, -2)
colores <- c('#001F93', '#0087FF', '#01DBFE', '#87CB2F', '#F8F402', '#FFD100', '#CA0F00')

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/ERA_LAND/bases_de_datos/Puren/pixel_1/plots/')

# png('SPI_mensual.png', width = 1080, height = 720, units = "px")

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
  scale_x_continuous(limits = c(1982, 2018), breaks=seq(1982, 2018, by=2)) +
  geom_hline(yintercept=intercepto, linetype="dashed", color = 'black') +
  facet_wrap(~nombre.mes, ncol = 3) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

# fin ---




# SPI invierno/verano ----

head(db.spi)

meses.de.invierno <- c(4:9)
meses.de.verano <- c(10:12, 1:3)


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


# Verano

id.verano <- which(db.spi00$mes%in%meses.de.verano)
db.spi.verano <- db.spi00[id.verano,]
anhos.unicos.verano <- unique(db.spi.verano$anho)

db.spi.verano.con.pendiente <- c()
for (i in 1:(length(anhos.unicos.verano)-2) ) {
  # i <- length(anhos.unicos.verano)
  
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


# Union de db's

db.spi.estaciones <- rbind(db.spi.invierno.con.pendiente, db.spi.verano.con.pendiente)
head(db.spi.estaciones)

dim(db.spi.invierno.con.pendiente)
dim(db.spi.verano.con.pendiente)
dim(db.spi.estaciones)

db.spi.estaciones$clasificacion <- NA
db.spi.estaciones$color <- NA
db.spi.estaciones$clasificacion[db.spi.estaciones$valor >= 2] <- 'Extremadamente húmedo'
db.spi.estaciones$color[db.spi.estaciones$valor >= 2] <- '#001F93'
db.spi.estaciones$clasificacion[db.spi.estaciones$valor < 2 & db.spi.estaciones$valor >= 1.5 ] <- 'Muy húmedo'
db.spi.estaciones$color[db.spi.estaciones$valor < 2 & db.spi.estaciones$valor >= 1.5 ] <- '#0087FF'
db.spi.estaciones$clasificacion[db.spi.estaciones$valor < 1.5 & db.spi.estaciones$valor >= 1] <- 'Moderadamente húmedo'
db.spi.estaciones$color[db.spi.estaciones$valor < 1.5 & db.spi.estaciones$valor >= 1] <- '#01DBFE'
db.spi.estaciones$clasificacion[db.spi.estaciones$valor < 1 & db.spi.estaciones$valor > -1] <- 'Casi normal'
db.spi.estaciones$color[db.spi.estaciones$valor < 1 & db.spi.estaciones$valor > -1] <- '#87CB2F'
db.spi.estaciones$clasificacion[db.spi.estaciones$valor <= -1 & db.spi.estaciones$valor > -1.5] <- 'Moderadamente seco'
db.spi.estaciones$color[db.spi.estaciones$valor <= -1 & db.spi.estaciones$valor > -1.5] <- '#F8F402'
db.spi.estaciones$clasificacion[db.spi.estaciones$valor <= -1.5 & db.spi.estaciones$valor > -2] <- 'Muy seco'
db.spi.estaciones$color[db.spi.estaciones$valor <= -1.5 & db.spi.estaciones$valor > -2] <- '#FFD100'
db.spi.estaciones$clasificacion[db.spi.estaciones$valor <= -2] <- 'Extremadamente seco'
db.spi.estaciones$color[db.spi.estaciones$valor <= -2] <- '#CA0F00'


# Plot
unique(db.spi.estaciones$color)

#intercepto <- c(2, 1.5, 1, -1, -1.5, -2)
#colores <- c('#001F93', '#0087FF', '#01DBFE', '#87CB2F', 'yellow', '#FFD100', '#CA0F00')

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/ERA_LAND/bases_de_datos/Puren/pixel_1/plots/')

png('SPI_invierno_verano.png', width = 750, height = 580, units = "px", type = 'cairo')

ggplot(db.spi.estaciones, aes(x=anho, y=valor)) +
  geom_point(aes(colour=clasificacion), size=3) +
  scale_color_manual(values = unique(db.spi.estaciones$color), name='Clasificación', aesthetics = "colour") +
  geom_line() +

  geom_errorbar(aes(ymin=valor-sd, ymax=valor+sd, colour=clasificacion), width=0.6, size=1,
                position=position_dodge2(0.05)) +

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
  scale_x_continuous(limits = c(1982, 2018), breaks=seq(1982, 2018, by=2)) +
  geom_hline(yintercept=intercepto, linetype="dashed", color = 'black') +
  facet_wrap(~estacion, ncol = 1) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

# fin ---

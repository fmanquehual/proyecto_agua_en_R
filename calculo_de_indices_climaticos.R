library(climdex.pcic)
library(lubridate)
library(ggplot2)
library(trend)


rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/proyecto_agua_en_R/')
source('funcion_calculo_de_pendiente_indices_climaticos.R')
source('funcion_calculo_de_pendiente_indices_climaticos_por_estacion.R')


# Lectura de datos ----

comuna <- 'Puren' # 'Teodoro_Schmidt' # 'Padre_Las_Casas' # 'Imperial'
directorio.principal <- 'C:/Users/Usuario/Documents/Francisco/proyecto_agua/CR2/base_de_datos/'

setwd(paste0(directorio.principal, comuna, '/', 'precipitacion/'))
pr <- read.csv(list.files(pattern = 'pr')[2])

setwd(paste0(directorio.principal, comuna, '/', 'temperatura_maxima/'))
tmax <- read.csv(list.files(pattern = 'tmax')[2])

setwd(paste0(directorio.principal, comuna, '/', 'temperatura_minima/'))
tmin <- read.csv(list.files(pattern = 'tmin')[2])

# fin ---




# Preparacion de datos ---- 

pr.fecha <- as.PCICt(pr$fecha, cal="gregorian")
tmin.fecha <- as.PCICt(tmin$fecha, cal="gregorian")
tmax.fecha <- as.PCICt(tmax$fecha, cal="gregorian")

ci <- climdexInput.raw(tmax=tmax$valor,
                       tmin=tmin$valor, 
                       prec=pr$valor,
                       tmax.dates=tmax.fecha, tmin.dates=tmin.fecha, prec.dates=pr.fecha, 
                       base.range=c(1979, 2018))

# fin ---




# Calculo de indices ----

fd <- climdex.fd(ci) ; length(fd) # 40 (1979:2018 = 40)
su <- climdex.su(ci) ; length(su) # 40
# id <- climdex.id(ci) ; length(id) # 40 # No lo usa la DMC y da resultados nulos
# tr <- climdex.tr(ci) ; length(tr) # 40 # No lo usa la DMC y da resultados nulos
# gsl <- climdex.gsl(ci) ; length(gsl) # 40 # No lo usa la DMC y da resultados nulos
txx <- climdex.txx(ci) ; length(txx) # 480 ((1979:2018)*12 = 480)
tnx <- climdex.tnx(ci) ; length(tnx) # 480
txn <- climdex.txn(ci) ; length(txn) # 480
tnn <- climdex.tnn(ci) ; length(tnn) # 480
tn10p <- climdex.tn10p(ci) ; length(tn10p) # 480
tx10p <- climdex.tx10p(ci) ; length(tx10p) # 480
tn90p <- climdex.tn90p(ci) ; length(tn90p) # 480
tx90p <- climdex.tx90p(ci) ; length(tx90p) # 480
wsdi <- climdex.wsdi(ci) ; length(wsdi) # 40
csdi <- climdex.csdi(ci) ; length(csdi) # 40
dtr <- climdex.dtr(ci) ; length(dtr) # 480
rx1day <- climdex.rx1day(ci) ; length(rx1day) # 480
rx5day <- climdex.rx5day(ci) ; length(rx5day) # 480
sdii <- climdex.sdii(ci) ; length(sdii) # 40
r10mm <- climdex.r10mm(ci) ; length(r10mm) # 40
r20mm <- climdex.r20mm(ci) ; length(r20mm) # 40
# rnnmm <- climdex.rnnmm(ci) ; length(rnnmm) # 40  # No lo usa la DMC
cdd <- climdex.cdd(ci) ; length(cdd) # 40
cwd <- climdex.cwd(ci) ; length(cwd) # 40
r95ptot <- climdex.r95ptot(ci) ; length(r95ptot) # 40
r99ptot <- climdex.r99ptot(ci) ; length(r99ptot) # 40
prcptot <- climdex.prcptot(ci) ; length(prcptot) # 40

# fin ---




# Db indices anuales ----

db.fd <- data.frame(fecha=names(fd), indice='fd', valor=fd)
db.fd$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.fd, anho_inicial = 1979)

db.su <- data.frame(fecha=names(su), indice='su', valor=su)
db.su$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.su, anho_inicial = 1979)

# db.id <- data.frame(fecha=names(id), indice='id', valor=id)
# db.id$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.id, anho_inicial = 1979)

# db.tr <- data.frame(fecha=names(tr), indice='tr', valor=tr)
# db.tr$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.tr, anho_inicial = 1979)

# db.gsl <- data.frame(fecha=names(gsl), indice='gsl', valor=gsl)
# db.gsl$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.gsl, anho_inicial = 1979)

db.wsdi <- data.frame(fecha=names(wsdi), indice='wsdi', valor=wsdi)
db.wsdi$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.wsdi, anho_inicial = 1979)

db.csdi <- data.frame(fecha=names(csdi), indice='csdi', valor=csdi)
db.csdi$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.csdi, anho_inicial = 1979)

db.sdii <- data.frame(fecha=names(sdii), indice='sdii', valor=sdii)
db.sdii$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.sdii, anho_inicial = 1979)

db.r10mm <- data.frame(fecha=names(r10mm), indice='r10mm', valor=r10mm)
db.r10mm$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.r10mm, anho_inicial = 1979)

db.r20mm <- data.frame(fecha=names(r20mm), indice='r20mm', valor=r20mm)
db.r20mm$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.r20mm, anho_inicial = 1979)

# db.rnnmm <- data.frame(fecha=names(rnnmm), indice='rnnmm', valor=rnnmm)
# db.rnnmm$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.rnnmm, anho_inicial = 1979)

db.cdd <- data.frame(fecha=names(cdd), indice='cdd', valor=cdd)
db.cdd$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.cdd, anho_inicial = 1979)

db.cwd <- data.frame(fecha=names(cwd), indice='cwd', valor=cwd)
db.cwd$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.cwd, anho_inicial = 1979)

db.r95ptot <- data.frame(fecha=names(r95ptot), indice='r95ptot', valor=r95ptot)
db.r95ptot$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.r95ptot, anho_inicial = 1979)

db.r99ptot <- data.frame(fecha=names(r99ptot), indice='r99ptot', valor=r99ptot)
db.r99ptot$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.r99ptot, anho_inicial = 1979)

db.prcptot <- data.frame(fecha=names(prcptot), indice='prcptot', valor=prcptot)
db.prcptot$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.prcptot, anho_inicial = 1979)

db.indices.anuales <- rbind(db.fd, db.su, db.wsdi, db.csdi, # db.id, db.tr, db.gsl, 
                            db.sdii, db.r10mm, db.r20mm, db.cdd, db.cwd, # db.rnnmm, 
                            db.r95ptot, db.r99ptot, db.prcptot)

db.indices.anuales$fecha <- as.numeric(db.indices.anuales$fecha)
row.names(db.indices.anuales) <- 1:nrow(db.indices.anuales)

dim(db.indices.anuales)
str(db.indices.anuales)
head(db.indices.anuales)

# fin ---




# Db indices mensuales ----

db.txx0 <- data.frame(fecha=names(txx), indice='txx', valor=txx)
db.txx <- calculo_de_pendiente_indices_climaticos_por_estacion(db.txx0, anho_inicial = 1979)

db.tnx0 <- data.frame(fecha=names(tnx), indice='tnx', valor=tnx)
db.tnx <- calculo_de_pendiente_indices_climaticos_por_estacion(db.tnx0, anho_inicial = 1979)

db.txn0 <- data.frame(fecha=names(txn), indice='txn', valor=txn)
db.txn <- calculo_de_pendiente_indices_climaticos_por_estacion(db.txn0, anho_inicial = 1979)

db.tnn0 <- data.frame(fecha=names(tnn), indice='tnn', valor=tnn)
db.tnn <- calculo_de_pendiente_indices_climaticos_por_estacion(db.tnn0, anho_inicial = 1979)

db.tn10p0 <- data.frame(fecha=names(tn10p), indice='tn10p', valor=tn10p)
db.tn10p <- calculo_de_pendiente_indices_climaticos_por_estacion(db.tn10p0, anho_inicial = 1979)

db.tx10p0 <- data.frame(fecha=names(tx10p), indice='tx10p', valor=tx10p)
db.tx10p <- calculo_de_pendiente_indices_climaticos_por_estacion(db.tx10p0, anho_inicial = 1979)

db.tn90p0 <- data.frame(fecha=names(tn90p), indice='tn90p', valor=tn90p)
db.tn90p <- calculo_de_pendiente_indices_climaticos_por_estacion(db.tn90p0, anho_inicial = 1979)

db.tx90p0 <- data.frame(fecha=names(tx90p), indice='tx90p', valor=tx90p)
db.tx90p <- calculo_de_pendiente_indices_climaticos_por_estacion(db.tx90p0, anho_inicial = 1979)

db.dtr0 <- data.frame(fecha=names(dtr), indice='dtr', valor=dtr)
db.dtr <- calculo_de_pendiente_indices_climaticos_por_estacion(db.dtr0, anho_inicial = 1979)

db.rx1day0 <- data.frame(fecha=names(rx1day), indice='rx1day', valor=rx1day)
db.rx1day <- calculo_de_pendiente_indices_climaticos_por_estacion(db.rx1day0, anho_inicial = 1979)

db.rx5day0 <- data.frame(fecha=names(rx5day), indice='rx5day', valor=rx5day)
db.rx5day <- calculo_de_pendiente_indices_climaticos_por_estacion(db.rx5day0, anho_inicial = 1979)


# db 1
db.indices.mensuales.parte.1 <- rbind(db.txx, db.tnx, db.txn)
row.names(db.indices.mensuales.parte.1) <- 1:nrow(db.indices.mensuales.parte.1)

db.indices.mensuales.parte.1$estacion <- factor(db.indices.mensuales.parte.1$estacion, 
                                                levels = c('Primavera', 'Verano', 'Otono', 'Invierno'))

head(db.indices.mensuales.parte.1)
str(db.indices.mensuales.parte.1)


# db 2
db.indices.mensuales.parte.2 <- rbind(db.tnn, db.tn10p, db.tx10p)
row.names(db.indices.mensuales.parte.2) <- 1:nrow(db.indices.mensuales.parte.2)

db.indices.mensuales.parte.2$estacion <- factor(db.indices.mensuales.parte.2$estacion, 
                                                levels = c('Primavera', 'Verano', 'Otono', 'Invierno'))

head(db.indices.mensuales.parte.2)
str(db.indices.mensuales.parte.2)


# db 3
db.indices.mensuales.parte.3 <- rbind(db.tn90p, db.tx90p, db.dtr)
row.names(db.indices.mensuales.parte.3) <- 1:nrow(db.indices.mensuales.parte.3)

db.indices.mensuales.parte.3$estacion <- factor(db.indices.mensuales.parte.3$estacion, 
                                                levels = c('Primavera', 'Verano', 'Otono', 'Invierno'))

head(db.indices.mensuales.parte.3)
str(db.indices.mensuales.parte.3)


# db 4
db.indices.mensuales.parte.4 <- rbind(db.rx1day, db.rx5day)
row.names(db.indices.mensuales.parte.4) <- 1:nrow(db.indices.mensuales.parte.4)

db.indices.mensuales.parte.4$estacion <- factor(db.indices.mensuales.parte.4$estacion, 
                                                levels = c('Primavera', 'Verano', 'Otono', 'Invierno'))

head(db.indices.mensuales.parte.4)
str(db.indices.mensuales.parte.4)

# fin ---




# Plot Anual ----

setwd(paste0(directorio.principal, comuna, '/', 'plots'))

nombre.plot <- paste0('indices_climaticos_anuales_', comuna, '.png') ; nombre.plot
png(nombre.plot, width = 850, height = 650, units = "px", type = 'cairo')

ggplot(db.indices.anuales, aes(x=fecha, y=valor) ) +
  geom_line() +
  labs(x = '', y = 'Valor') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = db.indices.anuales,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente), # x = -Inf, y = -Inf
    check_overlap = TRUE,
    hjust   = -0.05, # -0.1
    vjust   = -1, # -1
    inherit.aes=FALSE,
    size=3.9
  ) +
  
  scale_x_continuous(limits = c(1979, 2018), breaks=seq(1979, 2018, by=2)) +
  facet_wrap(~indice, ncol = 3, scales="free_y") +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 90, hjust = 1))

dev.off()

# fin ---




# Plots por estacion ----

# Parte 1

nombre.plot <- paste0('indices_climaticos_por_estacion_', comuna, '_parte_1', '.png') ; nombre.plot
png(nombre.plot, width = 850, height = 500, units = "px", type = 'cairo')

ggplot(db.indices.mensuales.parte.1, aes(x=fecha, y=valor)) +
  geom_line() +
  labs(x = '', y = 'Valor') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = db.indices.mensuales.parte.1,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
    check_overlap = TRUE,
    hjust   = -0.05,
    vjust   = -1,
    inherit.aes=FALSE,
    size=3.9
  ) +
  
  scale_x_continuous(limits = c(1979, 2018), breaks=seq(1979, 2018, by=2)) +
  facet_grid(indice~estacion, scales="free_y") +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 90, hjust = 1))

dev.off()


# Parte 2

nombre.plot <- paste0('indices_climaticos_por_estacion_', comuna, '_parte_2', '.png') ; nombre.plot
png(nombre.plot, width = 850, height = 500, units = "px", type = 'cairo')

ggplot(db.indices.mensuales.parte.2, aes(x=fecha, y=valor)) +
  geom_line() +
  labs(x = '', y = 'Valor') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = db.indices.mensuales.parte.2,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
    check_overlap = TRUE,
    hjust   = -0.05,
    vjust   = -1,
    inherit.aes=FALSE,
    size=3.9
  ) +
  scale_x_continuous(limits = c(1979, 2018), breaks=seq(1979, 2018, by=2)) +
  facet_grid(indice~estacion, scales="free_y") +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 90, hjust = 1))

dev.off()


# Parte 3

nombre.plot <- paste0('indices_climaticos_por_estacion_', comuna, '_parte_3', '.png') ; nombre.plot
png(nombre.plot, width = 850, height = 500, units = "px", type = 'cairo')

ggplot(db.indices.mensuales.parte.3, aes(x=fecha, y=valor)) +
  geom_line() +
  labs(x = '', y = 'Valor') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = db.indices.mensuales.parte.3,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
    check_overlap = TRUE,
    hjust   = -0.05,
    vjust   = -1,
    inherit.aes=FALSE,
    size=3.9
  ) +
  scale_x_continuous(limits = c(1979, 2018), breaks=seq(1979, 2018, by=2)) +
  facet_grid(indice~estacion, scales="free_y") +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 90, hjust = 1))

dev.off()


# Parte 4

nombre.plot <- paste0('indices_climaticos_por_estacion_', comuna, '_parte_4', '.png') ; nombre.plot
png(nombre.plot, width = 850, height = 500, units = "px", type = 'cairo')

ggplot(db.indices.mensuales.parte.4, aes(x=fecha, y=valor)) +
  geom_line() +
  labs(x = '', y = 'Valor') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  geom_text(
    data    = db.indices.mensuales.parte.4,
    mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
    check_overlap = TRUE,
    hjust   = -0.05,
    vjust   = -1,
    inherit.aes=FALSE,
    size=3.9
  ) +
  scale_x_continuous(limits = c(1979, 2018), breaks=seq(1979, 2018, by=2)) +
  facet_grid(indice~estacion, scales="free_y") +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 90, hjust = 1))

dev.off()

# fin ---


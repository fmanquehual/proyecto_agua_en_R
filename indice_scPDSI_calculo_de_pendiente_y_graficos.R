library(trend)
library(ggplot2)
library(lubridate)

rm(list=ls())
dev.off()

# DAN RESULTADOS MUY EXTRAÑOS!! NO RESULTO

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/proyecto_agua_en_R/')
source('funcion_ts_pdsi_a_db.R')

# Lectura de archivos ----

numero.de.pixel <- 1
comuna <- 'Imperial'
directorio.principal <- 'C:/Users/Usuario/Documents/Francisco/proyecto_agua/ERA_LAND/bases_de_datos/'

# Pixel 1
carpeta.de.archivos <- paste0(directorio.principal, comuna, '/', 'pixel_', numero.de.pixel, '/')
setwd(carpeta.de.archivos)

nombre.archivo <- list.files(pattern = 'PET_y_tp_pixel') ; nombre.archivo

db1 <- read.csv(nombre.archivo)
head(db1)


# Pixel 2

numero.de.pixel <- 2
carpeta.de.archivos <- paste0(directorio.principal, comuna, '/', 'pixel_', numero.de.pixel, '/')
setwd(carpeta.de.archivos)

nombre.archivo <- list.files(pattern = 'PET_y_tp_pixel') ; nombre.archivo

db2 <- read.csv(nombre.archivo)
head(db2)


# Pixel 3

numero.de.pixel <- 3
carpeta.de.archivos <- paste0(directorio.principal, comuna, '/', 'pixel_', numero.de.pixel, '/')
setwd(carpeta.de.archivos)

nombre.archivo <- list.files(pattern = 'PET_y_tp_pixel') ; nombre.archivo

db3 <- read.csv(nombre.archivo)
head(db3)


# Pixel 4

numero.de.pixel <- 4
carpeta.de.archivos <- paste0(directorio.principal, comuna, '/', 'pixel_', numero.de.pixel, '/')
setwd(carpeta.de.archivos)

nombre.archivo <- list.files(pattern = 'PET_y_tp_pixel') ; nombre.archivo

db4 <- read.csv(nombre.archivo)
head(db4)

# fin ---




# Calculo scPDSI ----

# Without self-calibrating, it use the default parameters of Palmer (1965).

sc_pdsi_1 <- pdsi(db1$P, db1$PE, start = 1990, sc = TRUE)
sc_pdsi_2 <- pdsi(db2$P, db2$PE, start = 1990, sc = TRUE)
sc_pdsi_3 <- pdsi(db3$P, db3$PE, start = 1990, sc = TRUE)
sc_pdsi_4 <- pdsi(db4$P, db4$PE, start = 1990, sc = TRUE)

# fin ---




# Calculo de pendiente ----

pendiente_sc_pdsi_1 <- sens.slope(sc_pdsi_1$X)
pendiente_sc_pdsi_2 <- sens.slope(sc_pdsi_2$X)
pendiente_sc_pdsi_3 <- sens.slope(sc_pdsi_3$X)
pendiente_sc_pdsi_4 <- sens.slope(sc_pdsi_4$X)

# fin ---




# De formato ts a data frame ----

db_sc_pdsi_1 <- ts_pdsi_a_db(sc_pdsi_1$X, 1990:2018, numero_de_pixel = 1)
db_sc_pdsi_2 <- ts_pdsi_a_db(sc_pdsi_2$X, 1990:2018, numero_de_pixel = 2)
db_sc_pdsi_3 <- ts_pdsi_a_db(sc_pdsi_3$X, 1990:2018, numero_de_pixel = 3)
db_sc_pdsi_4 <- ts_pdsi_a_db(sc_pdsi_4$X, 1990:2018, numero_de_pixel = 4)

head(db_sc_pdsi_1)
head(db_sc_pdsi_2)
head(db_sc_pdsi_3)
head(db_sc_pdsi_4)

db_sc_pdsi <- rbind(db_sc_pdsi_1, db_sc_pdsi_2, db_sc_pdsi_3, db_sc_pdsi_4)
head(db_sc_pdsi)
dim(db_sc_pdsi)
str(db_sc_pdsi)

db_sc_pdsi$pixel <- factor(db_sc_pdsi$pixel, levels = sort(unique(db_sc_pdsi$pixel)))
db_sc_pdsi$anho <- year(db_sc_pdsi$fecha)
db_sc_pdsi$mes <- month(db_sc_pdsi$fecha)

head(db_sc_pdsi)
str(db_sc_pdsi)

# fin ---




# Plot ----

par(mfrow=c(2,2))

plot(sc_pdsi_1)
plot(sc_pdsi_2)
plot(sc_pdsi_3)
plot(sc_pdsi_4)

# fin ---

db_sc_pdsi_i <- db_sc_pdsi[db_sc_pdsi$mes==1,]


ggplot(db_sc_pdsi_i, aes(x=anho, y=pdsi) ) +
  geom_point() +
  geom_line() +
  labs(x = '', y = 'PDSI') +
  geom_smooth(method = lm, se = FALSE, col = 'red') +
  # geom_text(
  #   data    = db.spi.con.pendiente,
  #   mapping = aes(x = -Inf, y = -Inf, label = leyenda.valor.pendiente),
  #   check_overlap = TRUE,
  #   hjust   = -0.1,
  #   vjust   = -1,
  #   inherit.aes=FALSE
  # ) +
  # scale_y_continuous(limits = c(-5, 5), breaks=seq(-5, 5, by=1)) +
  scale_x_continuous(limits = c(1982, 2018), breaks=seq(1982, 2018, by=2)) +
  # geom_hline(yintercept=intercepto, linetype="dashed", color = 'black') +
  facet_wrap(~pixel, ncol = 2) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1))

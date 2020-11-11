calculo_de_pendiente_indices_climaticos_por_estacion <- function(base_de_datos, anho_inicial){
  # base_de_datos <- db.txx0
    
  verano <- 1:3
  otonho <- 4:6
  invierno <- 7:9
  primavera <- 10:12
  
  base_de_datos$fecha <- paste0(base_de_datos$fecha, '-01')
  base_de_datos$fecha <- as.Date(base_de_datos$fecha)
  
  base_de_datos$mes <- month(base_de_datos$fecha)
  base_de_datos$estacion <- NA
  
  base_de_datos$estacion[base_de_datos$mes%in%verano] <- 'Verano'
  base_de_datos$estacion[base_de_datos$mes%in%otonho] <- 'Otono'
  base_de_datos$estacion[base_de_datos$mes%in%invierno] <- 'Invierno'
  base_de_datos$estacion[base_de_datos$mes%in%primavera] <- 'Primavera'
  
  base_de_datos <- base_de_datos[,c('fecha', 'indice', 'valor', 'estacion')]
  
  
  # Verano
  db.verano0 <- subset(base_de_datos, estacion=='Verano')
  
  db.verano0$anho <- year(db.verano0$fecha)
  anhos <- names(tapply(db.verano0$valor, db.verano0$anho, mean))
  indice <- unique(db.verano0$indice)
  valor <- as.numeric(tapply(db.verano0$valor, db.verano0$anho, mean))
  estacion <- unique(db.verano0$estacion)
  
  db.verano <- data.frame(fecha=anhos, indice=indice, valor=valor, estacion=estacion)
  db.verano$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.verano, anho_inicial = anho_inicial)
  
  
  # Otonho
  db.otonho0 <- subset(base_de_datos, estacion=='Otono')
  
  db.otonho0$anho <- year(db.otonho0$fecha)
  anhos <- names(tapply(db.otonho0$valor, db.otonho0$anho, mean))
  indice <- unique(db.otonho0$indice)
  valor <- as.numeric(tapply(db.otonho0$valor, db.otonho0$anho, mean))
  estacion <- unique(db.otonho0$estacion)
  
  db.otonho <- data.frame(fecha=anhos, indice=indice, valor=valor, estacion=estacion)
  db.otonho$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.otonho, anho_inicial = anho_inicial)
  
  
  # Invierno
  db.invierno0 <- subset(base_de_datos, estacion=='Invierno')
  
  db.invierno0$anho <- year(db.invierno0$fecha)
  anhos <- names(tapply(db.invierno0$valor, db.invierno0$anho, mean))
  indice <- unique(db.invierno0$indice)
  valor <- as.numeric(tapply(db.invierno0$valor, db.invierno0$anho, mean))
  estacion <- unique(db.invierno0$estacion)
  
  db.invierno <- data.frame(fecha=anhos, indice=indice, valor=valor, estacion=estacion)
  db.invierno$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.invierno, anho_inicial = anho_inicial)
  
  
  # Primavera
  db.primavera0 <- subset(base_de_datos, estacion=='Primavera')
  
  db.primavera0$anho <- year(db.primavera0$fecha)
  anhos <- names(tapply(db.primavera0$valor, db.primavera0$anho, mean))
  indice <- unique(db.primavera0$indice)
  valor <- as.numeric(tapply(db.primavera0$valor, db.primavera0$anho, mean))
  estacion <- unique(db.primavera0$estacion)
  
  db.primavera <- data.frame(fecha=anhos, indice=indice, valor=valor, estacion=estacion)
  db.primavera$leyenda.valor.pendiente <- calculo_de_pendiente_indices_climaticos(db.primavera, anho_inicial = anho_inicial)
  
  db.out <- rbind(db.verano, db.otonho, db.invierno, db.primavera)
  db.out$fecha <- as.numeric(db.out$fecha)
  
  return(db.out)
  
}

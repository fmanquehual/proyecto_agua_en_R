# Supuesto: Cada pixel tiene igual longitud


pendiente_media_para_curso_de_agua_principal <- function(capa_de_pendiente, capa_de_red_hidrica, numero_de_pixeles_a_promediar=4){
  
  capa_de_red_hidrica <- capa_de_red_hidrica
  
  # seleccionando el cursp de agua principal
  
  orden.maximo <- max(capa_de_red_hidrica@data$strahler)
  id <- which(capa_de_red_hidrica@data$strahler==orden.maximo)
  curso_de_agua_principal <- capa_de_red_hidrica[id,]
  
  
  
  # cortando capa de pendiente con la capa de curso de agua 
  
  pendiente_curso_de_agua_principal <- mask(capa_de_pendiente, curso_de_agua_principal)
  pendiente_curso_de_agua_principal_poligono <- raster::rasterToPolygons(pendiente_curso_de_agua_principal, n=4, na.rm = TRUE)
  
  
  
  # calculando la pendiente por tramo
  
  valores_de_pendiente <- pendiente_curso_de_agua_principal_poligono@data[,1]
  numero.de.pixeles <- length(valores_de_pendiente)
  secuencia.de.iteracion <- seq(1, numero.de.pixeles, by=numero_de_pixeles_a_promediar)
  
  numerador <- length(valores_de_pendiente)
  
  denominador <- c()
  for (i in secuencia.de.iteracion) {
    
    #i <-  secuencia.de.iteracion[3]
    
    pendiente.de.longitud.i <- mean( valores_de_pendiente[c( i:(i+numero_de_pixeles_a_promediar) )] , na.rm=TRUE)
    
    denominador.i <- 1/sqrt(pendiente.de.longitud.i)
    
    denominador <- sum(denominador, denominador.i)
    
    if(is.na(denominador)){message(i)}
    
  }
  
  
  
  # calculo de la pendiente media del curso de agua principal segun Taylor y Schwarz (1952)
  
  S <- (numerador/denominador)^2
  
  return(S)
  
}
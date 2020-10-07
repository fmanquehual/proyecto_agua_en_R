pendiente_media_para_curso_de_agua_principal <- function(base_de_datos, numero_de_pixeles_a_promediar=4){
  
  # calculando la pendiente por tramo
  
  valores_de_pendiente <- base_de_datos[,2]
  numero.de.pixeles <- length(valores_de_pendiente)
  secuencia.de.iteracion <- seq(1, numero.de.pixeles, by=numero_de_pixeles_a_promediar)
  
  numerador <- length(valores_de_pendiente)
  
  denominador <- c()
  for (i in secuencia.de.iteracion) {
    
    pendiente.de.longitud.i <- mean( valores_de_pendiente[c( i:(i+numero_de_pixeles_a_promediar) )] , na.rm=TRUE)
    
    denominador.i <- 1/sqrt(pendiente.de.longitud.i)
    
    denominador <- sum(denominador, denominador.i)
    
    if(is.na(denominador)){message(i)}
    
  }
  
  
  # calculo de la pendiente media del curso de agua principal segun Taylor y Schwarz (1952)
  
  S <- (numerador/denominador)^2
  
  return(S)
  
}
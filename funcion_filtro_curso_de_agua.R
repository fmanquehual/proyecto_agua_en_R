filtro_curso_de_agua <- function(capa, orden){
  
  id <- which(capa@data$strahler==orden)
  red.i <- capa[id,]
  
  return(red.i)
  
}

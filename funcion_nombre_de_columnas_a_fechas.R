nombre_de_columnas_a_fechas <- function(base_de_datos, eliminar_ultimas_dos_columnas=FALSE){
  
  if(eliminar_ultimas_dos_columnas==TRUE){
    nombres.originales <- colnames(base_de_datos)[-c((ncol(base_de_datos)-1), ncol(base_de_datos))]
    } else(nombres.originales <- colnames(base_de_datos))
  
  nombres.modificados1 <- gsub('GMT', '', nombres.originales)
  nombres.modificados2 <- gsub('X', '', nombres.modificados1)
  nombres.modificados3 <- gsub('\\.', '-', nombres.modificados2)
  nombres.como.fechas <- as.Date(nombres.modificados3)
  
  return(nombres.como.fechas)

}
calculo_de_pendiente_indices_climaticos <- function(base_de_datos, anho_inicial=1979){
  
  ts.i <- ts(base_de_datos$valor, start = 1979, freq = 1)
  
  slope.i <- sens.slope(ts.i,) ; slope.i
  valor.pendiente <- round(slope.i$estimates, 3)
  valor.p.value <- round(slope.i$p.value, 3)
  
  leyenda.valor.pendiente <- paste0('Pendiente = ', valor.pendiente, 
                                    ' (', 'p-value = ', valor.p.value, ')')
  
  return(leyenda.valor.pendiente)
  
}

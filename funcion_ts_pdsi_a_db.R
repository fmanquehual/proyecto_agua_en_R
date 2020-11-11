ts_pdsi_a_db <- function(valores_pdsi, anhos, numero_de_pixel){
  # valores_pdsi <- sc_pdsi_1$X  
  
  valores_pdsi_i <- as.vector(valores_pdsi)
  db_pdsi <- data.frame(pdsi=valores_pdsi_i)
  
  meses <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
  
  db_pdsi$anhos <- sort(rep(anhos, 12))
  db_pdsi$mes <- rep(meses, length(anhos))
  db_pdsi$dia <- '01'
  db_pdsi$fecha <- paste(db_pdsi$anhos, db_pdsi$mes, db_pdsi$dia, sep = '-')
  db_pdsi$fecha <- as.Date(db_pdsi$fecha)
  db_pdsi <- db_pdsi[,c('fecha', 'pdsi')]
  db_pdsi$pixel <- numero_de_pixel
  
  return(db_pdsi)
  
}
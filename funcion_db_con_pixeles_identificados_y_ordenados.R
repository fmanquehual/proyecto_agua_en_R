db_con_pixeles_identificados_y_ordenados <- function(raster_de_pendiente, capa_de_red_hidrica,
                                                      error_de_distancia_respecto_a_pixel_inicial=10,
                                                     mostrar_plot_con_ordenamiento_de_pixeles=FALSE){
  
  # raster_de_pendiente <- pendiente.cuenca
  # capa_de_red_hidrica <- red.hidrica
  # error_de_distancia_respecto_a_pixel_inicial <- 20
  # mostrar_plot_con_ordenamiento_de_pixeles <- TRUE
  
  # seleccionando el curso de agua principal
  
  orden.maximo <- max(capa_de_red_hidrica@data$strahler)
  id <- which(capa_de_red_hidrica@data$strahler==orden.maximo)
  curso_de_agua_principal <- capa_de_red_hidrica[id,]
  
  message(paste('[', Sys.time(), ']', ' Seleccion curso principal - Listo', sep = ''))
  
  
  # raster de pendiente del curso de agua principal
  pendiente_curso_de_agua_principal <- mask(raster_de_pendiente, curso_de_agua_principal)
  pendiente_curso_de_agua_principal_poligono <- raster::rasterToPolygons(pendiente_curso_de_agua_principal, n=4, na.rm = TRUE)
  pendiente_curso_de_agua_principal_clip <- crop(pendiente_curso_de_agua_principal, pendiente_curso_de_agua_principal_poligono)
  
  message(paste('[', Sys.time(), ']', ' Corte de capa de pendiente - Listo', sep = ''))
  
  
  # idetificacion de pixel inicial
  message('Haz click en el plot donde, aproximadamente, se encuentre el pixel inicial')
  
  plot(pendiente_curso_de_agua_principal_clip)
  ubicacion_del_pixel_inicial <- locator(n=1)
    
  pixeles.y.coordenadas <- raster::as.data.frame(pendiente_curso_de_agua_principal_clip, xy=TRUE, na.rm=TRUE)
  pixeles.y.coordenadas$diferencia.x <- round(pixeles.y.coordenadas$x-ubicacion_del_pixel_inicial$x, 0)
  pixeles.y.coordenadas$diferencia.y <- round(pixeles.y.coordenadas$y-ubicacion_del_pixel_inicial$y, 0)
  pixeles.y.coordenadas$diferencia.absoluta <- abs(pixeles.y.coordenadas$diferencia.x)+abs(pixeles.y.coordenadas$diferencia.y)
  
  pixel.inicial <- subset(pixeles.y.coordenadas, diferencia.absoluta<=error_de_distancia_respecto_a_pixel_inicial)
  id <- which(pixel.inicial$diferencia.absoluta%in%min(pixel.inicial$diferencia.absoluta))
  pixel.inicial <- pixel.inicial[id,]
  
  if(nrow(pixel.inicial)==0){stop('No se pudo identificar al pixel inicial.\n  Intenta con una nueva ubicacion o aumentando el error de distancia')}
  
  origen.x <- colFromX(pendiente_curso_de_agua_principal_clip, pixel.inicial$x)
  origen.y <- rowFromY(pendiente_curso_de_agua_principal_clip, pixel.inicial$y)
  
  message(paste('[', Sys.time(), ']', ' Identificacion de pixel inicial - Listo', sep = ''))
  
  
  # creando un raster que nos ayudara a ordenar y/o identificar cada pixel
  matriz.orden <- pendiente_curso_de_agua_principal_clip
  matriz.orden[is.na(matriz.orden)] <- -1

  for (i in 1:nrow(pixeles.y.coordenadas)){
    # i <- 71
    
    #valor.de.pixel.i <- c()
    valor.de.pixel.i <- suppressWarnings( c(
      matriz.orden[origen.y-1, origen.x], # arriba
      matriz.orden[origen.y, origen.x+1], # derecha
      matriz.orden[origen.y+1, origen.x], # abajo
      matriz.orden[origen.y, origen.x-1] # izquierda
    ) )
    
    if(i==1){valor.de.i.anterior <- NA}
      
    movimiento.obligado.diagonal <- length(valor.de.pixel.i[!valor.de.pixel.i%in%c(-1, valor.de.i.anterior, NA)])
    
    if(movimiento.obligado.diagonal==0){
      valor.de.pixel.i <- suppressWarnings( c(
        matriz.orden[origen.y-1, origen.x+1], # arriba-derecha
        matriz.orden[origen.y+1, origen.x+1], # abajo-derecha
        matriz.orden[origen.y+1, origen.x-1], # abajo-izquierda
        matriz.orden[origen.y-1, origen.x-1] # arriba-izquierda
       ) )
      } 
    
    valores.de.pixel.NA <- valor.de.pixel.i[is.na(valor.de.pixel.i)]
    if(length(valores.de.pixel.NA)>=1){valor.de.pixel.i[is.na(valor.de.pixel.i)] <- -1}
    
    #opciones.de.pixel.siguiente <- c()
    if(movimiento.obligado.diagonal!=0){
           opciones.de.pixel.siguiente <- data.frame(id=1:length(valor.de.pixel.i), valor.pixel=valor.de.pixel.i,
                                              movimiento.en.x=c(0,1,0,-1), movimiento.en.y=c(-1,0,1,0))
    } else(opciones.de.pixel.siguiente <- data.frame(id=1:length(valor.de.pixel.i), valor.pixel=valor.de.pixel.i,
                                                     movimiento.en.x=c(1,1,-1,-1), movimiento.en.y=c(-1,1,1,-1)))
    
    matriz.orden[origen.y, origen.x] <- i
    
    if(i==1){valor.de.i.anterior <- -1}
    
    movimiento.obligado.en.y <- length(opciones.de.pixel.siguiente$id[!opciones.de.pixel.siguiente$valor.pixel%in% c(-1, valor.de.i.anterior)])
    
    if(movimiento.obligado.en.y>=2){opciones.de.pixel.siguiente <- opciones.de.pixel.siguiente[-c(2,4),]}
    
    #movimiento.en.x <- c()
    movimiento.en.x <- opciones.de.pixel.siguiente$movimiento.en.x[!opciones.de.pixel.siguiente$valor.pixel%in%c(-1, valor.de.i.anterior)]
    #movimiento.en.y <- c()
    movimiento.en.y <- opciones.de.pixel.siguiente$movimiento.en.y[!opciones.de.pixel.siguiente$valor.pixel%in%c(-1, valor.de.i.anterior)]
    
    valor.de.i.anterior <- c(i,i-1, i-2)
    #x <- c()
    #y <- c()
    
    if(i==1){ c( x <- (origen.x+movimiento.en.x[1]), y <- (origen.y+movimiento.en.y[1]) )
      } else( c( x <- (origen.x+movimiento.en.x[1]), y <- (origen.y+movimiento.en.y[1]) ) )
    
    origen.x <- x
    #origen.x <- origen.x[length(origen.x)]
    
    origen.y <- y
    #origen.y <- origen.x[length(origen.y)]
    
  }
  
  matriz.orden[matriz.orden==-1] <- NA
  
  if(mostrar_plot_con_ordenamiento_de_pixeles==TRUE){c(
    plot(matriz.orden),
    text(matriz.orden))
  }
  
  message(paste('[', Sys.time(), ']', ' Raster con valor id para cada pixel - Listo', sep = ''))
  
  
  # creando una db con la informacion necesaria para identificar y ordenar los pixeles
  db.pixeles.y.coordenadas <- pixeles.y.coordenadas[,c(3, 1, 2)]
  db.pixeles.y.coordenadas$fila <- colFromX(pendiente_curso_de_agua_principal_clip, db.pixeles.y.coordenadas$x)
  db.pixeles.y.coordenadas$columna <- rowFromY(pendiente_curso_de_agua_principal_clip, db.pixeles.y.coordenadas$y)
  db.pixeles.y.coordenadas$x_y <- paste(db.pixeles.y.coordenadas$x, db.pixeles.y.coordenadas$y, sep='_')
  
  db.matriz.orden <- raster::as.data.frame(matriz.orden, xy=TRUE)
  db.matriz.orden$x_y <- paste(db.matriz.orden$x, db.matriz.orden$y, sep = '_')
  colnames(db.matriz.orden)[3] <- 'id'
  db.matriz.orden <- db.matriz.orden[,c('id', 'x_y')]
    
  db.out0 <- merge(db.pixeles.y.coordenadas, db.matriz.orden, by='x_y')
  db.out <- db.out0[order(db.out0$id),]
  db.out <- db.out[,c(7, 2, 5, 6, 3, 4)]
  
  message(paste('[', Sys.time(), ']', ' Base de datos - Listo', sep = ''))
  
  
  return(db.out)
  
}

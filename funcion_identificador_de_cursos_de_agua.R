library(raster)
library(rgdal)
library(rgeos)

rm(list=ls())
dev.off()


# Lectura de capas ----

#setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_para_identificador_de_cursos_de_agua/')
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/')
red_hidrica0 <- readOGR('.', 'VectorRedPadreCasas')

id <- which(red_hidrica0@data$strahler!=1)
red_hidrica_mask <- red_hidrica0[id,]
red_hidrica_mask <- buffer(red_hidrica_mask, width=10)

id <- which(red_hidrica0@data$strahler==1)
red_hidrica00 <- red_hidrica0[id,]
db_red_hidrica <- red_hidrica00@data

red_hidrica000 <- gDifference(red_hidrica00, red_hidrica_mask, byid=TRUE, id=row.names(db_red_hidrica))
red_hidrica <- SpatialLinesDataFrame(red_hidrica000, data = db_red_hidrica, match.ID=TRUE)

plot(red_hidrica, axes=TRUE)
head(red_hidrica@data)

red_hidrica@data$id <- 1:nrow(red_hidrica@data)
numero.de.objetos <- 1:nrow(red_hidrica@data) ; numero.de.objetos


# seleccion de 1 segmento

for (i in numero.de.objetos) {
#  i <- 2
  
  id.i <- which(red_hidrica@data$id==numero.de.objetos[i])
  red_hidrica_filtro <- red_hidrica[id.i,]
  
  coordenadas <- coordinates(red_hidrica_filtro)
  coordenadas <- as.data.frame(coordenadas)
  coordenadas <- coordenadas[c(1,nrow(coordenadas)),]

  
  # Transformacion a Spatial Points Data Frame
  
  segmento.i0 <- SpatialPoints(c(coordenadas[1], coordenadas[2]), proj4string = crs(red_hidrica_filtro))
  
  inicio.i <- paste('inicio', i, sep = '_')
  fin.i <- paste('fin', i, sep = '_')
  leyenda.strahler <- paste('Strahler', '=', red_hidrica_filtro$strahler, sep = ' ')
    
  db.segmento.i <- data.frame(nombre = c(inicio.i, fin.i), id = i, 
                              leyenda.strahler=leyenda.strahler,
                              valor.strahler=red_hidrica_filtro$strahler,
                              x=as.numeric(coordenadas[,1]), y=as.numeric(coordenadas[,2]))
  
  segmento.i <- SpatialPointsDataFrame(segmento.i0, data = db.segmento.i, match.ID = TRUE)
  
  if(i==1){segmentos.en.bruto <- segmento.i} else(segmentos.en.bruto <- raster::union(segmentos.en.bruto, segmento.i))
}

plot(red_hidrica, lwd=2, axes=TRUE)
plot(segmentos.en.bruto, pch = 16, col = 'red', add=TRUE)

segmentos.en.bruto@data$x_y <- paste(segmentos.en.bruto@data$x, segmentos.en.bruto@data$y, sep = '_')
segmentos.en.bruto@data$id_utilizado <- 0

segmentos.en.bruto@data
for(j in 1:nrow(segmentos.en.bruto@data)) {
 #j <- 19

  if(segmentos.en.bruto@data$id_utilizado[j]==1){next}
    
  valor.strahler.referencia <- as.numeric(segmentos.en.bruto@data$valor.strahler[j])
  x_y.i <- segmentos.en.bruto@data$x_y[j]
  
  match.i <- which(segmentos.en.bruto@data$x_y%in%x_y.i)
  
  ej0 <- segmentos.en.bruto[match.i,] ; ej0@data
  ej <- subset(ej0, valor.strahler==valor.strahler.referencia) ; ej@data
  
  id.match <- ej@data$id
  
  ej@data$valor.strahler <- as.numeric(ej@data$valor.strahler)
  valor.strahler.i <- unique(ej@data$valor.strahler)
  
  if(length(valor.strahler.i)>=2){next}
  
  segmentos.en.bruto@data$id_utilizado[segmentos.en.bruto@data$id%in%id.match] <- 1

  ej2 <- red_hidrica[id.match,]
  ej2@data <- as.data.frame(ej2@data[1,c('strahler')])
  colnames(ej2@data) <- 'strahler'
  
  plot(red_hidrica, lwd=2, axes=TRUE)
  plot(segmentos.en.bruto, pch = 16, col = 'red', add=TRUE)
  plot(ej2, lwd=2, col='green', add=TRUE)
  
  valores.de.strahler <- unique( as.numeric(ej2@data$strahler) )

  for (i in 1:nrow(ej2@data)) {
  # i <- 1
      linea.unica0 <- gLineMerge(ej2)
      linea.unica <- SpatialLinesDataFrame(linea.unica0, data = ej2@data, match.ID=TRUE)
      
      # plot(linea.unica, col='red', lwd=2, add=TRUE)
      # linea.unica2@data
    
    if(j==1 & i==1){
    
        if(length(valores.de.strahler)==1){segmento.depurado <- linea.unica} else(
                                           segmento.depurado <-  ej2)
    } else(c(
      
        #valor.strahler.de.segmento.anterior <- as.numeric( segmento.depurado@data$strahler[length(segmento.depurado@data)] ),
      
        #if(length(valores.de.strahler)==1){c( # 'c(' es nuevo
          
          #if(valores.de.strahler==valor.strahler.de.segmento.anterior){c(
              db.segmento.depurado <- segmento.depurado@data,
              inteserccion.i <- gIntersection(segmento.depurado, linea.unica),
              no.existe.interseccion <- is.null(inteserccion.i),
              
              if(no.existe.interseccion==FALSE){
                c(
                  linea.unica2 <- gUnion(segmento.depurado, linea.unica, byid=TRUE, id=row.names(db.segmento.depurado)),
                #linea.unica3 <- SpatialLinesDataFrame(linea.unica2, data = db.segmento.depurado, match.ID=TRUE)
                segmento.depurado <- SpatialLinesDataFrame(linea.unica2, data = db.segmento.depurado, match.ID=TRUE)
                )
                } else(segmento.depurado <- raster::union(segmento.depurado, linea.unica))
             ) 
            
            #} else(segmento.depurado <- raster::union(segmento.depurado, linea.unica))
            
           
         # } else(segmento.depurado <- raster::union(segmento.depurado, ej2))
        #)
       )

  }
}

segmento.depurado@data
plot(red_hidrica, lwd=2, axes=TRUE)
plot(segmentos.en.bruto, pch = 16, col = 'red', add=TRUE)
plot(segmento.depurado, lwd=2, col='green', add=TRUE)
# text(segmentos.en.bruto, segmentos.en.bruto$leyenda.strahler, pos = c(1:4))

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/coberturas_para_identificador_de_cursos_de_agua/')
writeOGR(segmento.depurado, ".", "segmento_depurado_13", driver="ESRI Shapefile",
        overwrite_layer = TRUE)

# fin ---
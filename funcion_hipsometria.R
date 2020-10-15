hipsometria <- function (x, absolute = TRUE, main = "", col = "blue", AddDen =T, colDen="red", ...) 
{   
  require(raster)
  require(rgdal)
  
  z <- getValues(x)
  zmin <- minValue(x)
  zmax <- maxValue(x)
  
  areaCN <- length(which(!is.na(z))) * xres(x) * yres(x)  #Area of catchment
  sfun <- plot.stepfun(ecdf(z))
  
  if(any(which(sfun$y==0.5))){
    zidmean <- which(sfun$y==0.5)[1]
    zmean <- sfun$t[zidmean]}
  
  x2id <- which(sfun$y>0.5)[1]
  x1id <- which(sfun$y<0.5)[length(which(sfun$y<0.5))]
  x2 <- sfun$y[x2id]
  x1 <- sfun$y[x1id]  
  y1 <- sfun$t[x1id]
  y2 <- sfun$t[x2id]
  
  zmean <- (0.5 - x1) * (y2 - y1) / (x2 - x1) + y1  
  
  relative.area <- (1 - sfun$y[-1])
  relative.elev <- (sfun$t[-c(1, length(sfun$t))] - zmin)/(zmax -  zmin)
  
  f <- splinefun(relative.area, relative.elev, method = "fmm")
  integral <- integrate(f = f, lower = 0, upper = 1)
  
  
  absolute.area <- (areaCN - sfun$y * areaCN )[-1]/1e+6
  absolute.elev <- sfun$t[-c(1, length(sfun$t))]
  
  if(absolute == TRUE){
    
    
    if(AddDen == T){ 
      plot(absolute.area, absolute.elev, main = main, type = "l", col = col, ...)
      xyd <- density(getValues(x),na.rm=T,from=minValue(x),to=maxValue(x))      
      cte <- areaCN/max(xyd$y)
      xden <- (xyd$y*cte) / 1e+6
      yden <- xyd$x
      lines(xden,yden,ty="l",col=colDen)
      axis(3, at= pretty(xden) ,labels=format(pretty(xden)/cte * 1e+6, digits =2 ))
      mtext("Densidad",side=3,line=2)
      legend("topright", bty = "n", 
             c("Curva hipsométrica", "Función densidad"), 
             lty=c(1,1),col=c(col,colDen),xjust = 1, yjust = 1,cex=0.8)
      legend("topleft", bty = "n", c(#paste("Elevación media:",round(zmean,2), "[m.s.n.m]"),
                                        # paste("Elevación máxima:", round(zmax,2), "[m.s.n.m]"),
                                        # paste("Elevación mínima:", round(zmin,2), "[m.s.n.m]"),
                                        # paste("Área:", round(areaCN/1e+6,2),"[km2]"),
                                        paste("Integral:",round(integral$value,3)),
                                        paste("Error:",round(integral$abs.error,3))),cex=.8)
    } else{
      plot(absolute.area, absolute.elev, main = main, type = "l", col = col, ...)
      legend("bottomleft", bty = "n", c(#paste("Elevación media:",round(zmean,2), "[m.s.n.m]"),
                                        # paste("Elevación máxima:", round(zmax,2), "[m.s.n.m]"),
                                        # paste("Elevación mínima:", round(zmin,2), "[m.s.n.m]"),
                                        # paste("Área:", round(areaCN/1e+6,2),"[km2]"),
                                        paste("Integral:",round(integral$value,3)),
                                        paste("Error:",round(integral$abs.error,3))),cex=.8)}
    
  } else {
    
    
    
    
    if(AddDen == T){  
      plot(relative.area, relative.elev, main = main, type = "l", col = col, ...)
      xyd <- density(getValues(x),na.rm=T)      
      cte <- 1/max(xyd$y)
      
      xden <- (xyd$y*cte) 
      yden <- (xyd$x-xyd$x[1])/(xyd$x[length(xyd$x)]-xyd$x[1])
      lines(xden,yden,ty="l",col=colDen)
      axis(3, at= pretty(xden) ,labels=format(pretty(xden)/cte, digits =2 ))
      mtext("Densidad",side=3,line=2)
      
      legend("bottomleft", bty = "n", 
             c("Curva hipsométrica", "Función densidad"), 
             lty=c(1,1),col=c(col,colDen),xjust = 1, yjust = 1,cex=0.8)
      legend("topright", bty = "n", c(#paste("Elevación media:",round(zmean,2), "[m.s.n.m]"),
                                        # paste("Elevación máxima:", round(zmax,2), "[m.s.n.m]"),
                                        # paste("Elevación mínima:", round(zmin,2), "[m.s.n.m]"),
                                        # paste("Área:", round(areaCN/1e+6,2),"[km2]"),
                                        paste("Integral:",round(integral$value,3)),
                                        paste("Error:",round(integral$abs.error,3))),cex=.8)
      
    } else{
      plot(relative.area, relative.elev, main = main, type = "l", col = col, ...)
      legend("bottomleft", bty = "n", c(#paste("Elevación media:",round(zmean,2), "[m.s.n.m]"),
      #                                   paste("Elevación máxima:", round(zmax,2), "[m.s.n.m]"),
      #                                   paste("Elevación mínima:", round(zmin,2), "[m.s.n.m]"),
      #                                   paste("Área:", round(areaCN/1e+6,2),"[km2]"),
                                        paste("Integral:",round(integral$value,3)),
                                        paste("Error:",round(integral$abs.error,3))),cex=.8)}
    
    
  }
  
  resultados <- c("Elev Media"=zmean, "Elev Max" = zmax, "Elev Min" = zmin,
                  "Area"=areaCN, "Integral"=integral$value, "Error"=integral$abs.error )
  resultados
  
}

# fuente: https://rpubs.com/joeantonio/71409
# - https://rstudio-pubs-static.s3.amazonaws.com/71408_b030e34a487f46d4ac1a0e5ecf67f5d5.html
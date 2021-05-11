# funciones

#### recta de regresion ####
recta.reg = function(x, y, residuo = FALSE, plot = FALSE, Summary = FALSE){
  recta = lm(y~x)
  
  if(!residuo){
    a = recta$coefficients[1]
    b = recta$coefficients[2]
    result = c(a,b)

  } else {
    a = recta$coefficients[1]
    b = recta$coefficients[2]
    res = recta$residuals
  
    result = list()
    result[[1]] = c(a,b); result[[2]] = res

  }
  
  if(plot){
    
    plot(x = x, y = y)
    lines(x = x, y = a + b*x, col = "red", lwd = 2)
    
  }
  
  if(Summary){
    print(summary(recta))
  }
  
  return(result)
}

#### test biserial ####

biserial.test = function(tabla, intervalos, alpha){
  
  # intervalos debe ser el valor medio de los intervalos. ingresar un vector con esos valores
  
  if(!is.data.frame(tabla) & !is.matrix(tabla)){
    stop("tabla debe ser un data.frame o una matriz")
  }
  
  if(length(tabla[,1]) != length(intervalos)){
    stop("la longitud de intervalos debe ser igual a la cantidad de filas")
  }
  
  if(is.character(alpha) | alpha > 1){
    stop("ALPHA debe estar entre 0 y 1")
  }
  
  # suma de las columnas
  tabla = cbind(tabla, total = apply(tabla, c(1), sum))
  
  
  # suma de las filas
  tot1 = sum(tabla[,1])
  tot2 = sum(tabla[,2])
  tot = sum(tabla[,3])
  
  # calculo del coeficiente rb
  
  x.si = sum(tabla[,1]*intervalos)/tot1 
  x.no = sum(tabla[,2]*intervalos)/tot2 
  x.to = sum(tabla[,3]*intervalos)/tot 
  
  
  sb = sqrt(sum(((intervalos-x.to)**2)*tabla[,3])/tot)
  p = tot1/tot
  q = 1 - p
  
  # valores de tabla
  z = qnorm(q)
  zp = dnorm(z)
  
  # coeficiente
  rb = (abs(x.si-x.no)/sb)*p*q/zp
  
  # sigma de la distribucion normal
  sigmab = ((sqrt(p*q)/zp) - rb**2)/sqrt(tot)
  
  # estadistico 
  est = rb/sigmab
  
  # Z critico
  zc = qnorm(p = 1-alpha/2)
  
  # test
  if(abs(est) > abs(zc)){
    print(paste("Rechazo H0 con un ", (1-alpha)*100, "% de confianza", sep = ""), quote = F)
  } else {
    print(paste("NO rechazo H0 con un ", (1-alpha)*100, "% de confianza", sep = ""), quote = F)
  }
  
  result = c(rb, sigmab, est, zc)
  names(result) = c("Rb", "Sigma b", "Estadistico", "Z.critico")
  
  return(result)
  
}


#### tetracor_test ####
tetracor_test = function(tabla, alpha){
  
  if(!is.data.frame(tabla) & !is.matrix(tabla)){
    stop("tabla debe ser un data.frame o una matriz")
  }
  
  if(length(tabla[,1]) != 2 & length(tabla[1,]) != 2){
    stop("la tabla debe ser de 2x2")
  }
  
  if(is.character(alpha) | alpha > 1){
    stop("ALPHA debe estar entre 0 y 1")
  }
  
  
  aux = (sqrt(tabla[1,1]*tabla[2,2]) - sqrt(tabla[1,2]*tabla[2,1]))/sqrt(tabla[1,1]*tabla[2,2]) + sqrt(tabla[1,2]*tabla[2,1])
  
  # Rt
  rt = sin((pi/2)*aux)
  
  
  # zh y zk
  
  n = sum(tabla)
  
  ph = sum(tabla[1,])/n
  pk = sum(tabla[2,])/n
  
  
  # antes era con q o con p daba lo mismo, pero lo hago con q. (1-p)
  zh = dnorm(qnorm(1-ph))
  zk = dnorm(qnorm(1-pk))
  
  # Se
  
  aux = sum(tabla[1,])*sum(tabla[,1])*sum(tabla[,2])*sum(tabla[2,])
  
  se = (1/(zk*zh*sqrt(n)))*sqrt(aux/n**4)
  
  est = rt/se
  
  # Z critico
  zc = qnorm(p = 1-alpha/2)
  
  ## zc = qt(p = 1-alpha/2, df = 1) ????
  
  
  # test
  if(abs(est) > abs(zc)){
    print(paste("Rechazo H0 con un ", (1-alpha)*100, "% de confianza", sep = ""), quote = F)
  } else {
    print(paste("NO rechazo H0 con un ", (1-alpha)*100, "% de confianza", sep = ""), quote = F)
  }
  
  result = c(rt, se, est, zc)
  names(result) = c("Rt", "Sigma e", "Estadistico", "Z.critico")
  
  return(result)
  
  
}


#### tabla de contingencia ####

ctg_test = function(tabla, alpha, totales = FALSE, show.table = F){
  
  if(!is.data.frame(tabla) & !is.matrix(tabla)){
    stop("tabla debe ser un data.frame o una matriz")
  }
  
  if(is.character(alpha) | alpha > 1){
    stop("ALPHA debe estar entre 0 y 1")
  }
  
  
  if(totales){
    tabla = tabla[1:(nrow(tabla)-1), 1:(ncol(tabla)-1)]
  } 
  
  tot_col = apply(tabla, c(2), sum)
  tot_row = apply(tabla, c(1), sum)
  total = sum(tot_row)
  
  #tabla random
  tabla_rand = matrix(nrow = nrow(tabla), ncol = ncol(tabla))
  
  for(r in 1:nrow(tabla)){
    
    tabla_rand[r,] = tot_row[r]*tot_col/total
   
  }
  
  if(show.table){
    print(tabla_rand)
  }
  
  # estadistico chi-sq
  est = sum(((tabla - tabla_rand)**2)/tabla_rand)
  
  # chi-teo
  est_teo = qchisq(p = (1-alpha), df = (nrow(tabla) - 1)*(ncol(tabla) - 1))
  
  
  # coeficiente de contingencia 
  
   c = sqrt(est/(est+total)) 
  
  # test
  if(abs(est) > abs(est_teo)){
    print(paste("Rechazo H0 con un ", (1-alpha)*100, "% de confianza", sep = ""), quote = F)
  } else {
    print(paste("NO rechazo H0 con un ", (1-alpha)*100, "% de confianza", sep = ""), quote = F)
  }
  
  
  result = c(est, est_teo, c)
  names(result) = c("Chi.c", "Chi.teo", "C")
  
  return(result)
  
}



############ FIELDPLOT ############
FieldPlot = function(field, lon, lat, resta = 0, tile = T, contour.fill = F, na.fill
                     , mult.cont = F, mult.cont.step = 1, contorno = F, nivel.cont = 0
                     , color.cont = "black", sig = F, type.sig = "tile", v.sig
                     , alpha.sig = 1, color.sig = "white", size.point = 1, mapa
                     , fill.mapa = F, colorbar = "Spectral", n.colors = 9
                     , cb.h = 10, cb.w = 1, cb.size = 10, h.just = .5, revert.brewer = F
                     , colorbar.pos = "right", escala, label.escala = ""
                     , titulo = "", title.size = 14, x.label = "", y.label = ""
                     , lats.size = 10, letter.size = 12
                     , mostrar = T, save = F, width = 15, height = 20, salida
                     , nombre.fig = "fig", modo = "individual"
                     , breaks.lon = seq(-90, -30, by = 10) #
                     , breaks.lat = seq(-60, 20, by = 10)){
                     
  library(ggplot2)
  library(RColorBrewer)            
  library(metR) 
  require(fields)
  library(maptools)
  # si falla algo:
  #library(maps)
  #require(mapdata)
  #library(mapproj)
  #library(sp)
  
  
  ruta = getwd()
  
  if(!is.data.frame(field)){
    
    data = expand.grid(lon = lon, lat = lat)
    data[,3] = array(field, dim = length(lon)*length(lat)) - resta
    
  }
  
  
  if(modo == "individual"){
    r = 1
  } else if(modo == "estaciones"){
    r = 4
  } else if(modo == "meses"){
    r = 12
  } else {
    stop("modo debe ser indiviudal, estaciones o meses")
  }
  
  
  mapa = tolower(mapa)
  
  if(mapa == "argentina"){
    
    map <- map_data("world", region = c("Argentina", colour = "black"))
    
    breaks.lon = seq(-80, -50, by = 10); limits.lon = c(min(breaks.lon), max(breaks.lon))
    breaks.lat = seq(-60, -20, by = 10); limits.lat = c(min(breaks.lat), max(breaks.lat))
    
    if(tile){ ### solo con mapa "argentina" crea mascara oceano si tile = T
      
      outline.arg <- map("world", regions = "Argentina", 
                         exact = TRUE, plot = F, fill = T)
      
      IDs <- sapply(strsplit(outline.arg$names, ":"), function(x) x[1])
      
      # Lo convierto en un objeto de clase "SpatialPolygons": 
      sp.arg <- map2SpatialPolygons(outline.arg, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
      
      resta.lon = 0
      # Convierto al data.frame "df1" 
      # en un objeto de clase "SpatialPolygons":
      # IMPORTANTE: Debe tener la misma proyección que "sp.arg". 
      sp.df1 <- SpatialPointsDataFrame(coords = data[,c("lon","lat")], data = data,
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))
      
      # Veo qué puntos de "sp.df1" están adentro de "sp.arg": 
      interseccion <- sp.df1[!is.na(over(sp.df1, sp.arg)), ]
      # Convierto mi "interseccion" en un data.frame:
      data <- as.data.frame(interseccion)
      
    }
  } else if(mapa == "sa"){
    
    map <- map_data("world2", regions = c("Brazil", "French Guiana", "Suriname", "Colombia",
                                          "Venezuela","Argentina", "Chile", "Uruguay",
                                           "Bolivia", "Ecuador", "Paraguay", "Peru", "Guyana",
                                          "Panama", "Costa Rica", "Nicaragua", "Martinique",
                                          "falkland islands", "Honduras", "El Salvador",
                                          "Guatemala", "Belice"), colour = "black")
    
   limits.lon = c(min(breaks.lon), max(breaks.lon))
   limits.lat = c(min(breaks.lat), max(breaks.lat))
   resta.lon = 360
   
   
  } else if(mapa == "mundo2"){
    
    map <- map_data("world2", colour = "black")
    resta.lon = 360
    limits.lon = c(min(breaks.lon), max(breaks.lon))
    limits.lat = c(min(breaks.lat), max(breaks.lat))
    
  } else if(mapa == "mundo"){ # <<<<<<<<<<<- preferente
    
    map <- map_data("world", colour = "black")
    limits.lon = c(min(breaks.lon), max(breaks.lon)) 
    limits.lat = c(min(breaks.lat), max(breaks.lat))
    resta.lon = 0
  }
  
  escala.limites = c(min(escala), max(escala))
  
  for(i in 1:r){
    
    g = ggplot(data = data, aes(x = lon + resta.lon, y = lat))
    
    if(revert.brewer == T){ # revertir paleta de colores
      if(colorbar.pos == "right"){ # posicion de barra de colores
        
        if(contour.fill & tile){ # contour.fill arriba del tile. El tile es para completar los bordes
          
          g =  g + geom_tile(aes(fill = V3), na.rm = T) +
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = V3), 
                              alpha = 1, na.fill = na.fill , breaks = escala) +
            scale_fill_stepsn(limits = escala.limites, name = label.escala,
                              colours = rev(brewer.pal(n=n.colors , colorbar))
                              , na.value = "white", breaks = escala,
                              guide = guide_colorbar(barwidth = cb.w
                                                     , barheight = cb.h
                                                     , title.position = "top", title.hjust = 0.5
                                                     , raster = F, ticks = T
                                                     , label.theme = element_text(size = cb.size)))
          
        } else if(contour.fill == F & tile == T){ # solo tile
          
          g =  g + geom_tile(aes(fill = V3), na.rm = T) + 
            scale_fill_stepsn(limits = escala.limites, name = label.escala
                              , colours = rev(brewer.pal(n=n.colors , colorbar))
                              , na.value = "white", breaks = escala
                              , guide = guide_colorbar(barwidth = cb.w
                                                       , barheight = cb.h
                                                       , title.position = "top"
                                                       , title.hjust = 0.5
                                                       , raster = F, ticks = T
                                                       , label.theme = element_text(size = cb.size)))
          
        } else if(contour.fill == T & tile == F){ # solo contour.fill
          
          g =  g +
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = V3),alpha = 1
                              , na.fill = na.fill , breaks = escala) +
            scale_fill_stepsn(limits = escala.limites, name = label.escala,
                              colours = rev(brewer.pal(n=n.colors , colorbar))
                              , na.value = "white", breaks = escala,
                              guide = guide_colorbar(barwidth = cb.w
                                                     , barheight = cb.h
                                                     , title.position = "top"
                                                     , title.hjust = 0.5
                                                     , raster = F, ticks = T
                                                     , label.theme = element_text(size = cb.size)))
        }
      } else { # sin revertir paleta de colores
        
        if(contour.fill & tile){
          
          g =  g + geom_tile(aes(fill = V3), na.rm = T) +
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = V3),
                               alpha = 1, na.fill = na.fill , breaks = escala) +
            scale_fill_stepsn(limits = escala.limites, name = label.escala
                              , colours = rev(brewer.pal(n=n.colors , colorbar))
                              , na.value = "white", breaks = escala,
                              guide = guide_colorbar(barwidth = cb.h
                                                     , barheight = cb.w
                                                     , title.position = "top"
                                                     , title.hjust = 0.5
                                                     , raster = F, ticks = T
                                                     , label.theme = element_text(size = cb.size)))
          
        } else if(contour.fill == F & tile == T){
          
          g =  g + geom_tile(aes(fill = V3), na.rm = T) +
            scale_fill_stepsn(limits = escala.limites, name = label.escala
                              , colours = rev(brewer.pal(n=n.colors , colorbar))
                              , na.value = "white", breaks = escala
                              , guide = guide_colorbar(barwidth = cb.h
                                                       , barheight = cb.w
                                                       , title.position = "top"
                                                       , title.hjust = 0.5
                                                       , raster = F, ticks = T
                                                       , label.theme = element_text(size = cb.size)))
          
        } else if(contour.fill == T & tile == F){
          
          g =  g +
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = V3)
                              ,alpha = 1, na.fill = na.fill , breaks = escala) +
            scale_fill_stepsn(limits = escala.limites, name = label.escala
                              , colours = rev(brewer.pal(n=n.colors , colorbar))
                              , na.value = "white", breaks = escala,
                              guide = guide_colorbar(barwidth = cb.h
                                                     , barheight = cb.w
                                                     , title.position = "top"
                                                     , title.hjust = 0.5
                                                     , raster = F, ticks = T
                                                     , label.theme = element_text(size = cb.size)))
        }
      }
    } else {
      
      if(colorbar.pos == "bottom"){
        
        if(contour.fill & tile){
          
          g =  g + geom_tile(aes(fill = V3), na.rm = T) +
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = V3)
                              ,alpha = 1, na.fill = na.fill , breaks = escala) +
            scale_fill_stepsn(limits = escala.limites, name = label.escala
                              , colours = brewer.pal(n=n.colors , colorbar)
                              , na.value = "white", breaks = escala,
                              guide = guide_colorbar(barwidth = cb.h
                                                     , barheight = cb.w
                                                     , title.position = "top"
                                                     , title.hjust = 0.5
                                                     , raster = F, ticks = T
                                                     , label.theme = element_text(size = cb.size)))
          
        } else if(contour.fill == F & tile == T){
          
          g =  g + geom_tile(aes(fill = V3), na.rm = T) + 
            scale_fill_stepsn(limits = escala.limites, name = label.escala
                              , colours = brewer.pal(n=n.colors , colorbar)
                              , na.value = "white", breaks = escala
                              , guide = guide_colorbar(barwidth = cb.h
                                                       , barheight = cb.w
                                                       , title.position = "top"
                                                       , title.hjust = 0.5
                                                       , raster = F, ticks = T
                                                       , label.theme = element_text(size = cb.size)))
          
        } else if(contour.fill == T & tile == F){
          
          g =  g +
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = V3)
                              ,alpha = 1, na.fill = na.fill , breaks = escala) +
            scale_fill_stepsn(limits = escala.limites, name = label.escala
                              , colours = brewer.pal(n=n.colors , colorbar)
                              , na.value = "white", breaks = escala,
                              guide = guide_colorbar(barwidth = cb.h
                                                     , barheight = cb.w
                                                     , title.position = "top"
                                                     , title.hjust = 0.5
                                                     , raster = F, ticks = T
                                                     , label.theme = element_text(size = cb.size)))
        }
      } else{
        
        if(contour.fill & tile){
          
          g =  g + geom_tile(aes(fill = V3), na.rm = T) +
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = V3)
                              ,alpha = 1, na.fill = na.fill , breaks = escala) +
            scale_fill_stepsn(limits = escala.limites, name = label.escala
                              , colours = brewer.pal(n=n.colors , colorbar)
                              , na.value = "white", breaks = escala,
                              guide = guide_colorbar(barwidth = cb.w
                                                     , barheight = cb.h
                                                     , title.position = "top"
                                                     , title.hjust = 0.5
                                                     , raster = F, ticks = T
                                                     , label.theme = element_text(size = cb.size)))
          
        } else if(contour.fill == F & tile == T){
          
          g =  g + geom_tile(aes(fill = V3), na.rm = T) +
            scale_fill_stepsn(limits = escala.limites, name = label.escala
                              , colours = brewer.pal(n=n.colors , colorbar)
                              , na.value = "white", breaks = escala
                              , guide = guide_colorbar(barwidth = cb.w
                                                       , barheight = cb.h
                                                       , title.position = "top"
                                                       , title.hjust = 0.5
                                                       , raster = F, ticks = T
                                                       , label.theme = element_text(size = cb.size)))
          
        } else if(contour.fill == T & tile == F){
          
          g =  g +
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = V3)
                              ,alpha = 1, na.fill = na.fill , breaks = escala) +
            scale_fill_stepsn(limits = escala.limites, name = label.escala
                              , colours = brewer.pal(n=n.colors , colorbar)
                              , na.value = "white", breaks = escala,
                              guide = guide_colorbar(barwidth = cb.w
                                                     , barheight = cb.h
                                                     , title.position = "top"
                                                     , title.hjust = 0.5
                                                     , raster = F, ticks = T
                                                     , label.theme = element_text(size = cb.size)))
        }
      }
    }
    
    
    
    
    
    
    # mascara de significancia
    # aun faltaria el caso de agregar otros datos que indiquen la sig
    if(sig == T){
      if(type.sig == "tile"){
        
        field.sig = field
        
        field.sig[which(abs(field.sig) < v.sig)] = NA
        
        data2 = expand.grid(lon = lon, lat = lat)
        data2[,3] = array(field.sig, dim = length(lon)*length(lat)) 
        colnames(data2)<-c("lon", "lat", "var")
        
        g = g +  geom_tile(data = subset(data2, is.na(var)), 
                           aes(x = lon + resta.lon, y = lat, fill = is.na(var))
                           , alpha = alpha.sig, fill = color.sig, show.legend = F)
        
        
      } else if(type.sig == "point2"){
        
        
        g = g + stat_subset(data = data, aes(x = lon + resta.lon, y = lat
                                             , z = V3, subset = V3 <= v.sig)
                            , size = size.point, color = color.sig
                            , alpha = alpha.sig, geom = "point")
        
      } else {
        
        g = g + stat_subset(data = data, aes(x = lon + resta.lon, y = lat
                                             , z = V3, subset = V3 > v.sig)
                            , size = size.point, color = color.sig
                            , alpha = alpha.sig, geom = "point")       
      }
      
    } 
    
    
    
    # agregando mapa
    if(fill.mapa == T){
      g = g + geom_polygon(data = map, aes(x = long , y = lat, group = group)
                           , fill = "black", color = color.mapa, alpha = 0.3) 
    } else {
      g = g + geom_polygon(data = map, aes(x = long ,y = lat, group = group)
                           , fill = NA, color = "black") 
    }
    
    
    # contornos 
    if(mult.cont){
      
      r.lim = round(max(data$V3, na.rm = T), 1)
      
      g = g +  geom_contour(data = data, aes(x = lon + resta.lon, y = lat, z = V3)
                            , breaks = seq(-r.lim, r.lim, by = mult.cont.step),
                            color = "black", size = 0.2) +
        geom_text_contour(aes(z = V3), stroke = 0)
      
    } 
    
    # un solo contor marcando un solo valor
    
    if(contorno){
      
      g = g + stat_contour(data = data, aes(x = lon + resta.lon, y = lat, z = V3)
                           , color = color.cont, size = 1, breaks = nivel.cont, show.legend = T) 
      
      
    }
    
    # theme..
    g = g + theme_minimal() + ggtitle(titulo) +
      
      scale_x_longitude(breaks = breaks.lon, name = x.label, limits = limits.lon)+
      scale_y_latitude(breaks = breaks.lat, name = y.label, limits = limits.lat)+
      theme(axis.text.y   = element_text(size = lats.size)
            , axis.text.x   = element_text(size = lats.size)
            , axis.title.y  = element_text(size = title.size),
            axis.title.x  = element_text(size = lats.size)
            , panel.grid.minor = element_blank()
            , axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
            panel.ontop = TRUE,
            plot.title = element_text(hjust = h.just, size = letter.size)) + 
      geom_hline(yintercept = 0, color = "black")
    
    
    
    if(colorbar.pos == "bottom"){
      g = g + theme(legend.position = "bottom")
    }
    
    if(save){ # guardar?
      ggsave(paste(ruta, salida, nombre.fig, ".jpg", sep = "")
             , plot = g, width = width, height = height, units = "cm")
      
      print(paste(ruta, salida, nombre.fig, ".jpg", sep = ""))
    }
    
    if(mostrar){ #mostrar ?
      return(g)
    }
    
    ###
  }
}




#### corr simple ####
corr_simple = function(matriz, serie, cf){
  
  # corr una matriz en modo S vs una serie 
  
  a = 1 - cf
  
  corr = cor(matriz,serie)
  
  
  rc = qnorm(1 - a/2)/sqrt(length(serie)-2)
  # qt(p = 0.95,df = length(serie)-2)/sqrt((length(serie)-1)+qt(p = 0.95,df = length(serie)-2))
  
  corr.sig = corr
  corr.sig[which(corr < rc)] = NA
  corr.sig[which(!is.na(corr.sig))] = 1
  
  result = list()
  result[[1]] = corr; result[[2]] = rc; result[[3]] = corr.sig
  return(result)
}


#### Lund ####
Lund = function(data, rc){
  
  aux = cor(data) # matriz de correlacion
  diag(aux) = NA # saco los valores de la diagonal
  
  aux[which(abs(aux)<rc)] = NA  # me quedo solo con los valores mayores al rc elegido
  print("#############################################################")
  print("Matriz de correlación donde r supera el Rc")
  print(aux)
  # tabla donde se guardaran los datos
  # fila 1. variable TIPO, columnas: GRUPÒ
  # resto de filas -{1}: integrantes del grupo 
  # todo guardado como numero de fila (tiempos, en modo T)
  tabla = matrix(data = NA, nrow = ncol(aux), ncol = ncol(aux)) 
  
  # si en la matriz de corr hay datos que no son NA, i.e, mayores o iguales a rc repite lo siguiente
  t = 1 # cantidad de veces que se realiza la motodologia
  while(length(which(!is.na(aux))) >= 1){ 
    
    # cantidad de casos significativos (r > rc) por columna
    l = vector()
    for(i in 1:ncol(aux)){
      
      l[i] = length(which(!is.na(aux[,i])))
      
    }
    
    t.x = which(l == max(l)) # la/s columnas con mayor cantidad de casos significativos
    
    # criterio de eleccion de la practica (el tiempo mas nuevo/antiguo.... el de mas a la derecha)
    t.x = max(t.x) # DEFINE TIPO
   
    g.x = which(!is.na(aux[,t.x])) # DEFINE GRUPO
    print("#############################################################")
    print(paste("Variable Tipo", t, ":", t.x))
    print(paste("Grupo:", t))
    print(g.x) # integrantes del grupo 1
    print("#############################################################")
    tabla[1,t] = t.x 
    tabla[2:(length(g.x)+1),t] = g.x
    
    #mattriz de corr. residual
    # reemplaza con "NA" todos los integrantes del grupo1
    aux[c(g.x,t.x),] = NA
    aux[,c(g.x,t.x)] = NA
    print("#############################################################")
    print("Matriz Residual")
    print(aux)
    
    t = t + 1
  }
  print(t-1)
  return(tabla)
}



#### Componentes ppales adaptado a funcion ####

ACP = function(data, save = F){
  
  X<-data
  
  # dimenciones de la matriz
  dimension<-dim(X)
  m<-dimension[1]
  n<-dimension[2]
  
  #scale resta la media a cda columna y divide por el sd
  Xs <- scale(X, center = TRUE, scale = TRUE) 
  
  
  #'Descompone en valores singulares', autovalores, autovectores, etc
  descom<-svd(Xs)
  
  P<-descom$u 
  d<-descom$d
  Q<-descom$v
  
  # D autovalores
  # transformo a d a matriz Diagonal para poder multiplicar
  sigma=diag(d)
  D<-(t(sigma) %*% sigma)/(m-1)  # "%*%" es el comando para producto de matrices
  
  #'las CP Zs
  Zs<-Xs %*% Q %*% (diag(diag(D) ^ (-0.5)))
  
  #('autovectores (Fs) son:')
  Fs <- Q%*%(diag(diag(D)^(0.5))) ### NOOOO!!! F --> Fs
  # Si se asigna algo F, F deja de poder ser usado com FALSE!!!
  
  #('Varianza explicada % (VECP) por cada componente')
  Diag<-diag(D)
  SumaDiag<-sum(Diag)
  VECP<-(Diag/SumaDiag)*100
  
  
  results = list()
  results[[1]] = round(diag(D), 2) # autovalores
  results[[2]] = round(Zs, 2) # componentes
  results[[3]] = round(Fs, 2) # autovectores
  results[[4]] = round(VECP, 2) # % varianza
  
  
  
  if(save){ # guardar?
    
    print('archivo de autovalores')
    autovalores<-write.table(diag(round(D,2)), 'autovalores.txt')
    
    print('archivo de Zs (CP)')
    componentes<-write.table(round(Zs,2), 'componentes.txt')
    
    print('archivo de autovectores (F)')
    autovectores<-write.table(round(Fs,2), 'autovectores.txt')
    
    print('archivo de la varianza explicada por cada componente')
    Varexplicada<-write.table(round(VECP,2), 'vecp.txt',col.names='VECP')
    
  }
  
  
  return(results)
  
  
}


############## graficar CPs ##############

PlotCP = function(cp, datos.obs, sig = T, type.sig = "scree", mapa = "mundo"
                  , escala, r.brewer = F, c.fill = T, colorbar = "Spectral"
                  , escala.obs, colorbar.obs, r.brewer.obs = F
                  , cp.draw = 1, cont = T, step.obs, step.cp
                  , salida = getwd(), nombre = "CP_fig"
                  , breaks.lon = seq(-90, -30, by = 10) 
                  , breaks.lat = seq(-60, 20, by = 10)
                  , step = 1, tile = T, x.lim = NULL, step.a = NULL
                  , width = 30, height = 25.5){
  
  # cp, salida de ACP
  # datos.obs, matriz de datos entrada en ACP con LONGITUDES Y LATITUDES
  # sig, significancia
  # type.sig, tipo.. scree, lev, kaiser o %, solo grafico con scree y lev
  # mapa, para la funcion FieldPlot(), definida antes
  # escala, escala del mapa de cps
  # r.brewer, revertir escala
  # c.fill, contour.fill de metR --> mas consumo de procesador
  # colorbar...
  # cp.draw, numero de cp que se va graficar, determina tambien el autovector q se grafica
  # cont, contornos ---> mas consumo de procesador
  # step.obs, intervalos de contornos en el mapa observado
  # step.cp, intervalos de contornos
  # step, intervalos en el grafico de autovector
  # step.a, intervalos en el scree o lev
  # salida, salida donde se guarda el grafico
  # nombre, ombre con el que se guarda
  # breaks.lon y lat, seteados para sudamerica, modificar en caso de graficar otra parte
  # tile, tile
  # x.lim, limite del grafico scree o lev
  # width y height, ancho y alto de la figura a guardar, en cm.
  
  # EJEMPLO
  # PlotCP(cp = cps, datos.obs = datos, type.sig = "scree"
  #        , mapa = "mundo", escala = seq(-.8,.8, by = .2)
  #        , colorbar = "RdBu", escala.obs = seq(-.8,.8, by = .2), r.brewer = T
  #        , cp.draw = i, salida = ruta_salidas, nombre = paste("CP", i, sep = "")
  #        , colorbar.obs = "RdBu", r.brewer.obs = T, c.fill = F, cont = T
  #        , step.obs = .4, step = 20, step.a = 1, step.cp = .4
  #        , tile = T, x.lim = 10
  #        , breaks.lon = seq(-80, 30 , by = 10)
  #        , breaks.lat = seq(-55, -20, by = 10)
  #        , width = 35)

  
  library(ggplot2)
  library(gridExtra)
  library(akima)
  source("funciones.R")
  
  type.sig = tolower(type.sig)
  mapa = tolower(mapa)
  result = NULL
  
  if(is.null(x.lim)){
    x.lim = ncol(cp[[2]])
  }
  
  if(is.null(step.a)){
    step.a = step
  }
  
  
  
  while(sig & is.null(type.sig)){
    type.sig = readline("type.sig = : scree, lev, % o kaiser")
  }
  
  if(sig){
    
    if(type.sig == "scree"){
      
      aux = data.frame(landa = as.numeric(cp[[1]]), cp = 1:ncol(cp[[2]]))
      
      gg = ggplot(data = aux, aes(x = cp)) + 
        geom_line(aes(y = landa), color = "firebrick", size = 1) + 
        scale_x_continuous(limits = c(min(aux[,2]),x.lim),
                           breaks = seq(min(aux[,2]),max(aux[,2]), by = step.a))+
        ggtitle("Scree Plot") + 
        ylab(expression(lambda)) + xlab("CPs") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 14)
              , axis.text.x = element_text(size = 14), 
              axis.title.y  = element_text(size = 14),
              axis.title.x  = element_text(size = 14),
              panel.border = element_rect(colour = "black", fill = NA, size = 1),
              panel.ontop = T,
              plot.title = element_text(hjust = 0.5)) +
        theme(legend.position = "bottom", legend.text = element_text(size = 12))
      
      
    } else if(type.sig == "kaiser"){
      
      aux = data.frame(landa = as.numeric(cp[[1]]), cp = 1:ncol(cp[[2]]))
      
      aux = aux[which(aux[,1]>=1),]
      result = aux
      
    } else if(type.sig == "lev"){ ## ver
      
      aux = data.frame(landa = as.numeric(cp[[1]]), cp = 1:ncol(cp[[2]]))
      
      gg = ggplot(data = aux, aes(x = cp)) + 
        geom_line(aes(y = log(landa)), color = "firebrick", size = 1) + 
        scale_x_continuous(limits = c(min(aux[,2]),x.lim)
                           , breaks = seq(min(aux[,2]),max(aux[,2]), by = step.a))+
        ggtitle("Diagrama de Lev") + 
        ylab(paste("log(",expression(lambda), ")", sep = "")) + xlab("CPs") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 14), 
              axis.text.x  = element_text(size = 14), 
              axis.title.y  = element_text(size = 14),
              axis.title.x  = element_text(size = 14),
              panel.border = element_rect(colour = "black", fill = NA, size = 1),
              panel.ontop = T,
              plot.title = element_text(hjust = 0.5)) + 
        theme(legend.position = "bottom", legend.text = element_text(size = 12))
      
      
    } else if(type.sig == "%")
      aux = cp[[1]]
    
    aux = data.frame(landa = as.numeric(cp[[1]]), cp = 1:ncol(cp[[2]]), Fs = cp[[4]])
    aux = aux[which(aux[,3] >= 70),]
    
    
    result = aux
    
  }
  
  # mapa cp.
  datos = cbind(datos.obs[,c(1,2)], cp[[2]])
  df = interp(x = datos[,1],
              y = datos[,2],
              z = datos[,cp.draw+2], extrap = FALSE)
  
  if(mapa == "sa"){
    df[[1]] = df[[1]] + 360
  }
  
  
  mapa.cp = FieldPlot(field = df[[3]], lon = df[[1]], lat = df[[2]], mapa = mapa
                      , escala = escala, revert.brewer = r.brewer
                      , contour.fill = c.fill, na.fill = -100000, mult.cont = cont
                      , save = F, colorbar = colorbar
                      , titulo = paste("CP ", cp.draw, "  -  ", cp[[4]][cp.draw]
                                       , "% de varianza explicada", sep = "")
                      , height = 10, width = 10, mostrar = T
                      , cb.h = 30, mult.cont.step = step.cp,
                      breaks.lon = breaks.lon 
                      , breaks.lat = breaks.lat, tile = tile)
  
  
  
  # mapa obs
  
  tiempo = which(abs(cp[[3]])[,cp.draw] == max(abs(cp[[3]])[,cp.draw], na.rm = T))
  
  df = interp(x = datos.obs[,1],
              y = datos.obs[,2],
              z = datos.obs[,tiempo + 2], extrap = FALSE) ## ojo aca si el archivo es disinto.ç
  if(mapa == "sa"){
    df[[1]] = df[[1]] + 360
  }
  
  mapa.obs = FieldPlot(field = df[[3]], lon = df[[1]], lat = df[[2]], mapa = mapa
                       , escala = escala.obs, revert.brewer = r.brewer.obs
                       , contour.fill = c.fill, na.fill = -100000, mult.cont = cont
                       , save = F, colorbar = colorbar.obs
                       , titulo = paste("Máx. correlación en tiempo", tiempo), mostrar = T
                       , cb.h = 30, mult.cont.step = step.obs,
                       breaks.lon = breaks.lon # en el caso de SA puede ser necesario modificarlos
                       , breaks.lat = breaks.lat, tile = tile)
  
  
  # autovectores
  
  aux = data.frame(a.v = cp[[3]][,cp.draw], t = 1:length(cp[[3]][,cp.draw]))
  
  
  a.vector = ggplot(data = aux, aes(x = t)) + 
    geom_line(aes(y = a.v), color = "dodgerblue", size = 1) + 
    geom_hline(yintercept = 0, color = "black")+
    geom_vline(xintercept = tiempo, color = "red") +
    scale_x_continuous(limits = c(min(aux[,2]),max(aux[,2]))
                       , breaks = seq(min(aux[,2]),max(aux[,2]), by = step)) +
    ggtitle(paste("Autovector CP", cp.draw, sep = "")) + 
    ylab("") + xlab("tiempo") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 14)
          , axis.text.x   = element_text(size = 14), 
          axis.title.y  = element_text(size = 14),
          axis.title.x  = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "bottom", legend.text = element_text(size = 12))
  
  
  # los 4 o 3 graficos
  
  if(type.sig == "scree" | type.sig == "lev"){ # 4 graficos
    
    gp1 = mapa.cp
    gp2 = mapa.obs
    gp3 = gg
    gp4 = a.vector
    
    gpls <- lapply(list(gp1,gp2,gp3, gp4), ggplotGrob )
    
    lay <- rbind(c(1,1,2,2),c(1,1,2,2))
    
    # de esta forma tan elegante no afecta los margenes  
    
    p1 = grid.arrange(gpls[[1]], gpls[[2]],             
                      layout_matrix = lay) 
    
    p2 = grid.arrange(gpls[[3]], gpls[[4]],             
                      layout_matrix = lay) 
    lay <- rbind(c(1,1,1,1,1,1,1,1),c(1,1,1,1,1,1,1,1),
                 c(1,1,1,1,1,1,1,1),c(1,1,1,1,1,1,1,1),
                 c(2,2,2,2,2,2,2,2),c(2,2,2,2,2,2,2,2))
    
    
    nombre_fig = paste(salida,"/", nombre, ".jpg", sep = "")
    
    ggsave(nombre_fig, plot = grid.arrange(p1, p2, ncol = 2, layout_matrix = lay),
           width = width, height = height ,units = "cm")
    
  } else {
    
    
    gp1 = mapa.cp
    gp2 = mapa.obs
    gp4 = a.vector
    
    gpls <- lapply(list(gp1,gp2, gp4), ggplotGrob )
    
    lay <- rbind(c(1,1,2,2),c(1,1,2,2))
    
    # de esta forma tan elegante no afecta los margenes  
    
    p1 = grid.arrange(gpls[[1]], gpls[[2]],             
                      layout_matrix = lay) 
    lay <- rbind(c(1,1,1,1),c(1,1,1,1))
    p2 = grid.arrange(gpls[[3]],             
                      layout_matrix = lay) 
    lay <- rbind(c(1,1,1,1,1,1,1,1),c(1,1,1,1,1,1,1,1),
                 c(1,1,1,1,1,1,1,1),c(1,1,1,1,1,1,1,1),
                 c(2,2,2,2,2,2,2,2),c(2,2,2,2,2,2,2,2))
    
    
    nombre_fig = paste(salida,"/", nombre, ".jpg", sep = "")
    
    ggsave(nombre_fig,plot =grid.arrange(p1, p2, ncol = 2, layout_matrix = lay),
           width = width, height = height ,units = "cm")
    
    
    return(result)
    
  }
}





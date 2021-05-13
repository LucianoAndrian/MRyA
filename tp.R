# TP
ruta_datos = "/home/auri/Facultad/Doc/Materias/MRyA/Practicas/TP/Grupo1/"
system(command = "mkdir TP/Salidas")
ruta_salidas = "/home/auri/Facultad/Doc/Materias/MRyA/Practicas/TP/Salidas/"

datos = as.data.frame(read.table(paste(ruta_datos, "Datos_SST.txt", sep = ""), sep = ";", header = T))

source("funciones.R")

# Anomalias 
aux = datos[,-c(1,2)]
anom = aux - rowMeans(aux, na.rm = T)

# cps
source("funciones.R")

cps = ACP(data = datos2, save = F)

anom.coord = cbind(datos[,c(1,2)], anom)

### Graficos ##

library(ggplot2)
library(RColorBrewer)            
library(metR) 
require(fields)
library(maptools)

# CPs

# Mapa
breaks.lon = seq(125, 300 , by = 25)
breaks.lat = seq(-30, 30, by = 10)
map <- map_data("world2", colour = "black")
limits.lon = c(min(breaks.lon), max(breaks.lon)) 
limits.lat = c(min(breaks.lat), max(breaks.lat))
#resta.lon = 0

# escala 
escala = seq(-5, 5, by = 1)
escala.limites = c(min(escala), max(escala))

aux = cbind(anom.obs[,c(1,2)], cps[[2]])


for(cp.draw in 1:4){
 
  df = interp(x = aux[,1],
              y = aux[,2],
              z = aux[,cp.draw+2], extrap = FALSE)
  
  
  lon = df[[1]]
  lat = df[[2]]
  field = df[[3]]
  
  data = expand.grid(lon = lon, lat = lat)
  data[,3] = array(field, dim = length(lon)*length(lat))
  
  g[[cp.draw]] = ggplot(data = data, aes(x = lon, y = lat)) + 
    geom_tile(aes(fill = V3), na.rm = T) +
    geom_contour_fill(data = data, aes(x = lon, y = lat, z = V3), 
                      alpha = 1, na.fill = T , breaks = escala) +
    
    # escala y paleta de colores
    scale_fill_stepsn(limits = escala.limites, name = "",
                      colours = rev(brewer.pal(n=11 , "RdBu"))
                      , na.value = "white", breaks = escala,
                      guide = guide_colorbar(barwidth = 30
                                             , barheight = 1
                                             , title.position = "top"
                                             , title.hjust = 0.5
                                             , raster = F, ticks = T
                                             , label.theme = element_text(size =  10))) +

  
    
    #mapa
    geom_polygon(data = map, aes(x = long ,y = lat, group = group)
                 , fill = NA, color = "black") + 
    
    # contornos
    geom_contour(data = data, aes(x = lon , y = lat, z = V3)
                 , breaks = escala
                 , color = "black", size = 0.2) +
    geom_text_contour(aes(z = V3), stroke = 0
                      ,breaks = escala ) +
    
    # coords.
    coord_fixed(xlim = limits.lon, ylim = limits.lat) +
    scale_x_continuous(name = "Longitud", breaks = breaks.lon) +
    scale_y_continuous(name = "Latitud", breaks = breaks.lat) +
    
    theme_minimal() + ggtitle(paste("Componente Principal", cp.draw)) +
    
  

    theme(axis.text.y   = element_text(size = 14)
          , axis.text.x   = element_text(size = 14)
          , axis.title.y  = element_text(size = 14),
          axis.title.x  = element_text(size = 14)
          , panel.grid.minor = element_blank()
          , axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          panel.ontop = TRUE,
          plot.title = element_text(hjust = 0.5, size = 20)) + 
    theme(legend.position = "bottom")
    
  
    ggsave(paste(ruta_salidas,"CP_", cp.draw, ".jpg", sep = "") 
           , plot = g[[cp.draw]], width = 20, height = 13, units = "cm")
  
   
}


# los 4 graficos
 ####### 
g[[1]] = ggplot(data = data, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = V3), na.rm = T) +
  geom_contour_fill(data = data, aes(x = lon, y = lat, z = V3), 
                    alpha = 1, na.fill = T , breaks = escala) +
  
  # escala y paleta de colores
  scale_fill_stepsn(limits = escala.limites, name = "",
                    colours = rev(brewer.pal(n=11 , "RdBu"))
                    , na.value = "white", breaks = escala,
                    guide = guide_colorbar(barwidth = 1
                                           , barheight = 30
                                           , title.position = "top"
                                           , title.hjust = 0.5
                                           , raster = F, ticks = T
                                           , label.theme = element_text(size =  10))) +
  
  
  
  #mapa
  geom_polygon(data = map, aes(x = long ,y = lat, group = group)
               , fill = NA, color = "black") + 
  
  # contornos
  geom_contour(data = data, aes(x = lon , y = lat, z = V3)
               , breaks = escala
               , color = "black", size = 0.2) +
  geom_text_contour(aes(z = V3), stroke = 0
                    ,breaks = escala ) +
  
  # coords.
  coord_fixed(xlim = limits.lon, ylim = limits.lat) +
  scale_x_continuous(name = "Longitud", breaks = breaks.lon) +
  scale_y_continuous(name = "Latitud", breaks = breaks.lat) +
  
  theme_minimal() + ggtitle(paste("Componente Principal", cp.draw)) +
  
  
  
  theme(axis.text.y   = element_text(size = 14)
        , axis.text.x   = element_text(size = 14)
        , axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14)
        , panel.grid.minor = element_blank()
        , axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        panel.ontop = TRUE,
        plot.title = element_text(hjust = 0.5, size = 20))
######
g_legend = function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)}

colorbar = g_legend(g[[1]])



gp1 = g[[1]] + theme(legend.position = "none", plot.margin = unit(c(.2,.2,.2,.2), "lines")
                     , plot.title = element_text(hjust = 0.5, size = 15)
                     , axis.text.y   = element_text(size = 10)
                     , axis.text.x   = element_text(size = 10)
                     , axis.title.y  = element_text(size = 10)
                     , axis.title.x  = element_text(size = 10))
gp2 = g[[2]] + theme(legend.position = "none", plot.margin = unit(c(.2,.2,.2,.2), "lines")
                     , plot.title = element_text(hjust = 0.5, size = 15)
                     , axis.text.y   = element_text(size = 10)
                     , axis.text.x   = element_text(size = 10)
                     , axis.title.y  = element_text(size = 10)
                     , axis.title.x  = element_text(size = 10))
gp3 = g[[3]] + theme(legend.position = "none", plot.margin = unit(c(.2,.2,.2,.2), "lines")
                     , plot.title = element_text(hjust = 0.5, size = 15)
                     , axis.text.y   = element_text(size = 10)
                     , axis.text.x   = element_text(size = 10)
                     , axis.title.y  = element_text(size = 10)
                     , axis.title.x  = element_text(size = 10))
gp4 = g[[4]] + theme(legend.position = "none", plot.margin = unit(c(.2,.2,.2,.2), "lines")
                     , plot.title = element_text(hjust = 0.5, size = 15)
                     , axis.text.y   = element_text(size = 10)
                     , axis.text.x   = element_text(size = 10)
                     , axis.title.y  = element_text(size = 10)
                     , axis.title.x  = element_text(size = 10))


gpls <- lapply(list(gp1,gp2,gp3, gp4), ggplotGrob )

lay <- rbind(c(1,1,1,1,1,1,1,1), c(1,1,1,1,1,1,1,1),
             c(2,2,2,2,2,2,2,2), c(2,2,2,2,2,2,2,2),
             c(3,3,3,3,3,3,3,3), c(3,3,3,3,3,3,3,3),
             c(4,4,4,4,4,4,4,4), c(4,4,4,4,4,4,4,4))

p1 = grid.arrange(gpls[[1]], gpls[[2]], gpls[[3]], gpls[[4]],             
                  layout_matrix = lay
                  ) 



lay <- rbind(c(1,1,1,1,1,1,1,1,2),c(1,1,1,1,1,1,1,1,2))


ggsave(paste(ruta_salidas,"CPs_1-4", ".jpg", sep = "") ,plot =grid.arrange(p1, ncol = 2, layout_matrix = lay, colorbar),
       width = 20, height = 25 ,units = "cm")
  




### CAMPOS observados el dia de maxima corr para cada cp

# parecido a lo anterior

escala = seq(-2.5, 2.5, by = 0.5)
escala.limites = c(min(escala), max(escala))

for(cp.draw in 1:4){
  
tiempo = which(abs(cps[[3]])[,cp.draw] == max(abs(cps[[3]])[,cp.draw], na.rm = T))
anios = gsub(x = x[-c(1,2)], pattern = "X*", replacement = "")


df = interp(x = anom.coord[,1],
            y = anom.coord[,2],
            z = anom.coord[,tiempo + 2], extrap = FALSE) 



lon = df[[1]]
lat = df[[2]]
field = df[[3]]

data = expand.grid(lon = lon, lat = lat)
data[,3] = array(field, dim = length(lon)*length(lat))

g[[cp.draw]] = ggplot(data = data, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = V3), na.rm = T) +
  geom_contour_fill(data = data, aes(x = lon, y = lat, z = V3), 
                    alpha = 1, na.fill = T , breaks = escala) +
  
  # escala y paleta de colores
  scale_fill_stepsn(limits = escala.limites, name = "ºC",
                    colours = rev(brewer.pal(n=11 , "RdBu"))
                    , na.value = "white", breaks = escala,
                    guide = guide_colorbar(barwidth = 30
                                           , barheight = 1
                                           , title.position = "top"
                                           , title.hjust = 0.5
                                           , raster = F, ticks = T
                                           , label.theme = element_text(size =  10))) +
  
  
  
  #mapa
  geom_polygon(data = map, aes(x = long ,y = lat, group = group)
               , fill = NA, color = "black") + 
  
  # contornos
  geom_contour(data = data, aes(x = lon , y = lat, z = V3)
               , breaks = escala
               , color = "black", size = 0.2) +
  geom_text_contour(aes(z = V3), stroke = 0
                    ,breaks = escala ) +
  
  # coords.
  coord_fixed(xlim = limits.lon, ylim = limits.lat) +
  scale_x_continuous(name = "Longitud", breaks = breaks.lon) +
  scale_y_continuous(name = "Latitud", breaks = breaks.lat) +
  
  theme_minimal() + ggtitle(paste("Diciembre", anios[tiempo], "- Max. Correlación con CP", cp.draw)) +
  
  theme(axis.text.y   = element_text(size = 14)
        , axis.text.x   = element_text(size = 14)
        , axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14)
        , panel.grid.minor = element_blank()
        , axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        panel.ontop = TRUE,
        plot.title = element_text(hjust = 0.5, size = 20)) + 
  theme(legend.position = "bottom")


ggsave(paste(ruta_salidas,"MaxCor", cp.draw, ".jpg", sep = "") 
       , plot = g[[cp.draw]], width = 20, height = 13, units = "cm")

}


########
g[[cp.draw]] = ggplot(data = data, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = V3), na.rm = T) +
  geom_contour_fill(data = data, aes(x = lon, y = lat, z = V3), 
                    alpha = 1, na.fill = T , breaks = escala) +
  
  # escala y paleta de colores
  scale_fill_stepsn(limits = escala.limites, name = "ºC",
                    colours = rev(brewer.pal(n=11 , "RdBu"))
                    , na.value = "white", breaks = escala,
                    guide = guide_colorbar(barwidth = 1
                                           , barheight = 30
                                           , title.position = "top"
                                           , title.hjust = 0.5
                                           , raster = F, ticks = T
                                           , label.theme = element_text(size =  10))) +
  
  
  
  #mapa
  geom_polygon(data = map, aes(x = long ,y = lat, group = group)
               , fill = NA, color = "black") + 
  
  # contornos
  geom_contour(data = data, aes(x = lon , y = lat, z = V3)
               , breaks = escala
               , color = "black", size = 0.2) +
  geom_text_contour(aes(z = V3), stroke = 0
                    ,breaks = escala ) +
  
  # coords.
  coord_fixed(xlim = limits.lon, ylim = limits.lat) +
  scale_x_continuous(name = "Longitud", breaks = breaks.lon) +
  scale_y_continuous(name = "Latitud", breaks = breaks.lat) +
  
  theme_minimal() + ggtitle(paste("Diciembre", anios[tiempo], "- Max. Correlación con CP", cp.draw)) +
  
  theme(axis.text.y   = element_text(size = 14)
        , axis.text.x   = element_text(size = 14)
        , axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14)
        , panel.grid.minor = element_blank()
        , axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        panel.ontop = TRUE,
        plot.title = element_text(hjust = 0.5, size = 20))
#######

g_legend = function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)}

colorbar = g_legend(g[[cp.draw]])



gp1 = g[[1]] + theme(legend.position = "none", plot.margin = unit(c(.2,.2,.2,.2), "lines")
                     , plot.title = element_text(hjust = 0.5, size = 15)
                     , axis.text.y   = element_text(size = 10)
                     , axis.text.x   = element_text(size = 10)
                     , axis.title.y  = element_text(size = 10)
                     , axis.title.x  = element_text(size = 10))
gp2 = g[[2]] + theme(legend.position = "none", plot.margin = unit(c(.2,.2,.2,.2), "lines")
                     , plot.title = element_text(hjust = 0.5, size = 15)
                     , axis.text.y   = element_text(size = 10)
                     , axis.text.x   = element_text(size = 10)
                     , axis.title.y  = element_text(size = 10)
                     , axis.title.x  = element_text(size = 10))
gp3 = g[[3]] + theme(legend.position = "none", plot.margin = unit(c(.2,.2,.2,.2), "lines")
                     , plot.title = element_text(hjust = 0.5, size = 15)
                     , axis.text.y   = element_text(size = 10)
                     , axis.text.x   = element_text(size = 10)
                     , axis.title.y  = element_text(size = 10)
                     , axis.title.x  = element_text(size = 10))
gp4 = g[[4]] + theme(legend.position = "none", plot.margin = unit(c(.2,.2,.2,.2), "lines")
                     , plot.title = element_text(hjust = 0.5, size = 15)
                     , axis.text.y   = element_text(size = 10)
                     , axis.text.x   = element_text(size = 10)
                     , axis.title.y  = element_text(size = 10)
                     , axis.title.x  = element_text(size = 10))


gpls <- lapply(list(gp1,gp2,gp3, gp4), ggplotGrob )

lay <- rbind(c(1,1,1,1,1,1,1,1), c(1,1,1,1,1,1,1,1),
             c(2,2,2,2,2,2,2,2), c(2,2,2,2,2,2,2,2),
             c(3,3,3,3,3,3,3,3), c(3,3,3,3,3,3,3,3),
             c(4,4,4,4,4,4,4,4), c(4,4,4,4,4,4,4,4))

p1 = grid.arrange(gpls[[1]], gpls[[2]], gpls[[3]], gpls[[4]],             
                  layout_matrix = lay
) 



lay <- rbind(c(1,1,1,1,1,1,1,1,2),c(1,1,1,1,1,1,1,1,2))


ggsave(paste(ruta_salidas,"Max-Cor_CP1-4", ".jpg", sep = "") ,plot =grid.arrange(p1, ncol = 2, layout_matrix = lay, colorbar),
       width = 20, height = 25 ,units = "cm")



######## AUTOVECTORES #####

step = 10

a.vector = list()

for(cp.draw in 1:4){
  
  aux = data.frame(a.v = cps[[3]][,cp.draw], t = 1:length(cps[[3]][,cp.draw]))
  tiempo = which(abs(cps[[3]])[,cp.draw] == max(abs(cps[[3]])[,cp.draw], na.rm = T))
  
  a.vector[[cp.draw]] = ggplot(data = aux, aes(x = t)) + 
    
    geom_line(aes(y = a.v), color = "dodgerblue", size = 1) + 
    geom_hline(yintercept = 0, color = "black")+
    geom_vline(xintercept = tiempo, color = "red") +
    scale_x_continuous(limits = c(min(aux[,2]),max(aux[,2]))
                       , breaks = seq(min(aux[,2]),max(aux[,2]), by = step)
                       , labels = seq(min(anios), max(anios), by = step)) +
    
    scale_y_continuous(breaks = seq(-1,1, by = .25), limits = c(-1,1))+
    
    ggtitle(paste("Autovector CP", cp.draw, sep = "")) + 
    
    ylab("Correlación") + xlab("Años") +
    
    theme_minimal() +
    theme(axis.text.y = element_text(size = 14)
          , axis.text.x   = element_text(size = 14), 
          axis.title.y  = element_text(size = 14),
          axis.title.x  = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "bottom", legend.text = element_text(size = 12))
  
  ggsave(paste(ruta_salidas,"a.vector", cp.draw, ".jpg", sep = "") ,plot = a.vector[[cp.draw]],
         width = 20, height = 10 ,units = "cm")
}

# todos juntos



gp1 = a.vector[[1]] + theme(legend.position = "none", plot.margin = unit(c(.2,.2,.2,.2), "lines")
                            , plot.title = element_text(hjust = 0.5, size = 15)
                            , axis.text.y   = element_text(size = 10)
                            , axis.text.x   = element_text(size = 10)
                            , axis.title.y  = element_text(size = 10)
                            , axis.title.x  = element_text(size = 10))
gp2 = a.vector[[2]] + theme(legend.position = "none", plot.margin = unit(c(.2,.2,.2,.2), "lines")
                            , plot.title = element_text(hjust = 0.5, size = 15)
                            , axis.text.y   = element_text(size = 10)
                            , axis.text.x   = element_text(size = 10)
                            , axis.title.y  = element_text(size = 10)
                            , axis.title.x  = element_text(size = 10))
gp3 = a.vector[[3]] + theme(legend.position = "none", plot.margin = unit(c(.2,.2,.2,.2), "lines")
                            , plot.title = element_text(hjust = 0.5, size = 15)
                            , axis.text.y   = element_text(size = 10)
                            , axis.text.x   = element_text(size = 10)
                            , axis.title.y  = element_text(size = 10)
                            , axis.title.x  = element_text(size = 10))
gp4 = a.vector[[4]] + theme(legend.position = "none", plot.margin = unit(c(.2,.2,.2,.2), "lines")
                            , plot.title = element_text(hjust = 0.5, size = 15)
                            , axis.text.y   = element_text(size = 10)
                            , axis.text.x   = element_text(size = 10)
                            , axis.title.y  = element_text(size = 10)
                            , axis.title.x  = element_text(size = 10))


gpls <- lapply(list(gp1,gp2,gp3, gp4), ggplotGrob )

lay <- rbind(c(1,1,1,1,1,1,1,1), c(1,1,1,1,1,1,1,1),
             c(2,2,2,2,2,2,2,2), c(2,2,2,2,2,2,2,2),
             c(3,3,3,3,3,3,3,3), c(3,3,3,3,3,3,3,3),
             c(4,4,4,4,4,4,4,4), c(4,4,4,4,4,4,4,4))

p1 = grid.arrange(gpls[[1]], gpls[[2]], gpls[[3]], gpls[[4]],             
                  layout_matrix = lay
) 


ggsave(paste(ruta_salidas,"a.vector1-4", ".jpg", sep = "") ,plot = p1,
       width = 20, height = 25 ,units = "cm")



### Scree plot

aux = data.frame(landa = as.numeric(cps[[1]]), cp = 1:ncol(cps[[2]]))
step.a = 1 
x.lim = 6
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

ggsave(paste(ruta_salidas,"Scree", ".jpg", sep = "") ,plot = gg,
       width = 15, height = 10 ,units = "cm")


# practica 4
ruta_datos = "/home/auri/Facultad/Doc/Materias/MRyA/Practicas/P4/"
system(command = "mkdir P4/SalidasP4")
ruta_salidas = "/home/auri/Facultad/Doc/Materias/MRyA/Practicas/P4/SalidasP4"


## E1 ##
# a
library(readxl)
library(ggplot2)
source("funciones.R")
library(akima)


library(xlsx)

datos = as.data.frame(read.xlsx(paste(ruta_datos,"Datos_p4.xlsx", sep = ""), sheetIndex = 1, endRow = 34, startRow = 2))


for(i in 1:12){
  df <- interp(x = datos[,1],
               y =  datos[,2],
               z =  datos[,i+3], extrap = FALSE)
  
  
  nombre = paste("pp_", month.abb[i], sep = "")
  
  FieldPlot(field = df[[3]], lon = df[[1]], lat = df[[2]], mapa = "argentina",
            escala = seq(0, 250, by = 25), revert.brewer = F, sig = F, v.sig = corr[[2]],
            type.sig = "tile", color.sig = "white", size.point = 0.1, contour.fill = F, na.fill = -100000
            , contorno = F, nivel.cont = corr[[2]],save = T, colorbar = "YlGnBu", nombre.fig = nombre
            , titulo = nombre, salida = "/P4/SalidasP4/", height = 10, width = 10, mostrar = F)
  
  
}


# ondas anuales

est = c(362,210,006,483,221,001)


for(i in est){
  
  aux = datos[which(datos[,3] == i),-c(1,2,3)]
  print(i)
  print(aux)
  
  
  
  if( i != est[1]){
    aux2 = cbind(aux2, t(aux))
  } else {
    aux2 = t(aux)
  }
  
}
aux2 = cbind(aux2, 1:12)


colnames(aux2) = c("Posadas", "MDP",
                  "Rivadavia", "Formosa",
                  "B.Blanca", "La_Quiaca", "mes")

aux2 = as.data.frame(aux2)

aux = ggplot(data = aux2, aes(x = mes)) +
  
  geom_line(aes(y = Posadas, colour = "Posadas"), size = 1) + 
  geom_line(aes(y = MDP, colour = "MDP"), size = 1) +
  geom_line(aes(y = Rivadavia, colour = "Rivadavia"), size = 1) +
  geom_line(aes(y = Formosa, colour = "Formosa"), size = 1) + 
  geom_line(aes(y = B.Blanca, colour = "B. Blanca"), size = 1) +
  geom_line(aes(y = La_Quiaca, colour = "La Quiaca"), size = 1) +
  scale_x_continuous(limits = c(1,12), breaks = c(1:12))+
  scale_colour_manual(breaks = c("Posadas", "MDP", "Rivadavia", "Formosa", 
                                 "B. Blanca", "La Quiaca"), 
                      values = c("forestgreen", "firebrick", "dodgerblue", "orange3", "yellow3", "purple"), name = "")  +
  
  ggtitle("Onda Anual Media 1975-1991") + 
    ylab("") + xlab("") +
  theme_minimal() +
  theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5)) + theme(legend.position = "bottom", legend.text = element_text(size = 12))

ggsave(paste(ruta_salidas,"/onda_anual",".jpg",sep =""), plot = aux, width = 18, height = 13  , units = "cm")


L.8 = Lund(data = datos[,-c(1,2,3)], rc = 0.8)

L.3 = Lund(data = datos[,-c(1,2,3)], rc = 0.3)
#

# solo .8
library(akima)
df <- interp(x = datos$lon,
             y =  datos$lat,
             z =  datos[,L.8[1,1]], extrap = FALSE)

FieldPlot(field = df[[3]], lon = df[[1]], lat = df[[2]], mapa = "argentina",
          escala = seq(0, 250, by = 25), revert.brewer = F, sig = F, v.sig = corr[[2]],
          type.sig = "tile", color.sig = "white", size.point = 0.1, contour.fill = F, na.fill = -100000
          , contorno = F, nivel.cont = corr[[2]],save = T, colorbar = "YlGnBu", nombre.fig = "T.1_oct"
          , titulo = "T1 - Octubre", salida = "/P4/SalidasP4/", height = 10, width = 10, mostrar = F)


df <- interp(x = datos$lon,
             y =  datos$lat,
             z =  datos[,L.8[2,1]], extrap = FALSE)

FieldPlot(field = df[[3]], lon = df[[1]], lat = df[[2]], mapa = "argentina",
          escala = seq(0, 250, by = 25), revert.brewer = F, sig = F, v.sig = corr[[2]],
          type.sig = "tile", color.sig = "white", size.point = 0.1, contour.fill = F, na.fill = -100000
          , contorno = F, nivel.cont = corr[[2]],save = T, colorbar = "YlGnBu", nombre.fig = "T.2_Dic"
          , titulo = "T2 - Dic", salida = "/P4/SalidasP4/", height = 10, width = 10, mostrar = F)

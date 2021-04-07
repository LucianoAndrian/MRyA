# Luciano Andrian 
# Metodos de regresión y etc... 2021
# Practica 1
library(readxl)
library(ggplot2)
ruta_datos = "/home/auri/Facultad/Doc/Materias/MRyA/Practicas/P1/Datos P1-20210322/"
system(command = "mkdir P1/SalidasP1")
ruta_salidas = "/home/auri/Facultad/Doc/Materias/MRyA/Practicas/P1/SalidasP1"

###### E1 ###### 
# el txt esta vacio
# --> xlsx
datos = as.data.frame(read_excel(paste(ruta_datos, "pp_media_laboulaye.xlsx", sep = "")))

#A. DETECTE DATOS FALTANTES
# "mirando" los datos aparecen valores de -999.9
# tambien graficando se pueden identificar:
plot.ts(datos[,4])

# cuantos datos faltantes?
n.faltantes = length(which(datos == -999.9)) # 4

faltantes = matrix(data = NA, nrow = n.faltantes, ncol = 2)
faltantes[,1] = datos[,2][which(datos == -999.9, arr.ind = T)[1:n.faltantes,1]]
faltantes[,2] = colnames(datos)[which(datos == -999.9, arr.ind = T)[1:n.faltantes,2]]
#fechas con datos faltantes
print(faltantes)

#b Calcule la onda anual media (evolución durante el año) de la precipitación mensual
#acumulada y el desvío standard de la precipitación de cada mes. Calcule también
#mediana, moda, varianza, asimetría y curtosis de las variables mensualmente.
#Discuta su significado.

# son pocos datos faltantes, los elimino del calculo -999.9 = NA

datos[which(datos == -999.9, arr.ind = T)] = NA

# onda anual media

#promedio de cada mes
aux = data.frame(round(sapply(datos[,3:14],mean, na.rm = T), digits = 1),1:12)
colnames(aux) = c("pp", "mes")

onda_anual =  ggplot(data = aux, aes(x = mes, y = pp)) + theme_minimal() +
  geom_line(color = "steelblue", size = 1) +
  scale_x_continuous(limits = c(1, 12),breaks =  seq(1,12, by = 1), labels = month.abb, name = "") +
  scale_y_continuous(limits = c(0,150),breaks = seq(0,150,by = 25), name = "mm") + 
  ggtitle("Onda anual media PP mensual acumulada - Laboulaye 1941-2004") + 
  theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
         axis.title.x  = element_text(size = 14),
         panel.border = element_rect(colour = "black", fill = NA, size = 1),
         panel.ontop = F,
         plot.title = element_text(hjust = 0.5))

ggsave(paste(ruta_salidas,"/E1.onda_anual",".jpg",sep =""), plot = onda_anual, width = 15, height = 13  , units = "cm")

# desvio estandar mensual, mediana, curtosis y moda
library(modeest) #moda
library(e1071) #curtosis

SD = vector()
MED = vector()
KURT = vector()
MODA = vector()

for(i in 3:14){
  SD[i-2] = sd(datos[,i], na.rm = T)
  MED[i-2] = median(datos[,i], na.rm = T)
  KURT[i-2] = kurtosis(datos[,i], na.rm = T)
  MODA[i-2] = mlv(datos[,i], na.rm = T)
}

parametros = data.frame(mes = month.abb, sd = round(SD, digits = 1), med = round(MED, digits = 1),
                 kur = round(KURT, digits = 1), moda = round(MODA, digits = 1))

#   FALTA LA ASIMETRIA 

#C Complete los datos faltantes con el valor medio mensual. 
#Discuta la posibilidad deutilizar otra metodología para completar dato faltante.

#media mensual en todo el periodo sin incluir los datos falantes
prom.meses = as.vector(sapply(datos[,3:14],mean, na.rm = T))

# los datos faltantes fueron reemplazados por NA.
aux = datos
# reemplazando por el valor medio mensual en cada caso
for(i in 1:length(which(is.na(aux), arr.ind = T)[,2])){
  datos[which(is.na(aux), arr.ind = T)][i] = round(prom.meses[which(is.na(aux), arr.ind = T)[i,2]-2], digits = 1)
}

# d grafico

aux = data.frame(round(sapply(datos[,3:14],mean, na.rm = T), digits = 1),1:12)
colnames(aux) = c("pp", "mes")

onda_anual_bar = ggplot(data = aux, aes(x = mes, y = pp, fill = pp)) + theme_minimal() +
  geom_bar(aes(x = mes, y = pp), stat = "identity", alpha = 1, show.legend = F) +
  scale_fill_gradient(low = "deepskyblue",  high = "dodgerblue4") +
  geom_line(data = aux, aes(y = pp+SD, x = mes), color = "dodgerblue4") +
  geom_line(data = aux, aes(y = pp-SD, x = mes), color = "deepskyblue") +
  geom_hline(yintercept = 0, color = "grey")+
  scale_x_continuous(limits = c(1, 12),breaks =  seq(1,12, by = 1), labels = month.abb, name = "") +
  scale_y_continuous(limits = c(-25,250),breaks = seq(-25,250,by = 25), name = "mm") + 
  ggtitle("PP media mensual acumulada y SD- Laboulaye 1941-2004") + 
  theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5))

ggsave(paste(ruta_salidas,"/E1.onda_anual_sd",".jpg",sep =""), plot = onda_anual_bar, width = 15, height = 13  , units = "cm")


# e Calcule la evolución de las anomalías de lluvia respecto de la media en todo el
#registro para los años 1960, 1970, 1980, 1990 y 2000. Grafique las 5 series en un
#mismo gráfico de líneas. Analice las diferencias entre las distintas series.

anom = as.data.frame(array(data = NA, dim = c(12*5,3))) 
anios = c(1960,1970,1980,1990,2000)

# calculo de anomalias, promedios mensuales ya definidos antes como "prom.meses"
# y ordenado para graficar
j = 0
for(i in 1:length(anios)){
  anom[(1+12*j):(12+12*j),1] = t(datos[which(datos == anios[i], arr.ind = T)[1,1],3:14] - prom.meses)
  anom[(1+12*j):(12+12*j),2] = anios[i]
  anom[(1+12*j):(12+12*j),3] = 1:12
  j = j + 1
}

anom[,2] = as.factor(anom[,2]) # para usar los anios en la referencia

colnames(anom) = c("Anom", "Año", "Mes")
anoms = ggplot(data = anom, aes(x = Mes, y = Anom, group = Año)) + theme_minimal() +
  geom_line(data = anom, aes(y = Anom, colour = Año), size = 1) +
  geom_hline(yintercept = 0, color = "black") + 
  
  scale_x_continuous(limits = c(1, 12),breaks =  seq(1,12, by = 1), labels = month.abb, name = "") +
  scale_y_continuous(limits = c(-100,100),breaks = seq(-100,100,by = 20), name = "mm") + 
  ggtitle("Anomalías respecto al período 1941 - 2004") + 
  theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5)) +theme(legend.position = "bottom")

ggsave(paste(ruta_salidas,"/E1.d_anoms",".jpg",sep =""), plot = anoms, width = 15, height = 13  , units = "cm")


###### E2 ######
estaciones = as.data.frame(read_excel(paste(ruta_datos, "pampa-pp.xlsx", sep = ""), sheet = 1 ))
datos = as.data.frame(read_excel(paste(ruta_datos, "pampa-pp.xlsx", sep = ""), sheet = 2 , skip = 1))


#a. príodo en común # ESTABA EN LA PRIMERA HOJA DEL XSL..... 

# ESTO NO ES NECESARIO ESTO (IGUAL FUNCIONA)

pos.anios = which(!is.na(datos[,1]))
minimos = maximos = vector()
for(i in 1:length(pos.anios)){
  if(i != length(pos.anios)){
    
    minimos[i] = min(datos[pos.anios[i]:(pos.anios[i+1]-1),3])
    maximos[i] = max(datos[pos.anios[i]:(pos.anios[i+1]-1),3])
    
  } else {
    
    minimos[i] = min(datos[pos.anios[i]:length(datos[,1]),3])
    maximos[i] = max(datos[pos.anios[i]:length(datos[,1]),3])
    
  }
}

periodo.comun = seq(max(minimos), min(maximos), by = 1)
# periodo en comun 1959-2000

#b. Calcule la onda anual media de precipitación en cada estación para el período
#   coincidente

# reordeno datos
datos2 = array(data = NA, dim = c(length(periodo.comun), 12, length(estaciones[,3])-1))

# calculo solo en los años de periodo en comun para cada estacion
for(i in 1:length(pos.anios)){
  if(i != length(pos.anios)){
    
    x = datos[pos.anios[i]:(pos.anios[i+1]-1),3:15]
    l = seq(which(x[,1] == min(periodo.comun)), which(x[,1] == max(periodo.comun)))
    
    datos2[,,i] = as.matrix(x[l,2:13])
    
  } else {
    
    minimos[i] = min(datos[pos.anios[i]:length(datos[,1]),3])
    x = datos[pos.anios[i]:length(datos[,1]),3:15]
    l = seq(which(x[,1] == min(periodo.comun)), which(x[,1] == max(periodo.comun)))
    
    datos2[,,i] = as.matrix(x[l,2:13])
    
  }
}

# onda anual para cada estacion

onda_anual = apply(datos2, c(2,3), mean, na.rm = T)


aux = matrix(data = NA, nrow = 12*16, ncol = 3)
aux[,3] = seq(1:12)
aux[,2] = onda_anual
aux = as.data.frame(aux)

aux2 = c("B.Blanca", "Pergamino", "Laboulaye", "OCBA", "Rosario", "Nve.Julio",
         "R.Cuarto", "V.Dolores", "Pilar", "Dolores", "Azul", "C.Suarez",
         "Pigue", "S.Viejo", "M.Juarez", "trs·arroyos")
for(i in 0:15){
  aux[(1+12*i):(12+12*i),1] = aux2[i+1]
}

aux[,1] = as.factor(aux[,1])

colnames(aux) = c("Estaciones", "PP", "Meses")

ggplot(data = aux, aes(x = Meses, y = PP, colour = Estaciones)) + 
  geom_line() + theme_minimal()




 


# c acumulada media de MAM, JJA, SON y DJF

seasons = array(data = NA, dim = c(4,16))
seasons[1,] = apply(onda_anual[3:5,], c(2), mean) # MAM
seasons[2,] = apply(onda_anual[6:8,], c(2), mean) # JJA
seasons[3,] = apply(onda_anual[9:11,], c(2), mean) # SON 
seasons[4,] = apply(onda_anual[c(1,2,12),], c(2), mean) # DJF

colnames(seasons) = as.character(unique(datos[,2]))
rownames(seasons) = c("MAM", "JJA", "SON", "DJF")


# d GRaficar, INTERPOLAR?

library(maps)
library(maptools)
library(ggplot2)
library(ggmap)
library(mapproj)
library(grid)
library(gridExtra)
library(akima)
library(metR)
library(RColorBrewer)


# problema con las latitudes: quedaban muy desfasadas de las posiciones reales
# algunas coordenadas fueron corregidas por ejemplo de -30,7 --> -30,5
# si bien antes pudo leer bien los valores con "," en este caso no lo hacia, las coordenadas 
# fueron pasadas a un txt con "." en lugar de ","

y = read.table(paste(ruta_datos, "prueba.txt", sep = ""))
puntos = data.frame(lon = y[,1]-.2, lat = y[,2]-.2) # pese a las correcciones, para ambas coordenadas se resto -.2º

#mapa estilo google maps.. (no funcionan los otros)
mapa = get_map(location = c(left = -68, bottom = -40, right=-55, top = -30 ), source = 'stamen', maptype = "toner-lite",zoom=6)

#mapa limites 
map <- map_data("world2", region = c("Argentina", "Uruguay"), colour = "black")

est = c("MAM", "JJA", "SON", "DJF")

puntos = cbind(puntos, aux2)

puntos = cbind(puntos, seasons[1,], seasons[2,], seasons[3,], seasons[4,])

#graficado



# no queda muy bien con el mapa de fondo, buscando division politica de argentina para usar....
for(i in 1:4){
  
  aux = data.frame(pp = seasons[i,], lon = y[,1], lat = y[,2]) # data.frame para graficar
  
  # INTERPOLACION
  akima_li <- with(aux,interp(x = aux$lon, y = aux$lat, z = aux$pp,
                            yo = seq(min(aux$lat)-1, max(aux$lat)+1, length = 20*15.71),
                            xo = seq(min(aux$lon)-1, max(aux$lon)+1, length = 20*8.87),
                            linear = F, extrap = T))
 aux =  data.frame(expand.grid(x = akima_li$x, y = akima_li$y), z = c(akima_li$z))
 
 
 v = ggmap(mapa, extent = 'normal') + 
   ggtitle("") +
   geom_contour_fill(data = aux, aes(x = x, y = y, z = z), alpha = 0.3, na.fill = -10000 , breaks = seq(30,120, by = 5)) +
   scale_fill_stepsn(limits = c(30,125), name = "mm", colours = (brewer.pal(n = 9 , "YlGnBu")), na.value = "white", breaks = seq(30,120,by = 10),
                     guide = guide_colorbar(barwidth = 1, barheight = 10, title.position = "top", title.hjust = 0.5,
                                            raster = F, ticks = T, label.theme = element_text(size = 10)))+
   geom_polygon(data = map, aes(x = long -360 ,y = lat, group = group),fill = NA, color = "black") +
   coord_cartesian(xlim = c(-65.5, -57), ylim = c(-39, -31)) +
   geom_point(data=puntos, aes(x = lon, y = lat), col="red",size=2) +
   geom_text(data = puntos, aes(label = aux2), vjust = -1) +
   geom_text(data = puntos, aes(label = round(puntos[,i+3], digits = 1)), vjust = 1.5) +
   xlab("") + 
   ylab("") +
   theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
         axis.title.x  = element_text(size = 14),
         panel.border = element_rect(colour = "black", fill = NA, size = 1),
         panel.ontop = F,
         plot.title = element_text(hjust = 0.5)) +
   ggtitle(paste("PP media - ", est[i], sep = "")) 
 
 ggsave(paste(ruta_salidas,"/E2.d_", est[i],".jpg",sep =""), plot = v, width = 15, height = 15, units = "cm")
 
}

##### E3 #####
datos = as.data.frame(read.table(paste(ruta_datos, "pp_media_regional.txt", sep = "")))
colnames(datos) = c("Anios", "Posadas", "P.d.Libres", "Aeroparque", "Ezeiza")



aux = ggplot(data = datos, aes(x = Anios)) +
  
  geom_line(aes(y = Posadas, colour = "Posadas"), size = 1) + 
  geom_line(aes(y = P.d.Libres, colour = "P.d.Libres"), size = 1) +
  geom_line(aes(y = Aeroparque, colour = "Aeroparque"), size = 1) +
  geom_line(aes(y = Ezeiza, colour = "Ezeiza"), size = 1) +
  
  scale_colour_manual(breaks = c("Posadas", "P.d.Libres", "Aeroparque", "Ezeiza"), 
                      values = c("forestgreen", "firebrick", "dodgerblue", "orange3"), name = "") + 
  
  ggtitle("Series PP anual 1959-2005") + ylab("") + xlab("") +
  theme_minimal() +
  theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5)) + theme(legend.position = "bottom", legend.text = element_text(size = 12))

ggsave(paste(ruta_salidas,"/E3.a_series",".jpg",sep =""), plot = aux, width = 15*2, height = 8*2, units = "cm")


#b. 
medias = colMeans(datos)
SD = apply(datos, c(2), sd, na.rm = T)
mediana = apply(datos, c(2), median, na.rm = T)
var = apply(datos, c(2), var, na.rm = T) # !?¿¿

#c AREALES ??

#d

aux = as.data.frame(t(t(datos) - medias)); aux[,1] = datos[,1]

#
aux = ggplot(data = aux, aes(x = Anios)) +
  
  geom_line(aes(y = Posadas, colour = "Posadas"), size = 1) + 
  geom_line(aes(y = P.d.Libres, colour = "P.d.Libres"), size = 1) +
  geom_line(aes(y = Aeroparque, colour = "Aeroparque"), size = 1) +
  geom_line(aes(y = Ezeiza, colour = "Ezeiza"), size = 1) +
  
  geom_hline(yintercept = 0, color = "black") +
  
  scale_colour_manual(breaks = c("Posadas", "P.d.Libres", "Aeroparque", "Ezeiza"), 
                      values = c("forestgreen", "firebrick", "dodgerblue", "orange3"), name = "") + 
  
  ggtitle("Series PP anual 1959-2005") + ylab("") + xlab("") +
  theme_minimal() +
  theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5)) + theme(legend.position = "bottom", legend.text = element_text(size = 12))

ggsave(paste(ruta_salidas,"/E3.d_series",".jpg",sep =""), plot = aux, width = 15*2, height = 8*2, units = "cm")
    
# lo mismo con datos faltantes...   

datos = as.data.frame(read.table(paste(ruta_datos, "pp_media_regional_faltante.txt", sep = "")))
colnames(datos) = c("Anios", "Posadas", "P.d.Libres", "Aeroparque", "Ezeiza")


aux = ggplot(data = datos, aes(x = Anios)) +
  
  geom_line(aes(y = Posadas, colour = "Posadas"), size = 1) + 
  geom_line(aes(y = P.d.Libres, colour = "P.d.Libres"), size = 1) +
  geom_line(aes(y = Aeroparque, colour = "Aeroparque"), size = 1) +
  geom_line(aes(y = Ezeiza, colour = "Ezeiza"), size = 1) +
  
  scale_colour_manual(breaks = c("Posadas", "P.d.Libres", "Aeroparque", "Ezeiza"), 
                      values = c("forestgreen", "firebrick", "dodgerblue", "orange3"), name = "") + 
  
  ggtitle("Series PP anual 1959-2005 - Faltante") + ylab("") + xlab("") +
  theme_minimal() +
  theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5)) + theme(legend.position = "bottom", legend.text = element_text(size = 12))

ggsave(paste(ruta_salidas,"/E3.a_series_faltante",".jpg",sep =""), plot = aux, width = 15*2, height = 8*2, units = "cm")


#b. 
medias = colMeans(datos, na.rm = T)
SD = apply(datos, c(2), sd, na.rm = T)
mediana = apply(datos, c(2), median, na.rm = T)
var = apply(datos, c(2), var, na.rm = T) # !?¿¿

#c AREALES ??

#d

aux = as.data.frame(t(t(datos) - medias)); aux[,1] = datos[,1]

#
aux = ggplot(data = aux, aes(x = Anios)) +
  
  geom_line(aes(y = Posadas, colour = "Posadas"), size = 1) + 
  geom_line(aes(y = P.d.Libres, colour = "P.d.Libres"), size = 1) +
  geom_line(aes(y = Aeroparque, colour = "Aeroparque"), size = 1) +
  geom_line(aes(y = Ezeiza, colour = "Ezeiza"), size = 1) +
  
  geom_hline(yintercept = 0, color = "black") +
  
  scale_colour_manual(breaks = c("Posadas", "P.d.Libres", "Aeroparque", "Ezeiza"), 
                      values = c("forestgreen", "firebrick", "dodgerblue", "orange3"), name = "") + 
  
  ggtitle("Series PP anual 1959-2005 - faltante") + ylab("") + xlab("") +
  theme_minimal() +
  theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5)) + theme(legend.position = "bottom", legend.text = element_text(size = 12))

ggsave(paste(ruta_salidas,"/E3.d_series_faltante",".jpg",sep =""), plot = aux, width = 15*2, height = 8*2, units = "cm")

#### E4 #####
# mirando los archivos veo valores de -9999 que estan sin espacio con la fila anterior, ej 221-9999
# voy a reemplazar "-9" por espacio y el dato -9999 va quedar como 999, para las dos estaciones
posadas = as.data.frame(read.table(text = gsub("-9", " ", readLines(paste(ruta_datos, "T8717800.txt", sep = ""))),
                                   skip = 1, na.strings = 999))

# archivo posadas indica periodo 1902-1994 -> 93 anios
nrow(posadas) #92!falta un anio
anios.posadas = as.numeric(gsub("3138717800*", "\\1", as.factor(posadas[,1])))
anios = seq(1902,1994)

anio.faltante = min(anios[which(anios.posadas != anios)])

#falta el anio 1980, lo agrego como dato faltante
posadas[,1] = anios.posadas
posadas = rbind(posadas[1:(which(posadas[,1] == 1979, arr.ind = T)),], c(1980, rep(NA,12)),
                posadas[(which(posadas[,1] == 1981, arr.ind = T)):length(posadas[,1]),])



junin = as.data.frame(read.table(text = gsub("-9", " ", readLines(paste(ruta_datos, "T8754800.txt", sep = ""))),
                                 skip = 1, na.strings = 999))
# archivo indica periodo de 37 años
nrow(junin) #ok

junin[,1] = as.numeric(gsub("3138754800*", "\\1", as.factor(junin[,1])))


# Determinar si la temperatura mensual de estas dos estaciones pertenece a la misma poblacion
# (probablemente NO....)

#antes de testear las medias, deberia testear las varianzas pero en ese caso hay que 
#hacer por mes? ya que para la varianza no es representativo promediar todo el año?

#media para cada mes, dentro del rango en comun 1958-1994
media.posadas = apply(posadas[which(posadas[,1]==1958):which(posadas[,1]==1994),2:13], 2, mean, na.rm = T)
media.junin = apply(junin[,2:13], 2, mean, na.rm = T)

fisher.test(media.posadas, media.junin)
var.test(media.posadas, media.junin) # esto esta bien, segun el test F (revisado "a mano")
# rechazo H0, la varianzas no perteneces a la misma poblacion

t.test(media.posadas, media.junin, alternative = "two.side", conf.level = .95) #no pertenecen a la misma poblacion


# se podria calcular el coef. de correlacion de pearson, pero el ciclo anual generaria correlacion espuria
# se deberia ver calculando la anomalia a cada mes, restando el ciclo anual o por trimestres.


# Teorico



##### E5 #####
source("funciones.R")
viento = as.data.frame(read_excel(paste(ruta_datos, "viento.xlsx", sep = "")))

aux = data.frame(mdp = 3:18, psd = 26:41, nqn = 49:64)

var = c("Velocidad Media", "Frecuencia")

estaciones = c("Mar del Plata", "Posadas", "Neuquen")

for(i in 1:3){
  for (j in 1:2){
    
    aux2 = viento[aux[,i],]
    
    aux2 = aux2[as.data.frame(which(aux2 == var[j], arr.ind = T))$row,3:15]
    colnames(aux2) = NULL
    v = array((as.numeric(as.matrix(aux2))), dim = c(8,13))
    
    x = c(360,45,90,135,180,225,270,315)
    v = as.data.frame(v)
    v$dir = x
    
    r.vientos(vel = v[,1], dir = v$dir, step = 1, p.card = 8, colorbar = "Spectral", 
              colorbar.name = "m/s", colorbar.rev = F, col_bar = T,
              titulo = paste(var[j], "enero - ", estaciones[i], sep = " "), 
              save = T,salida = ruta_salidas, nombre = paste("/", colnames(aux)[i], var[j],"_ene", sep = ""), 
              size.titulo = 40, text.size = 35, cb.size = 5, letter.size = 20)
    
    r.vientos(vel = v[,7], dir = v$dir, step = 1, p.card = 8, colorbar = "Spectral", 
              colorbar.name = "m/s", colorbar.rev = F, col_bar = T,
              titulo = paste(var[j], "julio - ", estaciones[i], sep = " "),
              save = T,salida = ruta_salidas, nombre = paste("/",colnames(aux)[i], var[j],"_jul", sep = ""),
              size.titulo = 40, text.size = 35, cb.size = 5, letter.size = 20)
    
  }
}


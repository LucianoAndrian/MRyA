# Luciano Andrian 
# Metodos de regresión y etc... 2021
# Practica 1

library(ggplot2)
ruta_datos = "/home/auri/Facultad/Doc/Materias/MRyA/Practicas/P1/Datos P1-20210322/"
system(command = "mkdir P1/SalidasP1")
ruta_salidas = "/home/auri/Facultad/Doc/Materias/MRyA/Practicas/P1/SalidasP1"

###### E1 ###### 
# el txt esta vacio
# --> xlsx
library(xlsx)
datos = as.data.frame(read.xlsx(paste(ruta_datos, "pp_media_laboulaye.xlsx", sep = ""), sheetIndex = 1))

###############################################################################
# control de calidad
# faltantes: estan marcados con -999.9
n.faltantes = length(which(datos == -999.9)) # 4

#cuales son?
faltantes = matrix(data = NA, nrow = n.faltantes, ncol = 2)
faltantes[,1] = datos[,2][which(datos == -999.9, arr.ind = T)[1:n.faltantes,1]]
faltantes[,2] = colnames(datos)[which(datos == -999.9, arr.ind = T)[1:n.faltantes,2]]
#fechas con datos faltantes
print(faltantes)

# pocos datos, los elimino
datos[which(datos == -999.9, arr.ind = T)] = NA


# hay años faltantes o repetidos

# de levels solo cuenta los numeros, 
# ej, si x = c(1,1,NA,3,4)
# > levels(factor(x))
# [1] "1" "3" "4"

l = length(levels(factor(datos[,2])))

# si existieran años repetidos l != nrow(datos)

l == nrow(datos) # no hay años repetidos

# faltantes, salteados o desordenados
# de darse cualcuiera de estos casos seria FALSE
identical(datos[,2], seq(min(datos[,2]), max(datos[,2]), by = 1))

###############################################################################

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
estaciones = as.data.frame(read.table(file = paste(ruta_datos, "datos-pampa-pp_estaciones.txt", sep = ""), sep = "", header = T))



datos = as.data.frame(read.table(paste(ruta_datos, "datos-pampa-pp.txt", sep = ""), sep = "\t", header = T))

#a control de calidad y periodo en comun.

##############################################################################
# periodo en comun en la hoja "estaciones" queda 1959-2000

est = as.numeric(levels(factor(datos$est)))

max.anios = min.anios = vector()

for(i in 1:length(est)){
  anios = datos[which(datos[,1] == est[i]),2]
  
  l = length(levels(factor(anios)))
  
  # si existieran años repetidos l != nrow(datos)
  
  no.rep = l == length(anios) # no hay años repetidos
  
  # faltantes, salteados o desordenados
  # de darse cualcuiera de estos casos seria FALSE
  falt = identical(as.numeric(anios), seq(min(anios), max(anios), by = 1))
  
  if(no.rep & falt){
    
    max.anios[i] = max(anios, na.rm = T)
    min.anios[i] = min(anios, na.rm = T)
  }
}

periodo.comun = seq(max(min.anios), min(max.anios), by = 1)


datos2 = array(data = NA, dim = c(length(periodo.comun), 12, length(est)))
for(i in 1:length(est)){
  
  aux = datos[which(datos[,1] == est[i]),]
  min.per = which(aux$año == periodo.comun[1])
  max.per = which(aux$año == max(periodo.comun))
  
  datos2[,,i] = as.matrix(aux[min.per:max.per,-c(1,2)])

}
##############################################################################

#las estaciones quedaron desordanas respecto a "estaciones"

estaciones = estaciones[order(estaciones$estacion,decreasing=F),]

# onda anual para cada estacion
onda_anual = apply(datos2, c(2,3), mean, na.rm = T)

aux = matrix(data = NA, nrow = 12*16, ncol = 3)
aux[,3] = seq(1:12)
aux[,2] = onda_anual
aux = as.data.frame(aux)

for(i in 0:15){
  aux[(1+12*i):(12+12*i),1] = as.character(estaciones$nombre[i+1])
}

aux[,1] = as.factor(aux[,1])

colnames(aux) = c("Estaciones", "PP", "Meses")

ggplot(data = aux, aes(x = Meses, y = PP, colour = Estaciones)) + 
  geom_line() + theme_minimal()

# c acumulada media de MAM, JJA, SON y DJF

seasons = array(data = NA, dim = c(4,16))
seasons[1,] = apply(onda_anual[3:5,], c(2), sum) # MAM
seasons[2,] = apply(onda_anual[6:8,], c(2), sum) # JJA
seasons[3,] = apply(onda_anual[9:11,], c(2), sum) # SON 
seasons[4,] = apply(onda_anual[c(1,2,12),], c(2), sum) # DJF

colnames(seasons) = as.character(unique(datos[,2]))
rownames(seasons) = c("MAM", "JJA", "SON", "DJF")


# d GRaficar, INTERPOLAR

puntos = cbind(estaciones$lon, estaciones$lat, seasons[1,], seasons[2,], seasons[3,], seasons[4,])

df <- interp(x = puntos[,1],
             y =  puntos[,2],
             z =  puntos[,3], extrap = FALSE)


# FieldPlot(field = df[[3]], lon = df[[1]], lat = df[[2]], mapa = "argentina"
#           , escala = seq(0,300, by = 30), tile = T, revert.brewer = T)


library(fields)
aux.breaks <- seq(0, 300, 30) # Definir los umbrales para graficar
colores <- rep(rev(brewer.pal(10, "Spectral")), each = 1) # Barra de colores

outline <- map("world", regions = "Argentina", 
               exact = TRUE, plot = FALSE)

xrange <- range(outline$x, na.rm = TRUE) # Veo los límites de las coordenadas.
yrange <- range(outline$y, na.rm = TRUE)
xbox <- xrange + c(-2, 2)
ybox <- yrange + c(-2, 2)
subset <- !is.na(outline$x)

# Grafico todo junto: 

jpeg("diciembre.jpg", pointsize = 12, res = 115, bg = "white")
image.plot(df$x, df$y, df$z, col = colores, breaks = aux.breaks, 
           ylab = "Latitud", xlab = "Longitud", 
           lab.breaks = aux.breaks, legend.shrink = 1, 
           xlim = c(min(df$x)-2, max(df$x)+2), ylim = c(min(df$y)-2, max(df$y)+2)) 
title ("SON")

contour(df$x, df$y, df$z, col = "black", add = TRUE, levels = seq(-1, 1, 0.15), ltw=1.4)

polypath(c(outline$x[subset], NA, c(xbox, rev(xbox))),
         c(outline$y[subset], NA, rep(ybox, each=2)),
         col = "white", rule = "evenodd", border = NA)

map(database = "world", add = TRUE, lwd = 0.5, col = "black")
graphics.off()



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

###### LEE Y REEMPLAZA!!!!!@#######
# voy a reemplazar "-9" por espacio y el dato -9999 va quedar como 999, para las dos estaciones
posadas = as.data.frame(read.table(text = gsub("-9", " ", readLines(paste(ruta_datos, "T8717800.txt", sep = ""))),
                                   skip = 1, na.strings = 999))

# archivo posadas indica periodo 1902-1994 -> 93 anios
posadas[,1] = as.numeric(gsub("3138717800*", "\\1", as.factor(posadas[,1])))


# hay años faltantes o repetidos

# de levels solo cuenta los numeros, 
# ej, si x = c(1,1,NA,3,4)
# > levels(factor(x))
# [1] "1" "3" "4"

l = length(levels(factor(posadas[,1])))

# si existieran años repetidos l != nrow(datos)

l == nrow(posadas) # no hay años repetidos

# faltantes, salteados o desordenados
# de darse cualcuiera de estos casos seria FALSE
identical(posadas[,1], seq(min(posadas[,1]), max(posadas[,1]), by = 1))

# HAY FALTANTE, REPETIDO O DESORDENADO

# Faltante?
l == length(seq(min(posadas[,1]), max(posadas[,1]), by = 1))
# Si hay un dato faltante
# cual?
anios = seq(min(posadas[,1]), max(posadas[,1]), by = 1)

anio.faltante = min(anios[which(anios.posadas != anios)])
#falta el anio 1980, lo agrego como dato faltante

posadas[,1] = anios.posadas
posadas = rbind(posadas[1:(which(posadas[,1] == 1979, arr.ind = T)),], c(1980, rep(NA,12)),
                posadas[(which(posadas[,1] == 1981, arr.ind = T)):length(posadas[,1]),])



junin = as.data.frame(read.table(text = gsub("-9", " ", readLines(paste(ruta_datos, "T8754800.txt", sep = ""))),
                                 skip = 1, na.strings = 999))
# archivo indica periodo de 37 años
junin[,1] = as.numeric(gsub("3138754800*", "\\1", as.factor(junin[,1])))



l = length(levels(factor(junin[,1])))

# si existieran años repetidos l != nrow(datos)

l == nrow(junin) # no hay años repetidos

# faltantes, salteados o desordenados
# de darse cualcuiera de estos casos seria FALSE
identical(junin[,1], seq(min(junin[,1]), max(junin[,1]), by = 1))

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
# 
# H0 = media.posadas = media.junin
# H1 = media.posadas != media.junin
# 
# alpha = 0.05
# n1 = n2 = n

Sx1x2 =sqrt(0.5*(var(media.posadas)+var(media.junin))) 
t.est = (mean(media.posadas) - mean(media.junin))/(Sx1x2*sqrt(2/n))

# t.est igual al obtenido t.test
# 
# t.test sigue un distribucion t-st con n1+n2-1 grados de libertad


# se podria calcular el coef. de correlacion de pearson, pero el ciclo anual generaria correlacion espuria
# se deberia ver calculando la anomalia a cada mes, restando el ciclo anual o por trimestres.

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


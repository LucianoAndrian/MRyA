# practica 2 
##### E1 #####
#Datos
x = c(6, 8, 10, 12, 14, 6, 10, 10, 8, 12, 16, 18, 16, 14, 12, 16, 14)
y = c(4, 8, 6, 4, 6, 10, 8, 10, 12, 8, 6, 6, 10, 8, 12, 10, 14)

x1 = c(18, 20, 20, 22, 24, 24)
y1 = c(16, 14, 18, 18, 14, 16)

x2 = c(28, 28, 30, 30)
y2 = c(22, 24, 22, 24)

# a
plot(x,y, col = "green", type = "p") # r ~ 0 

r = cor.test(x,y, alternative = "two.side", method = "pearson", conf.level = .95)
# H0: r pertenece a una poblacion con rho = 0
# H1: r NO... etc
#alpha = 0.05
# t = r * sqrt((n-2)/(1-r**2))
# qt(p = 0.95, df = n-2)

# pearson 
r$estimate

#p.value
r$p.value

# p.value > 0.05, no rechazo H0. r no significativamente distinto de cero con un 95% de confianza

x = c(x, x1)
y = c(y, y1)
# a
plot(x,y, col = "green", type = "p") 

r1 = cor.test(x,y, alternative = "two.side", method = "pearson", conf.level = .95)
# H0: r pertenece a una poblacion con rho = 0
# H1: r NO... etc
#alpha = 0.05

# pearson 
r1$estimate

#p.value
r1$p.value # reachazo H0, r es significicativamente distinto de cero con un 95% de confianza


x = c(x, x2)
y = c(y, y2)
# a
plot(x,y, col = "green", type = "p") 

r2 = cor.test(x,y, alternative = "two.side", method = "pearson", conf.level = .95)
# H0: r pertenece a una poblacion con rho = 0
# H1: r NO... etc
#alpha = 0.05

# pearson 
r2$estimate

#p.value
r2$p.value # reachazo H0, r es significicativamente distinto de cero con un 95% de confianza

#etc

##### E2 #####

source("funciones.R")

t = c(1:40)
y = t**1.1
recta = recta.reg(x = t, y = y, residuo = T, plot = T, Summary = TRUE)

#residuos
plot(x = t, y = recta[[2]])


r = cor(t, y)**2 # "R**2

# test Fisher
Fc = (r/(1-r))*(length(t)-2) # da igual que summary. OK

#Fteorico
Ft = qf(df1 = 1, df2 = length(t)-2, p = .95)

Fc > Ft # ademas, p.value <<< 0.05

#rechazo H0 con un 95% de confianza. la pendiente No pertenecer a una poblacion con pendiente cero


##### E3 #####
ruta_datos = "/home/auri/Facultad/Doc/Materias/MRyA/Practicas/P2/DatosP2/"
system(command = "mkdir P2/SalidasP2")
ruta_salidas = "/home/auri/Facultad/Doc/Materias/MRyA/Practicas/P2/SalidasP2"

datos = as.data.frame(read.table(paste(ruta_datos, "datosE2.txt", sep = ""), header = T))

# valor medio para cada variable

#recta de regresion para:
# (y1,x1)

E2 = function(x, y){
  
  v2.med = mean(x)
  v1.med = mean(y)
  
  r = cor(x,y, method = "pearson")
  
  recta = lm(y ~ x)
  plot(recta$residual, col = "red")
  
  a = recta$coefficients[1]
  b = recta$coefficients[2]
  
  
 result = c(v1.med, v2.med, a, b, r)
 names(result) = NULL
 return(result)
}


tabla = matrix(data = NA, nrow = 4, ncol = 5)
tabla[1,] = E2(x = datos$X1, y = datos$Y1)
tabla[2,] = E2(x = datos$X1, y = datos$Y2)
tabla[3,] = E2(x = datos$X1, y = datos$Y3) # residuos no aleatorios
tabla[4,] = E2(x = datos$X2, y = datos$Y4)

colnames(tabla) = c("V.med1", "V.med2", "a", "b", "r")

##### E4 #####
datos = read.table(paste(ruta_datos, "datos_Ej4.txt", sep = ""))

# relacion entre las variables
plot.ts(datos[,1])
plot.ts(datos[,2]) # valores "raros" hacia el final de la serie
# pero nse que variables es.
plot(x = datos[,1], y =datos[,2]) 
#relacion lineal en un intervalo acotado

r = cor.test(x = datos[,1], y = datos[,2], alternative = "two.side", 
          method = "pearson", conf.level = .95)

r$p.value # < 0.05 rechazo H0, r no pertenece a una poblacion con rho = cero

#usando t.st

t = r$estimate*sqrt((length(datos[,1])-2)/(1-r$estimate**2))

t.teor = qt(p = .95, df = length(datos[,1])-2, lower.tail = F)

abs(t) > abs(t.teor) # |t| > |t.teor| rechazo H0.. etc

##### E5 #####

def = c(89, 19, 2, 18, 5, 3, 48, 73, 23, 14, 22, 47, 23, 24, 4, 9)
ren = c(915, 2339, 2189,1989, 2139, 2259, 1359, 1519, 2519, 2299, 2209, 1799, 2269, 1429, 1859, 2559)


plot(x = def, y =ren) # ~lineal... 

recta = recta.reg(x = def, y = ren, plot = T, Summary = T)
# % R**2 ??? ~ 59% puede ser explciado por def.

Fc =(0.5985/(1-0.5985))*(length(def)-2)
qf(p = .95, df1 = 1, df2 = length(def)-2) #significativa

##### E6 #####
### ???

##### E7 #####

datos = read.table(paste(ruta_datos, "Tdiacba.txt", sep = ""), sep = "\t", skip = 1, head = T)  

# la 6 fila es lo que pide el item a ?

Tmed2 = (datos[,4]+datos[,5])/2
plot.ts(Tmed2, col = "blue")
lines(datos[,6], col = "red")

# es la misma serie...

#regresion lineal entre esta Tmedia y tmin o max.
# tiene q ser muy lineal...

#exploratorio:
plot(x = datos[,4], y = datos[,6])


recta =recta.reg(x = datos[,4], y = datos[,6], plot = T, Summary = T)
# significativa.


# practica 2 
ruta_datos = "/home/auri/Facultad/Doc/Materias/MRyA/Practicas/P3/"
system(command = "mkdir P3/SalidasP3")
ruta_salidas = "/home/auri/Facultad/Doc/Materias/MRyA/Practicas/P3/SalidasP3"

source("funciones.R")

## 1

ll = c(2, 9, 9, 29, 41, 60, 57, 17, 10, 1)
nll = c(0, 0, 0, 5, 5, 19, 41, 41, 13, 9)

datos = data.frame(ll = ll, nll = nll)

med = function(x,y){
  res = (x + y)/2
  return(res)
}


med_intervalo  = c(722.375, med(725,729.75), med(730,734.75), med(735,739.75),
                   med(740,744.75), med(745,749.75), med(750,754.75),
                   med(755,759.75), med(760,764.75), med(765,769.75))





biserial.test(tabla = datos, intervalos = med_intervalo, alpha = 0.05)

## 2

niebla = c(2, 4, 5, 2, 1, 1, 0)
n.niebla = c(1, 4, 5, 10, 8, 3, 3)

datos = data.frame(niebla = niebla, n.niebla = n.niebla)

med_intervalo = c(-2, 1, 4, 7, 10, 13, 16) 

biserial.test(tabla = datos, intervalos = med_intervalo, alpha = 0.05)


## 3 # chquear funcion

x = c(685,376)
y = c(158,2068)
tabla.a = data.frame(si = x, no = y)

x = c(572,283)
y = c(600,1832)
tabla.b = data.frame(si = x, no = y)

x = c(834,283)
y = c(338,1832)
tabla.c = data.frame(si = x, no = y)

#a
tetracor_test(tabla = tabla.a, alpha = 0.05)

#b
tetracor_test(tabla = tabla.b, alpha = 0.05)

#c
tetracor_test(tabla = tabla.c, alpha = 0.05)


#### 4 
# maiz
a = c(3,13,16,32)
b = c(5,22,5,32)
c = c(13,17,2,32)
d = c(21,52,23,96)

maiz = data.frame(a, b, c, d)

ctg_test(tabla = maiz, alpha = 0.05, totales = T, show.table = T)


#trigo

a = c(7,16,9,32)
b = c(5,20,9,31)
c = c(9,15,8,32)
d = c(21,51,23,95)

trigo = data.frame(a, b, c, d)
ctg_test(tabla = trigo, alpha = 0.05, totales = T, show.table = T)


## 5 REVISAR
###---> EbntregaE3.Rmd





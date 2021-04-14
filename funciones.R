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
  
  result = c(rb, sigmab, est)
  names(result) = c("Rb", "Sigma b", "Estadistico")
  
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
  
  result = c(rt, se, est)
  names(result) = c("Rt", "Sigma e", "Estadistico")
  
  return(result)
  
  
}


#### tabla de contingencia ####

ctg_test = function(tabla, alpha, totales = FALSE){
  
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
  print(tabla_rand)
  
  
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

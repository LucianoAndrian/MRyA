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
  
  if(!is.data.frame(tabla) & !is.matrix(tabla)){
    stop("tabla debe ser un data.frame")
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
    print(paste("Rechazo H0 con un ", (1-alpha)*100, "% de confianza", sep = ""))
  } else {
    print(paste("NO rechazo H0 con un ", (1-alpha)*100, "% de confianza", sep = ""))
  }
  
  result = c(rb, sigmab, est)
  names(result) = c("Rb", "Sigma b", "Estadistico")
  
  return(result)
  
}

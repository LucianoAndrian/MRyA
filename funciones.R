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

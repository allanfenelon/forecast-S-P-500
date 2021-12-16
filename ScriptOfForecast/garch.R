

GARCHF = function(treino, teste, u, n, m, n_m){
  
  #Diferenças do n ao seu Sn(n)=sucessor
  treinoG = diff(treino); testeG = diff(u)
  
  model.garch=garch(x=treinoG, order=c(0,1), control=garch.control(maxiter= 5000))
  prev.garch=predict(object= model.garch,newdata= c(treinoG[(n-2):(n-1)], testeG),genuine= FALSE)  
  
  y = model.garch$fitted.values[,1]
  xC = numeric(n)
  for(t in 2:n){
    xC[t] = y[t] + u[t-1]
  }
  
  xC[1] = y[1]
  xC[n] = y[n-1] + u[t]
  #uC = exp(xC)
  z = prev.garch[3:(m+2),1]
  
  xxC = numeric(m-1)
  xj = log(u)[(n+1):n_m]
  
  for(t in 2:m){
    xxC[t] = z[t] + xj[t-1]
  }
  
  xxC[1] = z[1] + xj[t-1]
  #uuC = exp(xxC)
  
  garch.forecast = c(xC, xxC)
  return(garch.forecast)
}

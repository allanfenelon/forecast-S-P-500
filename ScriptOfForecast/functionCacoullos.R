Ccacoullos = function(u, X1, X2){
  e1 = u-X1
  e2 = u-X2
  cacoullos.forecast = c()
  
  #Média e desvio padrão dos erros
  mean_E1 = mean(e1, na.rm = TRUE); sd_E1 = sd(e1, na.rm =TRUE)
  mean_E2 = mean(e2, na.rm =TRUE); sd_E2 = sd(e2, na.rm = TRUE)
  U_cand = seq(from = min(cbind(u, X1, X2)), to = max(cbind(u, X1, X2)), length.out = 150)
  g1 = dnorm(e1, mean_E1, sd_E1, log = TRUE)
  g2 = dnorm(e2, mean_E2, sd_E2, log=  TRUE)
  for(i in 1:length(u)){
    #i=1
    #doparallel
    #cacoullos.forecast = foreach(i=1:length(U))%dopar%{
    
    arima_prev = X1[i]
    garch_prev = X2[i]
    
    EUcand1 = U_cand-arima_prev
    EUcand2 = U_cand-garch_prev
    #Fe1 = pnorm(EUcand1, mean_E1, sd_E1, log = TRUE); Fe2 = pnorm(EUcand2, mean_E2, sd_E1, log = TRUE)
    #Fi = cbind(Fe1, Fe2) 
    v1 = pnorm(EUcand1,mean_E1, sd_E1)
    v2 = pnorm(EUcand2,mean_E2, sd_E2)
    Vi = cbind(v1, v2)
    
    #summary(Vi)
    prevb = c()
    for(j in 1:length(U_cand)){
      #j=1
      prevb[j] = dcacoullos(Vi[j,], Vi, log= TRUE)+g1[i]+g2[i]
    }
    
    maxIndex = which.max(prevb)
    cacoullos.forecast[i] = U_cand[maxIndex]
    #return(U_cand[maxIndex])
  }
  return(cacoullos.forecast)
}
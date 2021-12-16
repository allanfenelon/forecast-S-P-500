rmse = function(original, predito){
  diferenca = (original-predito)
  rmse = sqrt(mean(diferenca^2, na.rm = TRUE))
  return(rmse)
}

mae = function(original, predito){
  diferenca = abs(original-predito)
  mae = mean(diferenca, na.rm = TRUE)
  return(mae)
}

desemMAE = function(U, previsoes, N=1, N_M){
  metricaMAE = c()
  qtd = ncol(previsoes)
  for(i in 1:qtd){
    metricaMAE[i]= mae(U[N:N_M], previsoes[(N:N_M), i])
  }
  return(metricaMAE = cbind(metricaMAE[1],
                            metricaMAE[2],
                            metricaMAE[3],
                            metricaMAE[4],
                            metricaMAE[5]))
}

desemRMSE = function(U, previsoes, N=1, N_M){
  metricaRMSE = c()
  qtd = ncol(previsoes)
  for(i in 1:qtd){
    metricaRMSE[i]= rmse(U[N:N_M], previsoes[(N:N_M), i])
  }
  return(metricaRMSE = cbind(metricaRMSE[1],
                            metricaRMSE[2],
                            metricaRMSE[3],
                            metricaRMSE[4],
                            metricaRMSE[5]))
}

 DESEMPENHOtotal = function(u, previsoes, type){
   n_m_desem = as.numeric(nrow(previsoes))
   if(type == "MAE"){
     MAEtreino = desemMAE(u, previsoes, N=1, N_M = n)
     MAEteste = desemMAE(u, previsoes, N=(n+1), N_M = n_m_desem)
     return(MAEtotal = data.frame(MAEtreino, MAEteste))
     
   }else if(type == "RMSE"){
     RMSEtreino = desemRMSE(u, previsoes, N=1, N_M = n)
     RMSEteste = desemRMSE(u, previsoes, N=(n+1), N_M = n_m_desem)
     return(RMSEtotal = data.frame(RMSEtreino, RMSEteste))
   }else{
     print("TIPO INVÁLIDO!")
   }
   
 }
IMPRIMIRdesem = function(DATEmae, DATErmse){
  DATAdesem_og = data.frame(ARIMA = c(DATEmae$X1, DATEmae$X1.1, DATErmse$X1, DATErmse$X1.1),
                         GARCH = c(DATEmae$X2, DATEmae$X2.1, DATErmse$X2, DATErmse$X2.1),
                         MEDIA = c(DATEmae$X3, DATEmae$X3.1, DATErmse$X3, DATErmse$X3.1),
                         MEDIANA = c(DATEmae$X4, DATEmae$X4.1, DATErmse$X4, DATErmse$X4.1),
                         MV = c(DATEmae$X5, DATEmae$X5.1, DATErmse$X5, DATErmse$X5.1))
  DATAdesem = cbind(" " = c("MAE treino","MAE teste", "RMSE treinamento", "RMSE teste"), DATAdesem_og)
  
}



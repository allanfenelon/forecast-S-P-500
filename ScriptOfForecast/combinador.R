meanANDmedian = function(forecasts, type){
  if(type == "mean" || type == "median" || type == "sum"){
    apply(forecasts, MARGIN = 1, type, na.rm = TRUE )
  }else{
    print("Erro, tipo nao suportado!")
  }
}

getMinimalVariancePredictor = function(residuals, forecasts, from, to){
  print(paste("*************", "MINIMAL VARIANCE COMBINATION", "*************"))
  modellingTime = proc.time()[[3]]
  nSingleModels = ncol(forecasts)
  covMatrix=cov(residuals[from:to, ], use = "pairwise.complete.obs")
  invCovMatrix = tryCatch({
    expr = inv(covMatrix)}, 
    error = function(e){
      message("Error: inv(covMatrix). Approaching via ginv(covMatrix)"); 
      return(ginv(covMatrix))
    }
    #,finally={message(paste("Error: ginv(covMatrix)")); return(NA)}
  )
  #print(covMatrix%*%invCovMatrix)
  weigths = numeric(nSingleModels)
  for(i in 1:nSingleModels){
    weigths[i] = sum(invCovMatrix[i,])
  }
  sw=sum(weigths)
  weigths = weigths/sw
  modellingTime = proc.time()[[3]] - modellingTime#time in seconds
  forecastingTime = proc.time()[[3]]
  mvForecasts = weigths[1]*forecasts[,1]
  for(i in 2:nSingleModels){
    mvForecasts = mvForecasts + weigths[i]*forecasts[,i]
  }
  forecastingTime = proc.time()[[3]] - forecastingTime
  modellingTime = as.numeric(modellingTime)
  forecastingTime = as.numeric(forecastingTime)
  weigths = as.list(weigths)
  names(weigths) = colnames(forecasts)
  optimal = list()
  optimal$modellingTime = modellingTime
  optimal$forecastingTime = forecastingTime
  optimal$forecasts = mvForecasts
  optimal$weigths = weigths
  return(optimal)
}

combMV = function(forecasts, residuos){
  linhas = nrow(residuos)
  minimal.variance = getMinimalVariancePredictor(residuos, forecasts, 1, linhas)
  pesos = minimal.variance$weigths
  
  forecastW = data.frame(forecasts$X1*pesos$X1, forecasts$X2*pesos$X2)
  mean.minimalvariance = meanANDmedian(forecastW, type = "sum")
  
  return(mv.forecast = mean.minimalvariance)
  
  
}
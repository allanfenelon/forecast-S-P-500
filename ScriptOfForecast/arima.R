
ARIMAF = function(treino, teste){
  model.arima = auto.arima(treino, ic = "bic", trace = T, allowdrift = FALSE)
  prev.arima = Arima(teste, model = model.arima)
  ajustado.arima = fitted(prev.arima)
  arima.forecast = c(model.arima$fitted, ajustado.arima)
  return(arima.forecast)
}
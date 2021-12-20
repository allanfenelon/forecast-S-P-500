##############################
#   ALLAN PEREIRA FENELON   #
#############################
#install.packages("package")
library("forecast");library("tseries");library("poLCA")
library("copula");library("nortest");library("rmutil")
library("doParallel")

source('./ScriptOfForecast/Cacoullos.R', encoding = 'UTF-8')
source('./ScriptOfForecast/Auxiliar.R', encoding = 'UTF-8')
source('./ScriptOfForecast/arima.R', encoding = 'UTF-8')
source('./ScriptOfForecast/garch.R', encoding = 'UTF-8')
source('./ScriptOfForecast/combinador.R', encoding = 'UTF-8')
source('./ScriptOfForecast/desempenho.R', encoding = 'UTF-8')
source('./ScriptOfForecast/parallelCacoullos.R', encoding = 'UTF-8')
#source('./functionCacoullos.R', encoding = 'UTF-8')

dados = read.csv(file = "./db/SP500.csv", header = TRUE, sep = ";")

#Série temporal em escala log
target = dados$target
u = log(dados$target)
#Dimensões da Série temporal
n_m = length(u); n = round(.8*n_m); m = n_m - n
#Seprarando a série de treino e teste
u_train = u[1:n]; u_test = u[(n+1):n_m]
x = u_train;u_trainG = diff(x)
y = c(u_train[n], u_test); u_testG = diff(y)


#Forecast of model Arima
arima.forecast = ARIMAF(u_train, u_test)

#Forecast of model Garch
#garch.forecast = GARCHF(u_train, u_test, u, n, m, n_m) -> precisa de correção
model.garch = garch(x = u_trainG, order = c(0,1), 
                    control = garch.control(maxiter = 5000))
prev.garch = predict(object = model.garch,newdata = c(u_trainG[(n-2):(n-1)], u_testG), genuine = FALSE)  
y = model.garch$fitted.values[,1]
xC = numeric(n)
for(t in 2:n){
  xC[t] = y[t] + x[t-1]
}
xC[1] = y[1]
xC[n] = y[n-1] + x[t]
#uC = exp(xC)
z = prev.garch[3:(m+2),1]
xxC = numeric(m-1)
xj = log(target)[(n+1):n_m]
for(t in 2:m){
  xxC[t] = z[t] + xj[t-1]
}
xxC[1] = z[1] + xj[t-1]
#uuC = exp(xxC)
garch.forecast = c(xC, xxC)###########################

#Ajustes da série temporal
X1 = arima.forecast[-1]
X2 = garch.forecast[-1]
u = u[-1]

forecasts = data.frame(X1, X2)

#Forecast comb/ of Mean
mean.forecast = meanANDmedian(forecasts, type = "mean")

#Forecast comb/ of Median
median.forecast = meanANDmedian(forecasts, type = "median")

#Forecast comb/ of MINIMAL VARIANCE
residuos = u - forecasts
mv.forecast = combMV(forecasts, residuos)


#Forecast comb/ of Copula de Cacoullos
#cacoullos.forecast = Ccacoullos(u, X1, X2)
Ucand = seq(from = min(cbind(u, X1, X2)), to = max(cbind(u, X1, X2)), length.out = 900)
e1=X1-u;e2=X2-u #ERROS SÉRIE TEMPORAL
#DENSIDADE DE CADA ERRO
g1 = dnorm(e1, mean(e1), sd(e1), log = TRUE)
g2 = dnorm(e2, mean(e2), sd(e2), log=  TRUE)
copula = c()
cacoullos.forecast = c()
#COMPUTACAO PARALELA (test)
cluster = getParalellComputingConfiguration(3)
registerDoParallel(cluster)
modelosIndv = cbind(X1, X2)
cacoullos.forecast = foreach(t=1:length(u))%dopar%{
#for(t in 1:length(u)){
  Ec1=Ucand-X1[t];Ec2=Ucand-X2[t]
  v1 = pnorm(Ec1, mean(e1), sd(e1))
  v2 = pnorm(Ec2, mean(e2), sd(e2))
  vi = cbind(v1, v2)
  #foreach(k=1:length(Ucand))%dopar%{
  #for(k in 1:length(Ucand)){
  cacoullos=apply(cbind(v1[1:length(Ucand)],v2[1:length(Ucand)]), 1, dcacoullos, vi, log = TRUE)+g1[t]+g2[t]
    #dcacoullos(vi[k,], vi, log = TRUE)+g1[t]+g2[t]
    #}
  maxIndex = which.max(cacoullos)
  return(Ucand[maxIndex])
}
cacoullos.forecast = do.call(rbind.data.frame, cacoullos.forecast)

#ERRO AQUI cacoullosTest = Pcacoullos(u, X1, X2, type = "treino")



#PLOT
ts.plot(cbind(u, X1, X2, cacoullos.forecast), col = c("black","red","blue","green"))
abline(v = n)

#Métricas de desempenho
previsoes = cbind(ARIMA = X1, GARCH = X2, MÉDIA = mean.forecast, 
                  MEDIANA = median.forecast, MV = mv.forecast,
                  CACOULLOS = cacoullos.forecast)
MAEtotal = DESEMPENHOtotal(u, previsoes, type = "MAE")
RMSEtotal = DESEMPENHOtotal(u, previsoes, type = "RMSE")
desempenho = IMPRIMIRdesem(MAEtotal, RMSEtotal)

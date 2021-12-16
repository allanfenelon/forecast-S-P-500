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
#source('./functionCacoullos.R', encoding = 'UTF-8')

dados = read.csv(file = "./db/SP500.csv", header = TRUE, sep = ";")

#Série temporal em escala log
u = log(dados$target)
#Dimensões da Série temporal
n_m = length(u); n = round(.8*n_m); m = n_m - n
#Seprarando a série de treino e teste
u_train = u[1:n]; u_test = u[(n+1):n_m]; 

#Forecast of model Arima
arima.forecast = ARIMAF(u_train, u_test)

#Forecast of model Garch
garch.forecast = GARCHF(u_train, u_test, u, n, m, n_m)

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

previsoes = cbind(ARIMA = X1, GARCH = X2, MÉDIA = mean.forecast, 
                       MEDIANA = median.forecast, MV = mv.forecast)


#Métricas de desempenho

MAEtotal = DESEMPENHOtotal(u, previsoes, type = "MAE")
RMSEtotal = DESEMPENHOtotal(u, previsoes, type = "RMSE")
desempenho = IMPRIMIRdesem(MAEtotal, RMSEtotal)

getS = function(e, sampled_e){#e is a single point with k dimmensions
  n = nrow(sampled_e); k = ncol(sampled_e)
  S = numeric(n)
  for (t in 1:n){
    S[t] = sum((e - sampled_e[t,])^2, na.rm=TRUE)
  }
  return(S)
}
dcacoullos = function(e, sampled_e, lambda=1, log = TRUE){#e is a single point with k dimmensions
  # if(class(e)!="matrix"){e = matrix(e, nrow = 1)}
  n = nrow(sampled_e); k = ncol(sampled_e)
  ret = -(k/2)*log(2*pi) - k*log(lambda) - log(n) 
  S = getS(e, sampled_e)
  ret = ret +     log(sum(exp(-S/(2*lambda^2))))
  if (log==FALSE){ret = exp(ret)}
  return(ret)
}

Pcacoullos = function(u, X1, X2, type){
  n_m = length(u);m=1;qtd = round(.90*n_m)
  if(type == "treino"){
    n = round(.8*n_m)
  }else if(type == "teste"){
    n = n_m
  }else{
    message("TIPO ERRADO")
  }
  Ucand = seq(from = min(cbind(u[m:n], X1[m:n], X2[m:n])), to = max(cbind(u[m:n], X1[m:n], X2[m:n])), length.out = qtd)
  e1=X1[m:n]-u[m:n];e2=X2[m:n]-u[m:n] #ERROS SÉRIE TEMPORAL
  #DENSIDADE DE CADA ERRO
  g1 = dnorm(e1, mean(e1), sd(e1), log = TRUE)
  g2 = dnorm(e2, mean(e2), sd(e2), log=  TRUE)
  
  copula = c()
  cacoullos.forecast = c()
  "for(i in 1:length(u)){
    Ec1=X1[i]-Ucand;Ec2=X2[i]-Ucand
    
    v1 = pnorm(Ec1, mean(e1), sd(e1))
    v2 = pnorm(Ec2, mean(e2), sd(e2))
    for(t in 1:length(Ucand)){
      copula[t] = dcacoullos(cbind(v1[t],v2[t]), cbind(v1,v2), log = TRUE)+g1[i]+g2[i]
    }
    maxIndex = which.max(copula)
    cacoullos.forecast[i] = Ucand[maxIndex]
  }"
  #COMPUTACAO PARALELA (test)
  cluster = getParalellComputingConfiguration(3)
  registerDoParallel(cluster)
  modelosIndv = cbind(X1, X2)
  cacoullos.forecast = foreach(t=m:n)%dopar%{
    #for(t in 1:length(u)){
    Ec1=Ucand-X1[t];Ec2=Ucand-X2[t]
    v1 = pnorm(Ec1, mean(e1), sd(e1))
    v2 = pnorm(Ec2, mean(e2), sd(e2))
    #foreach(k=1:length(u))%dopar%{
    #for(t in 1:length(Ucand)){
    cacoullos=apply(cbind(v1[1:length(Ucand)],v2[1:length(Ucand)]), 1, dcacoullos, vi, log = TRUE)+g1[t]+g2[t]
    #}
    maxIndex = which.max(cacoullos)
    return(Ucand[maxIndex])
  }
  return(cacoullos.forecast = do.call(rbind.data.frame, cacoullos.forecast))
}


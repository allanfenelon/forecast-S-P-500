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

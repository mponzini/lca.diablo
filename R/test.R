dummy <- function(R,respvars){
  n = length(respvars)
  R_mean = matrix(0,nrow=n,ncol=n,byrow=TRUE,dimnames=list(respvars,respvars))
  pair_number = 0
  for (i in 1: (n-1)){
    for (j in (i+1):n){
      pair_number = pair_number +1
      R_mean[i,i] = R_mean[i,i] + R[[pair_number]][1,1]
      R_mean[i,j] = R_mean[i,j] + R[[pair_number]][1,2]
      R_mean[j,i] = R_mean[j,i] + R[[pair_number]][2,1]
      R_mean[j,j] = R_mean[j,j] + R[[pair_number]][2,2]
    }
  }
  diag(R_mean) <- diag(R_mean)/(n-1)
  return (R_mean)
}

#' @export

summarize_D_matrix <- function(D,respvars){
  n = length(respvars)
  D_mean = matrix(0,nrow=n,ncol=n,byrow=TRUE,dimnames=list(respvars,respvars))
  pair_number = 0
  for (i in 1: (n-1)){
    for (j in (i+1):n){
      pair_number = pair_number +1
      D_mean[i,i] = D_mean[i,i] + D[[pair_number]][1,1]
      D_mean[i,j] = D_mean[i,j] + D[[pair_number]][1,2]
      D_mean[j,i] = D_mean[j,i] + D[[pair_number]][2,1]
      D_mean[j,j] = D_mean[j,j] + D[[pair_number]][2,2]
    }
  }
  diag(D_mean) <- diag(D_mean)/(n-1)
  return (D_mean)
}

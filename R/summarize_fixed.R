#' @export

summarize_fixed <- function(Fixedest,respvars,fixed,H,G){
  ## Assign value
  estimate <- vector()
  cov <- vector()

  nsubjects = ncol(G)
  p=nrow(G)
  K=matrix(0,p,p)
  for (i in 1:nsubjects){
    K = K +G[,i]%*%t(G[,i])
  }
  K = K * (1/nsubjects)
  H = H * (1/nsubjects)
  Sigma=solve(H)%*%K%*%solve(H)
  Sigma=Sigma*(1/nsubjects)

  fixed_ = do.call(rbind,Fixedest)

  nrespvars = length(respvars)
  parameters = c("int",fixed)    # holding the names of all parameters
  nparameter = length(fixed)+1   # Number of parameter
  npairs = choose(nrespvars, 2)
  # Combine the fixed effect coefficients
  respvar = vector()

  ## Creating the string for scanning parameter names
  parameter = rep(rep(parameters,each=nrespvars-1),choose(nrespvars, 2))  #

  ## Creating the string for scanning response names
  for (i in 1:(nrespvars-1)){
    for (j in (i+1):nrespvars){
      respvar = c(respvar,rep(c(respvars[i],respvars[j]),dim(Fixedest[[1]])[1]/2))
    }
  }

  ## Now we can calculate the average and create a matrix for coeffcients and standard deviation
  A <- matrix(0, nrow = nrespvars*nparameter, ncol =nparameter*npairs*2)

  for (i in 1:nrespvars){
    for (j in 1:nparameter){
      for (k in 1:(nparameter*npairs*2)){
        A[(((i-1)*nparameter)+j),k] = ifelse(
          (respvars[i] == respvar[k] &  parameters[j] == parameter[k]),
          (1 / (nrespvars - 1)) , 0
        )
      }
    }
  }
  estimate <- A %*%fixed_[,1]
  cov <- A %*% Sigma %*% t(A)
  # Varest <- vardiag::vardiag(cov)
  #estimate <- append(estimate,mean(fixed_ [which (respvar == respvars[i] & parameter == parameters[j]),1]))

  #cov <- append(cov,mean(fixed_ [which (respvar == respvars[i] & parameter == parameters[j]),2]))


  estimate_matrix <- matrix(estimate, ncol = nrespvars, nrow = nparameter)
  colnames(estimate_matrix) <- respvars
  rownames(estimate_matrix) <- parameters

  return(estimate_matrix)
}

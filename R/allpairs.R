#' @export

allpairs <- function(data, respvars, fixed, randomno1, randomno2, timevar, time4mc, id, catvar){
  data.table::setorderv(data, id) # giving an id to the data in case we don't have id for the data
  pairno = 0
  nrespvars = length(respvars)

  # Creating empty datasets which will be used later
  D <- list()
  R <- list()
  pair_model <- list()
  H_ <- list()  # H matrix for each pair
  G_ <- list()  # G matrix for each pair
  V_ <- list()  # vcov matrix for each pairs, each observation
  Y_ <- list()  # y for each pairs, each observation
  MU_ <- list()  # MU for each pairs, each observation
  Fixedest <- list()
  Hessian_out = list(H_,G_,V_,Y_,MU_)

  # The loop to run all pairs
  for (l in 1: (nrespvars-1)){
    for (j in (l+1):nrespvars){
      yl=respvars[l]
      yj=respvars[j]
      rl=randomno2[l]
      rj=randomno2[j]
      pairno = pairno + 1

      cat("------------------------\n","var1=",yl,"var2=",yj,"\n pairno =",pairno,"\n------------------------\n")

      # The parameter will be updated later for pair1 and pair2 - we will pass more arguments into the functions
      if (length(randomno1)>0){
        pair_result <- pair1(rawdata=data,var1=yl, var2=yj, fixed=fixed, rand=randomno1, timevar=timevar, pairno = pairno, id=id,catvar=catvar)

        convstatus = 1  # I am not sure how to get the convstatus from the lmer package
      }
      else if (length(randomno2)>0){
        # pair2(var1=yl, var2=yj, fixed=fixed, rand1=rl, rand2=rj,timevar=timevar, pairno=pairno)
      }

      # convstatus_&pair. is an output from pair1/2,
      # convstatus = convstatus_&pair$reason

      cat(" ",convstatus,"!\n")

      if (convstatus>0){
        Fixedest <- c(Fixedest, list(pair_result$fixed))
        D <- c(D, list(pair_result$D))
        R <- c(R, list(pair_result$R))
        pair_model <- c(pair_model, list(pair_result))

        # this V is variance-covariance matrix for all covairates, we prob not going to use it
        # V <- c(V, vcov (pair_result$model))

        # Calculate Hessian Matrix
        Hessian_out <- hessian_pair(pair_result=pair_result,H_=Hessian_out$H_,G_=Hessian_out$G_,V_=Hessian_out$V_,Y_=Hessian_out$Y_,MU_=Hessian_out$MU_,fixedest=Fixedest[[pairno]][,1],pairn=pairno)

      }
    }
  }
  H = Matrix::bdiag(Hessian_out$H_)
  G = do.call(rbind,Hessian_out$G_)
  V = Hessian_out$V_
  Y = Hessian_out$Y_
  MU = Hessian_out$MU_

  D_mean = summarize_D_matrix(D,respvars)
  R_mean = summarize_R_matrix(R,respvars)
  fix_est_sum = summarize_fixed(Fixedest,respvars,fixed,H=H,G=G)

  return(list(R_mean=R_mean,D_mean=D_mean,R=R,D=D,V=V,Y=Y,MU=MU ,Fixedest=Fixedest,Hessian=H,Gradient = G,Fixedest_mean = fix_est_sum, pair_model = pair_model))
}

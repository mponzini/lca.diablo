# All functions for LCA #
detachAllPackages <- function() {

  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")

  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]

  package.list <- setdiff(package.list,basic.packages)

  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)

}
detachAllPackages()

pacman::p_load("dplyr", "tidyr", "lme4", "lmerTest")
# updated functions while attempting to run Metformin data
# changed which arguments are required for each function

# Helper functions #
# onepair
pair1 <- function(rawdata,var1, var2, fixed, rand,timevar,pairno,id,catvar){
  nop = length(fixed)

  pair_temp <- rawdata %>%
    mutate(Resp1 = ifelse(is.na(!!sym(var1))==F, as.numeric(!!sym(var1)),NA),
           Resp2 = ifelse(is.na(!!sym(var2))==F, as.numeric(!!sym(var2)),NA))%>%
    dplyr::select(matches(c(id,timevar,fixed)),Resp1, Resp2)

  # pivot to long format, handling outcome groups
  out_temp <- pair_temp %>% pivot_longer(Resp1:Resp2,
                                         names_to = "NAME",
                                         values_to = "Score",
                                         values_drop_na = FALSE) %>%
    mutate(outcome_num = as.numeric(substr(NAME,5,5)),
           int1 = ifelse(as.numeric(substr(NAME,5,5))==1,1,0),
           int2 = ifelse(as.numeric(substr(NAME,5,5))==2,1,0))

  # Handling Fixed Parameters
  fixed_var = vector()
  for (i in 1:nop){
    if (fixed[i]%in%catvar == F)
      out_temp <- out_temp %>% mutate(!!sym(paste(fixed[i],1,"_",sep="")) := ifelse(outcome_num==1,!!sym(paste(fixed[i])),0),
                                      !!sym(paste(fixed[i],2,"_",sep="")) := ifelse(outcome_num==2,!!sym(paste(fixed[i])),0))
    else{
      out_temp <- out_temp %>% mutate(!!sym(paste(fixed[i],1,"_",sep="")) := relevel(as.factor(ifelse(outcome_num==1,!!sym(paste(fixed[i])),0)),ref="1"),
                                      !!sym(paste(fixed[i],2,"_",sep="")) := relevel(as.factor(ifelse(outcome_num==2,!!sym(paste(fixed[i])),0)),ref="0"))

    }
    fixed_var[2*i-1] = paste(fixed[i],1,"_",sep="")
    fixed_var[2*i] = paste(fixed[i],2,"_",sep="")
  }

  out_temp$visit = as.factor(out_temp$visit)
  out_temp$outcome_num = as.factor(out_temp$outcome_num)
  # Create the formula for repeated mixed effect model
  formula00 <- as.formula(paste("Score ~ 0+int1 + int2 +",paste(fixed_var, collapse = " + "),"+", "(0+int1+int2","|",id,") + (0+outcome_num|",timevar,":",id,")"))

  # formula10 <- as.formula(paste("Score ~ 0+int1 + int2 +",paste(fixed_var, collapse = " + "),"+", "(0+int1+int2","+",paste(fixed[1],1,"_",sep=""),"|",id,") + (0+outcome_num|",timevar,":",id,")"))
  # formula01 <- as.formula(paste("Score ~ 0+int1 + int2 +",paste(fixed_var, collapse = " + "),"+", "(0+int1+int2","+",paste(fixed[1],2,"_",sep=""),"|",id,") + (0+outcome_num|",timevar,":",id,")"))
  # formula11 <- as.formula(paste("Score ~ 0+ int1 + int2 +",paste(fixed_var, collapse = " + "),"+", "(0+int1+int2","+",paste(fixed[1],1,"_",sep=""),"+",paste(fixed[1],2,"_",sep=""),"|",id,") + (0+outcome_num|",timevar,":",id,")"))
  ## issue with converge

  # intercept + intercept -- only consider the model with random intercept only
  model00 <- suppressMessages(lmerTest::lmer(formula00, data = out_temp,control=lmerControl(check.nobs.vs.nRE="ignore")))
  # slope + intercept
  # model10 <- suppressMessages(lmerTest::lmer(formula10, data = out_temp,control=lmerControl(check.nobs.vs.nRE="ignore")))
  # intercept + slope
  # model01 <- suppressMessages(lmerTest::lmer(formula01, data = out_temp,control=lmerControl(check.nobs.vs.nRE="ignore")))
  # slope + slope
  # model11 <- suppressMessages(lmerTest::lmer(formula11, data = out_temp,control=lmerControl(check.nobs.vs.nRE="ignore")))

  ###     selecting the best model for random part based on AIC
  #####   I am not able to check the convergence status at current stage)   #####
  # AIC_list = c(AIC(model00),AIC(model10),AIC(model01),AIC(model11))

  # if(which(AIC_list == min(AIC_list)) == 1){
  #   best_model = model00
  #   Z = out_temp[,c("int1","int2")]
  # }else{
  #   if(which(AIC_list == min(AIC_list))==2){
  #     best_model = model10
  #     Z = out_temp[,c("int1","int2",paste(fixed[1],1,"_",sep=""))]
  #   }else{
  #     if(which(AIC_list == min(AIC_list))==3){
  #       best_model = model01
  #       Z = out_temp[,c("int1","int2",paste(fixed[1],2,"_",sep=""))]
  #     } else{
  #       best_model = model11
  #       Z = out_temp[,c("int1","int2",paste(fixed[1],1,"_",sep=""),paste(fixed[1],2,"_",sep=""))]
  #     }
  #   }
  # }
  best_model = model00
  Z = out_temp[,c("int1","int2")]

  # generating X matrix for fixed covariates
  X = out_temp[,c("int1","int2",fixed_var)]

  # getting fixed effect
  fixed = coef(summary(best_model))

  # getting D matrix
  D = data.frame(VarCorr(best_model)[[2]])

  # generating R matrix
  R = data.frame(VarCorr(best_model)[[1]])+diag(sigma(best_model)^2,2)


  return(list(fixed=fixed,D=D,R=R,model=model00,out_temp=out_temp,X=X,Z=Z))
}

#########################
#   Average R for each combinations
#########################

summary_R_matrix <- function(R,respvars){
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


#########################
#   Average D for each combinations
#########################

summary_D_matrix <- function(D,respvars){
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
#########################
#   Summaryfixed
#########################

Summaryfixed <- function(Fixedest,respvars,fixed,H,G){
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

#########################
#   Hessian function calculation
#                     (Return Hessian, Gradiant Matrix, and pairwised yi, mu_i, Vi)
#########################

Hessian_pair <- function(pair_result,H_,G_,V_,Y_,MU_,fixedest,pairn){
  D = pair_result$D
  R = pair_result$R
  y = pair_result$out_temp$Score
  X = pair_result$X %>% mutate_all(as.character)  %>% mutate_all(as.numeric)
  Z = pair_result$Z
  id_list = table(pair_result$out_temp$id)
  # pair_result$model
  resid = resid(pair_result$model)

  # residual from lmer will ignore the missing value, we have to adding them back in to make sure the match of vector length
  redis_miss <- which(!as.character(1:length(y))%in%names(resid))
  fill_na <- rep(NA,length(redis_miss))
  names(fill_na) <-as.character(redis_miss)
  resid <- c(resid(pair_result$model),fill_na)[order(as.numeric(names (c(resid(pair_result$model),fill_na))))]

  # Check for missing observation in Y and X
  # Vector notmissy: contains the list of non-missing observations in Y
  # Vector notmissx: contains the list of non-missing observations in X.
  # (If any of the covariate value is missing - it will be considered as missing in general)
  Hessian = matrix(ncol = ncol(X),nrow = ncol(X),0)
  Gradient = vector()
  V = list()
  Y = list()
  MU = list()
  for (i in 1:length(id_list)){
    if (i == 1)
      pos = 1

    Zi = Z[pos:(pos+id_list[i]-1),]
    Xi = X[pos:(pos+id_list[i]-1),]
    yi = y[pos:(pos+id_list[i]-1)]
    residi = resid[pos:(pos+id_list[i]-1)]
    ni = id_list[i]
    I_ni = diag(ni/2)
    Ri = I_ni%x%as.matrix(R)

    notmiss_index <- which(rowSums(is.na(Xi))==0&is.na(yi)==F)

    if  (length(notmiss_index)>0){
      Xi = Xi[notmiss_index,]
      yi = yi[notmiss_index]
      Zi = Zi[notmiss_index,]
      residi=residi[notmiss_index]
      Ri=Ri[notmiss_index,notmiss_index]
      Vi = as.matrix(Zi)%*%as.matrix(D)%*%t(as.matrix(Zi)) + Ri
      Wi = MASS::ginv(Vi)
      Hessian_i = as.matrix(t(Xi))%*%Wi%*%as.matrix(Xi)
      Hessian = Hessian + Hessian_i
      grad_ik = as.matrix(t(Xi))%*%Wi%*%as.matrix(residi)
      Gradient = cbind(Gradient,grad_ik)
      V = c(V,list(Vi))
      Y = c(Y,list(yi))
      MU = c(MU,list(as.numeric(apply(as.matrix(Xi),c(1,2),as.numeric)%*%as.matrix(fixedest))))
    } else {
      Gradient = cbind(Gradient,rep(0,ncol(X)))
      V = c(V,list(NA))
      Y = c(Y,list(NA))
      MU = c(MU,list(NA))
    }
    pos = pos + id_list[i]
  }

  Y_ = c(Y_, list(Y))
  V_ = c(V_, list(V))
  H_ = c(H_, list(Hessian))
  G_ = c(G_, list(Gradient))
  MU_ = c(MU_, list(MU))

  return(list(H_=H_, G_=G_, V_=V_, Y_=Y_,MU_=MU_))
}

#########################
#   Allpair function - the main function runs by each paired response variable
#
# respvars  = an array of all response variables
# fixed     = an array of fixed variables
# randomno1 = specify 1 if the decision on random slope or intercept need to be made for each pairbased on AIC and convergence criteria
# randomno2 = array of indicator which match to "respvars" variable, "1" if random slope of corresponding response variable is of interest.
# timevar   = the name of time variable
# time4mc   = time for marginal correlation
# pairno    = a recording variable for the current pair #
###################################
#########################
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
        Hessian_out <- Hessian_pair(pair_result=pair_result,H_=Hessian_out$H_,G_=Hessian_out$G_,V_=Hessian_out$V_,Y_=Hessian_out$Y_,MU_=Hessian_out$MU_,fixedest=Fixedest[[pairno]][,1],pairn=pairno)

      }
    }
  }
  H = Matrix::bdiag(Hessian_out$H_)
  G = do.call(rbind,Hessian_out$G_)
  V = Hessian_out$V_
  Y = Hessian_out$Y_
  MU = Hessian_out$MU_

  D_mean = summary_D_matrix(D,respvars)
  R_mean = summary_R_matrix(R,respvars)
  fix_est_sum = Summaryfixed(Fixedest,respvars,fixed,H=H,G=G)

  return(list(R_mean=R_mean,D_mean=D_mean,R=R,D=D,V=V,Y=Y,MU=MU ,Fixedest=Fixedest,Hessian=H,Gradient = G,Fixedest_mean = fix_est_sum, pair_model = pair_model))
}


############################################################
#
# this function has to be used under get_likelihood
# calculate the likelihood for each observation based on pair number
#
############################################################

get_likelihood_i <- function(D_p,R_p,id_list,theta,X,Z,y){

  likelihood_list = vector()   # store the likelihood for each observation into the list
  for (i in 1:length(id_list)){ # i = the ith number observation
    if (i == 1)
      pos = 1

    Zi = Z[pos:(pos+id_list[i]-1),]
    Xi = X[pos:(pos+id_list[i]-1),]
    yi = y[pos:(pos+id_list[i]-1)]
    ni = length(yi)
    notmiss_index <- which(rowSums(is.na(Xi))==0&is.na(yi)==F)

    ni = id_list[1]
    I_ni = diag(ni/2)
    R_p_i = I_ni%x%as.matrix(R_p)

    if  (length(notmiss_index)>0){
      Xi = Xi[notmiss_index,]
      Zi = Zi[notmiss_index,]
      yi = yi[notmiss_index]
      R_p_i=R_p_i[notmiss_index,notmiss_index]

      V_i = as.matrix(Zi)%*%as.matrix(D_p)%*%t(as.matrix(Zi)) + R_p_i
      Mu_i = as.numeric(apply(as.matrix(Xi),c(1,2),as.numeric)%*%as.matrix(theta))

      L_pi = (2*pi)^(-ni/2)*det(V_i)^(-1/2)*exp(-1/2*(yi-Mu_i)%*%solve(V_i)%*%(yi-Mu_i))
    } else {
      V_i = NULL
      Mu_i = NULL
      L_pi = NULL
    }
    pos = pos + id_list[i]

    likelihood_list = append(likelihood_list, L_pi)
  }
  return (likelihood_list)
}


############################################################
# this function return the likelihood table for K-th cluster
#
# respvars_n = # of response variable
# pairedresult = the k-th cluster fitting result
# X, Z, Y, id should be matched for each individual, pass in from the new data
# (Need to replace the model$X etc.)
############################################################
get_likelihood <- function(respvars_n, pairedresult, fixed, XYZ = NULL){
  likelihood_list = vector()    # likelihood table for the kth pairedresult, row as observation i, column as pair rs or p
  for (r in 1:(respvars_n-1)){
    if (r == 1)
      pair_n = 0
    for (s in (r+1):respvars_n){ # pair rs = pair p
      pair_n = pair_n + 1

      D_p = pairedresult$D_mean[c(r,s),c(r,s)]
      R_p = pairedresult$R_mean[c(r,s),c(r,s)]
      theta = as.numeric(t(pairedresult$Fixedest_mean[,c(r,s)]))

      model = pairedresult$pair_model[[pair_n]]
      # get these from full data set
      X = XYZ[[pair_n]][, c("int1", "int2", colnames(XYZ[[pair_n]])[grep(paste(fixed, collapse = "|"), colnames(XYZ[[pair_n]]))])] |>
        dplyr::select(-any_of(fixed))
      Z = XYZ[[pair_n]][, c("int1", "int2")]
      y = XYZ[[pair_n]]$Score
      id = table(XYZ[[pair_n]]$id) # from full data set

      likelihood_list = cbind(likelihood_list,get_likelihood_i(D_p=D_p,R_p=R_p,id_list=id,theta=theta,X=X,Z=Z,y=y))
    }
  }
  return(likelihood_list)
}

###########
# function to pull X, Y, Z for one pair using all data
##########
pair_xyz <- function(data, var1, var2, fixed, rand,timevar,pairno,id,catvar){
  nop = length(fixed)
  pair_temp <- data %>%
    mutate(Resp1 = ifelse(is.na(!!sym(var1))==F, as.numeric(!!sym(var1)),NA),
           Resp2 = ifelse(is.na(!!sym(var2))==F, as.numeric(!!sym(var2)),NA))%>%
    dplyr::select(matches(c(id,timevar,fixed)),Resp1, Resp2)

  # pivot to long format, handling outcome groups
  out_temp <- pair_temp %>% pivot_longer(Resp1:Resp2,
                                         names_to = "NAME",
                                         values_to = "Score",
                                         values_drop_na = FALSE) %>%
    mutate(outcome_num = as.numeric(substr(NAME,5,5)),
           int1 = ifelse(as.numeric(substr(NAME,5,5))==1,1,0),
           int2 = ifelse(as.numeric(substr(NAME,5,5))==2,1,0))


  # Handling Fixed Parameters
  for (i in 1:nop){
    if (fixed[i]%in%catvar == F)
      out_temp <- out_temp %>% mutate(!!sym(paste(fixed[i],1,"_",sep="")) := ifelse(outcome_num==1,!!sym(paste(fixed[i])),0),
                                      !!sym(paste(fixed[i],2,"_",sep="")) := ifelse(outcome_num==2,!!sym(paste(fixed[i])),0))
    else{
      out_temp <- out_temp %>% mutate(!!sym(paste(fixed[i],1,"_",sep="")) := relevel(as.factor(ifelse(outcome_num==1,!!sym(paste(fixed[i])),0)),ref="1"),
                                      !!sym(paste(fixed[i],2,"_",sep="")) := relevel(as.factor(ifelse(outcome_num==2,!!sym(paste(fixed[i])),0)),ref="0"))

    }
  }

  out_temp$visit = as.factor(out_temp$visit)
  out_temp$outcome_num = as.factor(out_temp$outcome_num)


  return(out_temp)
}

#########
# function to get X, Y, Z for ALL pairs using full data set
#########
all_xyz <- function(data, respvars, fixed, randomno1, randomno2, timevar, time4mc, id, catvar){
  data.table::setorderv(data, id) # giving an id to the data in case we don't have id for the data
  pairno = 0
  nrespvars = length(respvars)

  npairs <- ncol(combn(respvars, 2))
  # # Creating empty datasets which will be used later
  # D <- list()
  # R <- list()
  # pair_model <- list()
  # H_ <- list()  # H matrix for each pair
  # G_ <- list()  # G matrix for each pair
  # V_ <- list()  # vcov matrix for each pairs, each observation
  # Y_ <- list()  # y for each pairs, each observation
  # MU_ <- list()  # MU for each pairs, each observation
  # Fixedest <- list()
  # Hessian_out = list(H_,G_,V_,Y_,MU_)
  pair_result <- vector(mode = 'list', length = npairs)
  # The loop to run all pairs
  for (l in 1: (nrespvars-1)){
    for (j in (l+1):nrespvars){
      yl=respvars[l]
      yj=respvars[j]
      rl=randomno2[l]
      rj=randomno2[j]
      pairno = pairno + 1

      # cat("------------------------\n","var1=",yl,"var2=",yj,"\n pairno =",pairno,"\n------------------------\n")

      pair_result[[pairno]] <- pair_xyz(
        data=data,var1=yl, var2=yj, fixed=fixed, rand=randomno1,
        timevar=timevar, pairno = pairno, id=id,catvar=catvar
      )



      # convstatus_&pair. is an output from pair1/2,
      # convstatus = convstatus_&pair$reason

      # cat(" ",convstatus,"!\n")
      #
      # if (convstatus>0){
      #   Fixedest <- c(Fixedest, list(pair_result$fixed))
      #   D <- c(D, list(pair_result$D))
      #   R <- c(R, list(pair_result$R))
      #   pair_model <- c(pair_model, list(pair_result))
      #
      #   # this V is variance-covariance matrix for all covairates, we prob not going to use it
      #   # V <- c(V, vcov (pair_result$model))
      #
      #   # Calculate Hessian Matrix
      #   Hessian_out <- Hessian_pair(pair_result=pair_result,H_=Hessian_out$H_,G_=Hessian_out$G_,V_=Hessian_out$V_,Y_=Hessian_out$Y_,MU_=Hessian_out$MU_,fixedest=Fixedest[[pairno]][,1],pairn=pairno)
      #
      # }
    }
  }
  # H = Matrix::bdiag(Hessian_out$H_)
  # G = do.call(rbind,Hessian_out$G_)
  # V = Hessian_out$V_
  # Y = Hessian_out$Y_
  # MU = Hessian_out$MU_
  #
  # D_mean = summary_D_matrix(D,respvars)
  # R_mean = summary_R_matrix(R,respvars)
  # fix_est_sum = Summaryfixed(Fixedest,respvars,fixed,H=H,G=G)

  return(pair_result)
}


# function to run LCA
lca <- function(data, k, respvars, fixed, randomno1, randomno2, timevar, time4mc, id, catvar){
  nrespvars = length(respvars)
  # Get data frame of X, Y, and Z for all pairs
  data_xyz <- all_xyz(data = data, respvars = respvars, fixed = fixed,
                      randomno1 = randomno1, randomno2 = randomno2,
                      timevar = timevar, time4mc = time4mc, id = id,
                      catvar = catvar)
  # Initial cluster assignment.
  cluster_data <- data.frame(
    id = unique(data$id),
    cluster = sample(1:k, length(unique(data$id)), replace = TRUE)
  )
  # merge cluster assignment to data
  data <- merge(data, cluster_data, by = "id")

  # set check = 1 to enter repeat
  check <- 1

  repeat{
    if(check == 0){
      break
    }
    # split data by cluster, need to repeat as clusters change
    data_split <- split(data, data$cluster)

    # Fit all pairwise models for each cluster.
    # <see Yunyi's code>
    k_results <- lapply(data_split, function(x)
      allpairs(data = x, respvars = respvars, fixed = fixed,
               randomno1 = randomno1, randomno2 = randomno2, timevar = timevar,
               time4mc = time4mc, id = id, catvar = catvar))

    # wrapper to process results for each cluster?
    likelihood <- lapply(
      k_results,
      function(x) get_likelihood(respvars_n = nrespvars,
                                 pairedresult = x,
                                 fixed = fixed,
                                 XYZ = data_xyz)
    )

    # get sum of log likelihoods for each cluster
    sum_ll <- as.data.frame(sapply(1:k, function(x) rowSums(log(likelihood[[x]]))))
    # assign cluster to rowwise max
    clust_new <- as.numeric(gsub("V", "", colnames(sum_ll)[max.col(sum_ll)]))

    cluster_new <- data.frame(
      id = unique(data$id),
      new_cluster = clust_new
    )

    # make id variable characters in both data and cluster_new for join to work
    data$id <- as.character(data$id)
    cluster_new$id <- as.character(cluster_new$id)

    # assign new cluster -> merge data with previous result?
    # contrains cluster assignment and sum(loglik)
    data <- dplyr::left_join(data, cluster_new, by = 'id')
    # check if at least one obs changed cluster
    check <- sum(abs(data$cluster - data$new_cluster), na.rm = TRUE)
    # update cluster with new_cluster
    data$cluster <- data$new_cluster
    # remove new_cluster
    data <- data[, !names(data) %in% "new_cluster", drop = FALSE]
    # remove sum(loglik) if we need to iterate, otherwise we want to keep final
    # sum(loglik)
    # if(check != 0){
    #   data <- data[, !names(data) %in% "loglik", drop = FALSE]
    # }
  }

  # return:
  #   data with cluster assignment and sum(loglik)
  #   model results?
  results <- list(
    "Data" = data,
    "Models" = k_results,
    "Log.Likelihoods" = lapply(likelihood, function(x) log(x)),
    "Sum.Log.Lik" = sum_ll
  )
  return(results)

}

# wrapper to run LCA b times to find the best iteration
lca_iterations <- function(data, k, b = 30, respvars, fixed, randomno1, randomno2, timevar, time4mc, id, catvar){
  # repeat lca to minimize impact of initial cluster assignment
  iterations <- vector(mode = "list", length = b)
  for(i in 1:b){
    iterations[[i]] <- lca(data, k, respvars, fixed, randomno1, randomno2, timevar, time4mc, id, catvar)
  }
  # select iteration with highest value for the pseudo-likelihood #
  # get the individual pseudo-likelihood for the assigned cluster
  iterations_indiv_likelihoods <- lapply(
    iterations,
    function(x) x$Sum.Log.Lik |>
      tibble::rownames_to_column("temp.id") |>
      mutate(temp.id = as.numeric(temp.id)) |>
      pivot_longer(cols = contains("V"),
                   names_to = "cluster",
                   values_to = "loglik") |>
      group_by(temp.id) |>
      slice(which.max(loglik))
  )
  # get the pseudo-likelihood for each iteration
  iterations_likelihoods <- data.frame(
    Iteration = seq(1, b, 1),
    Pseudo.Likelihood = sapply(
      1:b,
      function(x) sum(iterations_indiv_likelihoods[[x]]$loglik)
    )
  )
  # get iteration # with highest pseudo-likelihood
  best_iteration <- iterations_likelihoods |>
    filter(Pseudo.Likelihood == max(Pseudo.Likelihood)) |>
    dplyr::select(Iteration) |>
    unlist() |>
    unname()
  # check if all iterations are the same
  iteration_check <- ifelse(length(best_iteration) > 1, 1, 0)
  # select the best iteration from list of iterations
  if(iteration_check == 1){
    # if all iterations are the same, return the first iteration
    result <- iterations[[best_iteration[1]]]
  } else{
    # return the best iteration
    result <- iterations[[best_iteration]]
  }


  # return best result
  return(result)

}

# calculate AIC and BIC
BruckerAIC <- function(test_lca_iterations){
  # Number of Classes
  k = ncol(test_lca_iterations$Sum.Log.Lik)

  tb <-  test_lca_iterations$Sum.Log.Lik
  # pl likelihood
  pl = test_lca_iterations$Sum.Log.Lik %>% apply(1, max, na.rm=TRUE) %>% sum()

  # calculate the trace
  trace_theta = 0
  for (i in 1:k){
    Hessian <- test_lca_iterations$Models[[i]]$Hessian
    Gradient <- test_lca_iterations$Models[[i]]$Gradient

    N = ncol(Gradient)
    H = Hessian/N   # J in the sas code - average hessian matrix
    J = 1/N*Gradient%*%t(Gradient)  # K in the sas code
    G = H%*%solve(J)%*%H
    trace_theta = trace_theta + sum(diag(H%*%solve(G)))
  }

  AIC = -2 * pl + 2 * trace_theta
  BIC = -2 * pl + log(N) * trace_theta
  return(cbind(AIC,BIC))
}

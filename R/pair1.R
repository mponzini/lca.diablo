#' @export

pair1 <- function(
    rawdata,
    var1,
    var2,
    fixed,
    rand,
    timevar,
    pairno,
    id,
    catvar
){
  nop = length(fixed)

  pair_temp <- rawdata |>
    dplyr::mutate(
      Resp1 = ifelse(is.na(.data[[var1]])==F, as.numeric(.data[[var1]]),NA),
      Resp2 = ifelse(is.na(.data[[var2]])==F, as.numeric(.data[[var2]]),NA)
    ) |>
    dplyr::select(
      dplyr::all_of(id, timevar, fixed), Resp1, Resp2
    )

  # pivot to long format, handling outcome groups
  out_temp <- pair_temp |>
    tidyr::pivot_longer(
      Resp1:Resp2,
      names_to = "NAME",
      values_to = "Score",
      values_drop_na = FALSE
    ) |>
    dplyr::mutate(
      outcome_num = as.numeric(substr(NAME, 5, 5)),
      int1 = ifelse(as.numeric(substr(NAME, 5, 5)) == 1, 1, 0),
      int2 = ifelse(as.numeric(substr(NAME, 5, 5)) == 2, 1, 0)
    )

  # Handling Fixed Parameters
  fixed_var = vector()
  for (i in 1:nop){
    if (fixed[i] %in% catvar == F)
      out_temp <- out_temp |>
        dplyr::mutate(
          !!sym(paste(fixed[i], 1, "_", sep="")) :=
            ifelse(outcome_num == 1, .data[[fixed[i]]], 0),
          !!sym(paste(fixed[i], 2, "_", sep="")) :=
            ifelse(outcome_num == 2, .data[[fixed[i]]], 0))
    else{
      out_temp <- out_temp |>
        dplyr::mutate(
          !!sym(paste(fixed[i],1,"_",sep="")) :=
            relevel(
              as.factor(ifelse(outcome_num == 1, .data[[fixed[i]]], 0)), ref="1"
            ),
          !!sym(paste(fixed[i],2,"_",sep="")) :=
            relevel(
              as.factor(ifelse(outcome_num == 2, .data[[fixed[i]]], 0)), ref="0"
            )
        )

    }

    fixed_var[2 * i - 1] = paste(fixed[i], 1, "_", sep="")
    fixed_var[ 2* i] = paste(fixed[i], 2, "_", sep="")
  }

  out_temp$visit = as.factor(out_temp$visit)
  out_temp$outcome_num = as.factor(out_temp$outcome_num)
  # Create the formula for repeated mixed effect model
  formula00 <- paste(
    "Score ~ 0+int1 + int2 +",
    paste(fixed_var, collapse = " + "),
    "+", "(0+int1+int2", "|", id, ") + (0+outcome_num|", timevar, ":", id, ")"
  ) |>
    as.formula()

  # intercept + intercept -- only consider the model with random intercept only
  model00 <- suppressMessages(
    lmerTest::lmer(
      formula00,
      data = out_temp,
      control=lmerControl(check.nobs.vs.nRE = "ignore")
    )
  )

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
  Z = out_temp[,c("int1", "int2")]

  # generating X matrix for fixed covariates
  X = out_temp[,c("int1", "int2", fixed_var)]

  # getting fixed effect
  fixed = coef(summary(best_model))

  # getting D matrix
  D = data.frame(VarCorr(best_model)[[2]])

  # generating R matrix
  R = data.frame(VarCorr(best_model)[[1]]) + diag(sigma(best_model) ^ 2, 2)

  ans <- list(
    fixed = fixed,
    D = D,
    R = R,
    model = model00,
    out_temp = out_temp,
    X = X,
    Z = Z
  )

  return(ans)
}

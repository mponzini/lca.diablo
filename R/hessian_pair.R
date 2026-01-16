#' @export

hessian_pair <- function(pair_result, H_, G_, V_, Y_, MU_, fixedest, pairn) {
  D <- pair_result$D
  R <- pair_result$R
  y <- pair_result$out_temp$Score
  X <- pair_result$X |>
    mutate_all(as.character) |>
    mutate_all(as.numeric)
  Z <- pair_result$Z
  id_list <- table(pair_result$out_temp$id)
  # pair_result$model
  resid <- resid(pair_result$model)

  # residual from lmer will ignore the missing value,
  # we have to adding them back in to make sure the match of vector length
  redis_miss <- which(!as.character(1:length(y)) %in% names(resid))
  fill_na <- rep(NA, length(redis_miss))
  names(fill_na) <- as.character(redis_miss)
  resid <- c(
    resid(pair_result$model),
    fill_na
  )[order(as.numeric(names(c(resid(pair_result$model), fill_na))))]

  # Check for missing observation in Y and X
  # Vector notmissy: contains the list of non-missing observations in Y
  # Vector notmissx: contains the list of non-missing observations in X.
  # (If any of the covariate value is missing - it will be considered as
  # missing in general)
  Hessian <- matrix(ncol = ncol(X), nrow = ncol(X), 0)
  Gradient <- vector()
  V <- list()
  Y <- list()
  MU <- list()
  for (i in 1:length(id_list)) {
    if (i == 1)
      pos <- 1

    Zi <- Z[pos:(pos + id_list[i] - 1), ]
    Xi <- X[pos:(pos + id_list[i] - 1), ]
    yi <- y[pos:(pos + id_list[i] - 1)]
    residi <- resid[pos:(pos + id_list[i] - 1)]
    ni <- id_list[i]
    I_ni <- diag(ni / 2)
    Ri <- I_ni %x% as.matrix(R)

    notmiss_index <- which(rowSums(is.na(Xi)) == 0 & is.na(yi) == FALSE)

    if (length(notmiss_index) > 0) {
      Xi <- Xi[notmiss_index, ]
      yi <- yi[notmiss_index]
      Zi <- Zi[notmiss_index, ]
      residi <- residi[notmiss_index]
      Ri <- Ri[notmiss_index, notmiss_index]
      Vi <- as.matrix(Zi) %*% as.matrix(D) %*% t(as.matrix(Zi)) + Ri
      Wi <- MASS::ginv(Vi)
      Hessian_i <- as.matrix(t(Xi)) %*% Wi %*% as.matrix(Xi)
      Hessian <- Hessian + Hessian_i
      grad_ik <- as.matrix(t(Xi)) %*% Wi %*% as.matrix(residi)
      Gradient <- cbind(Gradient, grad_ik)
      V <- c(V, list(Vi))
      Y <- c(Y, list(yi))
      MU <- c(
        MU,
        list(as.numeric(apply(as.matrix(Xi), c(1, 2), as.numeric) %*%
                          as.matrix(fixedest)))
      )
    } else {
      Gradient <- cbind(Gradient, rep(0, ncol(X)))
      V <- c(V, list(NA))
      Y <- c(Y, list(NA))
      MU <- c(MU, list(NA))
    }
    pos <- pos + id_list[i]
  }

  Y_ <- c(Y_, list(Y))
  V_ <- c(V_, list(V))
  H_ <- c(H_, list(Hessian))
  G_ <- c(G_, list(Gradient))
  MU_ <- c(MU_, list(MU))

  return(list(H_ = H_, G_ = G_, V_ = V_, Y_ = Y_, MU_ = MU_))
}

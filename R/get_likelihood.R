#' @export

get_likelihood <- function(respvars_n, pairedresult, fixed, XYZ = NULL) {
  # likelihood table for the kth pairedresult, row as observation i,
  # column as pair rs or p
  likelihood_list <- vector()
  for (r in 1:(respvars_n - 1)) {
    if (r == 1) {
      pair_n <- 0
    }
    for (s in (r + 1):respvars_n) { # pair rs = pair p
      pair_n <- pair_n + 1

      D_p <- pairedresult$D_mean[c(r, s), c(r, s)]
      R_p <- pairedresult$R_mean[c(r, s), c(r, s)]
      theta <- as.numeric(t(pairedresult$Fixedest_mean[, c(r, s)]))

      model <- pairedresult$pair_model[[pair_n]]
      # get these from full data set
      X <- XYZ[[pair_n]][, c(
        "int1", "int2",
        colnames(XYZ[[pair_n]])[grep(
          paste(fixed, collapse = "|"),
          colnames(XYZ[[pair_n]])
        )]
      )] |>
        dplyr::select(-any_of(fixed))
      Z <- XYZ[[pair_n]][, c("int1", "int2")]
      y <- XYZ[[pair_n]]$Score
      id <- table(XYZ[[pair_n]]$id) # from full data set

      likelihood_list <- cbind(
        likelihood_list,
        get_likelihood_i(
          D_p = D_p, R_p = R_p, id_list = id, theta = theta,
          X = X, Z = Z, y = y
        )
      )
    }
  }
  return(likelihood_list)
}

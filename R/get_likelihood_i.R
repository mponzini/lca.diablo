#' @export

get_likelihood_i <- function(D_p, R_p, id_list, theta, X, Z, y) {
  # store the likelihood for each observation into the list
  likelihood_list <- vector()
  for (i in 1:length(id_list)) { # i = the ith number observation
    if (i == 1)
      pos <- 1

    Zi <- Z[pos:(pos + id_list[i] - 1), ]
    Xi <- X[pos:(pos + id_list[i] - 1), ]
    yi <- y[pos:(pos + id_list[i] - 1)]
    ni <- length(yi)
    notmiss_index <- which(rowSums(is.na(Xi)) == 0 & is.na(yi) == FALSE)

    ni <- id_list[1]
    I_ni <- diag(ni / 2)
    R_p_i <- I_ni %x% as.matrix(R_p)

    if (length(notmiss_index) > 0) {
      Xi <- Xi[notmiss_index, ]
      Zi <- Zi[notmiss_index, ]
      yi <- yi[notmiss_index]
      R_p_i <- R_p_i[notmiss_index, notmiss_index]

      V_i <- as.matrix(Zi) %*% as.matrix(D_p) %*% t(as.matrix(Zi)) + R_p_i
      Mu_i <- as.numeric(apply(as.matrix(Xi), c(1, 2), as.numeric) %*%
                           as.matrix(theta))

      L_pi <- (2 * pi)^(-ni / 2) * det(V_i)^(-1 / 2) *
        exp(-1 / 2 * (yi - Mu_i) %*% solve(V_i) %*% (yi - Mu_i))
    } else {
      V_i <- NULL
      Mu_i <- NULL
      L_pi <- NULL
    }
    pos <- pos + id_list[i]

    likelihood_list <- append(likelihood_list, L_pi)
  }
  return(likelihood_list)
}

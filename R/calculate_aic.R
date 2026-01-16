#' @export

calculate_aic <- function(test_lca_iterations) {
  # Number of Classes
  k <- ncol(test_lca_iterations$Sum.Log.Lik)

  tb <- test_lca_iterations$Sum.Log.Lik
  # pl likelihood
  pl <- test_lca_iterations$Sum.Log.Lik %>%
    apply(1, max, na.rm = TRUE) %>%
    sum()

  # calculate the trace
  trace_theta <- 0
  for (i in 1:k) {
    Hessian <- test_lca_iterations$Models[[i]]$Hessian
    Gradient <- test_lca_iterations$Models[[i]]$Gradient

    N <- ncol(Gradient)
    H <- Hessian / N   # J in the sas code - average hessian matrix
    J <- 1 / N * Gradient %*% t(Gradient)  # K in the sas code
    G <- H %*% solve(J) %*% H
    trace_theta <- trace_theta + sum(diag(H %*% solve(G)))
  }

  AIC <- -2 * pl + 2 * trace_theta
  BIC <- -2 * pl + log(N) * trace_theta
  return(cbind(AIC, BIC))
}

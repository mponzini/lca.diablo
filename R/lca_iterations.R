#' @export

lca_iterations <- function(data, k, b = 30, respvars, fixed, randomno1,
                           randomno2, timevar, time4mc, id, catvar) {
  # repeat lca to minimize impact of initial cluster assignment
  iterations <- vector(mode = "list", length = b)
  for (i in 1:b) {
    iterations[[i]] <- lca(
      data, k, respvars, fixed, randomno1, randomno2,
      timevar, time4mc, id, catvar
    )
  }
  # select iteration with highest value for the pseudo-likelihood #
  # get the individual pseudo-likelihood for the assigned cluster
  iterations_indiv_likelihoods <- lapply(
    iterations,
    function(x) x$Sum.Log.Lik |>
      tibble::rownames_to_column("temp.id") |>
      mutate(temp.id = as.numeric(temp.id)) |>
      pivot_longer(
        cols = contains("V"),
        names_to = "cluster",
        values_to = "loglik"
      ) |>
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
  if (iteration_check == 1) {
    # if all iterations are the same, return the first iteration
    result <- iterations[[best_iteration[1]]]
  } else {
    # return the best iteration
    result <- iterations[[best_iteration]]
  }

  # return best result
  return(result)
}

#' @export

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

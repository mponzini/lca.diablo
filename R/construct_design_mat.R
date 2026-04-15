#' @export

construct_design_matrix <- function(omics) {
  # extract names
  omics_names <- names(omics)
  len <- length(omics_names)

  # fit pairwise pls
  pls_results <- combn(
    x = names(omics),
    m = 2,
    function(z) {
      tmp_pls <- mixOmics::pls(omics[[z[1]]], omics[[z[2]]], ncomp = 1)
      cor(tmp_pls$variates$X, tmp_pls$variates$Y) |> as.numeric()
    }
  )


  pls_matrix <- diag(x = 0, ncol = len, nrow = len)
  x <- 1
  # fill in upper mat
  for(i in 1:len) {
    for (j in 1:len) {
      if (i >= j) {
        next
      } else {
        pls_matrix[i, j] <- pls_results[x]
        x <- x + 1
      }
    }
  }
  x <- 1
  # fill in lower mat
  for(j in 1:len) {
    for (i in 1:len) {
      if (i <= j) {
        next
      } else {
        pls_matrix[i, j] <- pls_results[x]
        x <- x + 1
      }
    }
  }

  rownames(pls_matrix) <- colnames(pls_matrix) <- omics_names

  return(pls_matrix)
}

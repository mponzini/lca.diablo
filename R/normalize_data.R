normalize_data <- function(data) {
  # extract numeric columns
  num_cols <- sapply(data, is.numeric)

  df <- data
  # scale numeric variables
  df[num_cols] <- scale(df[num_cols], center = TRUE, scale = TRUE)
  # return
  df
}

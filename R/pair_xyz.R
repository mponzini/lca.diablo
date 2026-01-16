#' @export

pair_xyz <- function(data, var1, var2, fixed, rand, timevar, pairno, id,
                     catvar) {
  nop <- length(fixed)
  pair_temp <- data %>%
    mutate(
      Resp1 = ifelse(is.na(!!sym(var1)) == FALSE, as.numeric(!!sym(var1)), NA),
      Resp2 = ifelse(is.na(!!sym(var2)) == FALSE, as.numeric(!!sym(var2)), NA)
    ) %>%
    dplyr::select(matches(c(id, timevar, fixed)), Resp1, Resp2)

  # pivot to long format, handling outcome groups
  out_temp <- pair_temp %>%
    pivot_longer(
      Resp1:Resp2,
      names_to = "NAME",
      values_to = "Score",
      values_drop_na = FALSE
    ) %>%
    mutate(
      outcome_num = as.numeric(substr(NAME, 5, 5)),
      int1 = ifelse(as.numeric(substr(NAME, 5, 5)) == 1, 1, 0),
      int2 = ifelse(as.numeric(substr(NAME, 5, 5)) == 2, 1, 0)
    )


  # Handling Fixed Parameters
  for (i in 1:nop) {
    if (fixed[i] %in% catvar == FALSE) {
      out_temp <- out_temp %>%
        mutate(
          !!sym(paste(fixed[i], 1, "_", sep = "")) :=
            ifelse(outcome_num == 1, !!sym(paste(fixed[i])), 0),
          !!sym(paste(fixed[i], 2, "_", sep = "")) :=
            ifelse(outcome_num == 2, !!sym(paste(fixed[i])), 0)
        )
    } else {
      out_temp <- out_temp %>%
        mutate(
          !!sym(paste(fixed[i], 1, "_", sep = "")) :=
            relevel(
              as.factor(ifelse(outcome_num == 1, !!sym(paste(fixed[i])), 0)),
              ref = "1"
            ),
          !!sym(paste(fixed[i], 2, "_", sep = "")) :=
            relevel(
              as.factor(ifelse(outcome_num == 2, !!sym(paste(fixed[i])), 0)),
              ref = "0"
            )
        )
    }
  }

  out_temp$visit <- as.factor(out_temp$visit)
  out_temp$outcome_num <- as.factor(out_temp$outcome_num)


  return(out_temp)
}

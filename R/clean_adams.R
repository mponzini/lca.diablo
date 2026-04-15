clean_adams <- function(
    dataset,
    adams_vars = c(
      "adams_manic", "adams_depress", "adams_avoid",
      "adams_anxious", "adams_obsess"
    )
){
  dataset |>
    # make sure ADAMS variables are numeric
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(adams_vars),
        ~ as.numeric(.x) |> suppressWarnings()
      )
    )
}

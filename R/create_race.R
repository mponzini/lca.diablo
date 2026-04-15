create_race <- function(
    dataset,
    race_vars = c(
      "race___1", "race___2", "race___3", "race___4",
      "race___5", "race___6", "race___7"
    )
){
  dataset |>
    dplyr::group_by(fxs_sts_id) |>
    tidyr::fill(
      dplyr::all_of(race_vars)
    ) |>
    dplyr::ungroup() |>
    # convert race variables back to 0/1
    dplyr::mutate(
      dplyr::across(
        .cols = all_of(race_vars),
        ~ .x |> as.numeric()
      )
    ) |>
    # create Race variable
    dplyr::mutate(
      Race = dplyr::case_when(
        (rowSums(dplyr::across(dplyr::all_of(race_vars))) > 1 |
           race___4 == 1 | race___5 == 1 | race___6 == 1 | race___7 == 1) ~
          "Other",
        race___1 == 1 ~ "White",
        race___2 == 1 ~ "Black",
        race___3 == 1 ~ "Asian"
      ) |>
        factor(
          levels = c(
            "White", "Black", "Asian", "Other"
          )
        )
    )
}

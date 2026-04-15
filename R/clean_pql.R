clean_pql <- function(
    dataset,
    vars = c(
      "pql_pf_tot", "pql_ef_tot", "pql_socf_tot", "pql_schf_tot",
      "pql_pf_tot_cor", "pql_ef_tot_cor", "pql_socf_tot_cor", "pql_schf_tot_cor"
    )
){
  dataset |>
    # set "" strings to NA_character_
    dplyr::mutate(
      dplyr::across(
        .cols = where(is.character),
        ~ ifelse(.x == "", NA_character_, .x)
      )
    ) |>
    # if Corrected variable is not missing, update Total var value
    dplyr::mutate(
      pql_pf_tot = ifelse(
        test = !is.na(pql_pf_tot_cor),
        yes = pql_pf_tot_cor,
        no = pql_pf_tot
      ),
      pql_ef_tot = ifelse(
        test = !is.na(pql_ef_tot_cor),
        yes = pql_ef_tot_cor,
        no = pql_ef_tot
      ),
      pql_socf_tot = ifelse(
        test = !is.na(pql_socf_tot_cor),
        yes = pql_socf_tot_cor,
        no = pql_socf_tot
      ),
      pql_schf_tot = ifelse(
        test = !is.na(pql_schf_tot_cor),
        yes = pql_schf_tot_cor,
        no = pql_schf_tot
      )
    ) |>
    # convert all pedsql variables to numeric
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(vars),
        ~ as.numeric(.x) |> suppressWarnings()
      )
    ) |>
    # remove corrected variables
    dplyr::select(
      -c(pql_pf_tot_cor, pql_ef_tot_cor, pql_socf_tot_cor, pql_schf_tot_cor)
    )
}

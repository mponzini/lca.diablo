#' @title Calculate Differences in Omics Data Between Visits
#' @description
#' The Similarity Network Fusion does not handle longitudinal data, so this
#' function calculates the difference between Visit 3 and Visit 1 for each
#' patient prior to fitting SNF.
#'
#' @param omics_data A [data.frame] that contains the omics data with
#' row names as sample IDs.
#' @param patient_data A [data.frame] that contains the patient data with
#' columns for fxs_sts_id, Visit1, and Visit3.
#' @returns A [data.frame] containing the differences (Visti 3 - Visit 1) in
#' omics data for each patient.
#' @export

calculate_difference <- function(omics_data, patient_data) {
  # set difference function for data.table
  difference <- function(x) {
    x - data.table::shift(x)
  }
  # move rownames (id) to column
  omics <- omics_data |>
    tibble::as_tibble(rownames = NA) |>
    tibble::rownames_to_column(var = "id")
  # convert patient_data to long format: id, lab_id. ordered Visit1, Visit3
  patient_long <- patient_data |>
    dplyr::select(fxs_sts_id, Visit1, Visit3) |>
    tidyr::pivot_longer(
      cols = c(Visit1, Visit3),
      names_to = "visit",
      values_to = "lab_id"
    )
  # merge omics and patient data, sort by fxs_sts_id
  all_data <- omics |>
    dplyr::left_join(
      x = _,
      y = patient_long,
      by = c("id" = "lab_id")
    ) |>
    dplyr::arrange(fxs_sts_id, visit)

  # drop id variables, convert to data.table and calculate differences
  all_data_dt <- all_data |>
    dplyr::select(-id) |>
    data.table::as.data.table()

  data_differences <- all_data_dt[
    ,
    lapply(
      .SD,
      difference
    ),
    fxs_sts_id,
    .SDcols = colnames(all_data_dt)[-c(ncol(all_data_dt) - 1,
                                       ncol(all_data_dt))]
  ] |>
    dplyr::filter(!is.na(.data[[colnames(all_data_dt)[1]]]))

  return(data_differences)
}

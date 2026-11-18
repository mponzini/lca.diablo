#' Calculate phenotype changes from Visit 1 to Visit 3
#'
#' Creates one row per participant, retains the treatment assignment, and
#' calculates Visit 3 minus Visit 1 for all phenotype variables.
#'
#' A difference is `NA` when either Visit 1 or Visit 3 is missing. Variable
#' labels stored in the `"label"` attribute are retained in the result.
#'
#' @param data A data frame containing repeated phenotype measurements.
#' @param id_col Participant identifier column.
#' @param visit_col Visit column.
#' @param treatment_col Treatment assignment column.
#' @param baseline_visit Label identifying Visit 1.
#' @param followup_visit Label identifying Visit 3.
#'
#' @return A data frame containing participant ID, treatment, and phenotype
#'   differences.
#'
#' @examples
#' \dontrun{
#' phenotype_diff <- calculate_phenotype_diff(phenotype_data)
#' }
#'
#' @export
calculate_phenotype_diff <- function(
    data,
    id_col = "fxs_sts_id",
    visit_col = "redcap_event_name",
    treatment_col = "Treatment",
    baseline_visit = "Baseline Visit (Visit 1)",
    followup_visit = "Week 16 (Visit 3/Early Term)"
) {
  required_cols <- c(id_col, visit_col, treatment_col)

  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }

  column_args <- list(
    id_col = id_col,
    visit_col = visit_col,
    treatment_col = treatment_col
  )
  valid_column_args <- vapply(
    column_args,
    function(x) is.character(x) && length(x) == 1L && !is.na(x),
    logical(1)
  )
  if (!all(valid_column_args)) {
    stop(
      "id_col, visit_col, and treatment_col must each name one column.",
      call. = FALSE
    )
  }

  if (!all(required_cols %in% names(data))) {
    missing_cols <- setdiff(required_cols, names(data))
    stop(
      paste("Missing required column(s):", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  visit_data <- data[
    data[[visit_col]] %in% c(baseline_visit, followup_visit),
    ,
    drop = FALSE
  ]

  if (anyDuplicated(visit_data[c(id_col, visit_col)])) {
    stop(
      "A participant has more than one row for the same visit.",
      call. = FALSE
    )
  }

  measure_cols <- setdiff(
    names(data),
    required_cols
  )

  non_numeric <- measure_cols[
    !vapply(data[measure_cols], is.numeric, logical(1))
  ]

  if (length(non_numeric) > 0L) {
    stop(
      paste(
        "These phenotype columns are not numeric:",
        paste(non_numeric, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  baseline_data <- visit_data[
    visit_data[[visit_col]] == baseline_visit,
    ,
    drop = FALSE
  ]

  followup_data <- visit_data[
    visit_data[[visit_col]] == followup_visit,
    ,
    drop = FALSE
  ]

  participant_ids <- unique(visit_data[[id_col]])

  baseline_index <- match(
    participant_ids,
    baseline_data[[id_col]]
  )

  followup_index <- match(
    participant_ids,
    followup_data[[id_col]]
  )

  treatment <- baseline_data[[treatment_col]][baseline_index]
  missing_treatment <- is.na(treatment)

  treatment[missing_treatment] <-
    followup_data[[treatment_col]][followup_index][missing_treatment]

  both_visits <- !is.na(baseline_index) & !is.na(followup_index)
  treatment_mismatch <- both_visits &
    !is.na(baseline_data[[treatment_col]][baseline_index]) &
    !is.na(followup_data[[treatment_col]][followup_index]) &
    as.character(baseline_data[[treatment_col]][baseline_index]) !=
      as.character(followup_data[[treatment_col]][followup_index])

  if (any(treatment_mismatch)) {
    stop(
      "Treatment assignment differs between baseline and follow-up for at least one participant.",
      call. = FALSE
    )
  }

  result <- data.frame(
    participant_ids,
    treatment,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  names(result) <- c(id_col, treatment_col)

  for (column in measure_cols) {
    result[[column]] <-
      followup_data[[column]][followup_index] -
      baseline_data[[column]][baseline_index]

    variable_label <- attr(data[[column]], "label", exact = TRUE)
    if (!is.null(variable_label)) {
      attr(result[[column]], "label") <- variable_label
    }
  }

  result
}

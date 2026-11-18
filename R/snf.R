#' Calculate change between two visits
#'
#' @param data Data frame containing repeated measures.
#' @param id_col Subject identifier column name.
#' @param visit_col Visit column name.
#' @param measure_cols Clinical measure column names.
#' @param baseline_visit Baseline visit label. Defaults to `"V1"`.
#' @param followup_visit Follow-up visit label. Defaults to `"V3"`.
#' @param favorable_direction Optional named numeric vector used to orient
#'   deltas so positive values indicate a favorable response.
#'
#' @return A data frame with one row per subject and visit deltas.
#' @export
calculate_visit_delta <- function(
    data,
    id_col,
    visit_col,
    measure_cols,
    baseline_visit = "V1",
    followup_visit = "V3",
    favorable_direction = NULL
) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(id_col), length(id_col) == 1L, id_col %in% names(data))
  stopifnot(is.character(visit_col), length(visit_col) == 1L, visit_col %in% names(data))
  stopifnot(all(measure_cols %in% names(data)))

  baseline_data <- data[data[[visit_col]] == baseline_visit, c(id_col, measure_cols), drop = FALSE]
  followup_data <- data[data[[visit_col]] == followup_visit, c(id_col, measure_cols), drop = FALSE]

  names(baseline_data)[-1] <- paste0(measure_cols, "__baseline")
  names(followup_data)[-1] <- paste0(measure_cols, "__followup")

  merged <- merge(
    x = followup_data,
    y = baseline_data,
    by = id_col,
    all = FALSE,
    sort = FALSE
  )

  delta_df <- data.frame(id = merged[[id_col]], stringsAsFactors = FALSE)
  names(delta_df)[1] <- id_col

  for (measure in measure_cols) {
    followup_col <- paste0(measure, "__followup")
    baseline_col <- paste0(measure, "__baseline")
    delta_df[[measure]] <- merged[[followup_col]] - merged[[baseline_col]]
  }

  if (!is.null(favorable_direction)) {
    if (is.null(names(favorable_direction))) {
      stop("favorable_direction must be a named numeric vector.", call. = FALSE)
    }
    valid_measures <- intersect(measure_cols, names(favorable_direction))
    for (measure in valid_measures) {
      delta_df[[measure]] <- delta_df[[measure]] * favorable_direction[[measure]]
    }
  }

  delta_df
}

.filter_uninformative_features <- function(
    block_matrix,
    filter_groups = NULL,
    top_features = NULL,
    method = c("mad", "variance")
) {
  if (is.null(top_features)) {
    return(block_matrix)
  }

  method <- match.arg(method)
  keep_n <- min(top_features, ncol(block_matrix))

  if (!is.null(filter_groups) && length(filter_groups) != nrow(block_matrix)) {
    stop("filter_groups must have one value per subject.", call. = FALSE)
  }

  supervised <- !is.null(filter_groups) &&
    length(unique(stats::na.omit(filter_groups))) >= 2L

  if (supervised) {
    # Label-based (supervised) selection — kept for backward compatibility.
    # NOTE: selecting features by their association with a group label (e.g.
    # treatment assignment) before clustering on those same features will
    # make the resulting clusters circularly associated with that label.
    # Prefer the unsupervised branch below (filter_groups = NULL) when the
    # goal is label-free subtype discovery.
    p_values <- apply(
      block_matrix,
      MARGIN = 2,
      FUN = function(x) {
        complete <- !is.na(x) & !is.na(filter_groups)
        if (sum(complete) < 4L || length(unique(filter_groups[complete])) < 2L) {
          return(1)
        }

        tryCatch(
          stats::t.test(x[complete] ~ as.factor(filter_groups[complete]))$p.value,
          error = function(e) 1
        )
      }
    )

    p_values[!is.finite(p_values)] <- 1
    keep_idx <- order(p_values, decreasing = FALSE)[seq_len(keep_n)]
  } else {
    # Unsupervised selection — rank features by dispersion across ALL
    # subjects, with no reference to any group label. MAD (median absolute
    # deviation) is the default because it is robust to outliers, which are
    # common in omics data; variance is available as an alternative.
    dispersion <- apply(
      block_matrix,
      MARGIN = 2,
      FUN = function(x) {
        x <- x[!is.na(x)]
        if (length(x) < 2L) {
          return(0)
        }

        if (method == "mad") stats::mad(x) else stats::var(x)
      }
    )

    dispersion[!is.finite(dispersion)] <- 0
    keep_idx <- order(dispersion, decreasing = TRUE)[seq_len(keep_n)]
  }

  block_matrix[, keep_idx, drop = FALSE]
}

.normalize_snf_block <- function(block_matrix, control_index = NULL, log_transform = FALSE) {
  block_matrix <- as.matrix(block_matrix)
  storage.mode(block_matrix) <- "double"

  if (log_transform) {
    min_value <- suppressWarnings(min(block_matrix, na.rm = TRUE))
    if (is.finite(min_value) && min_value <= 0) {
      block_matrix <- block_matrix - min_value + 1
    }
    block_matrix <- log1p(block_matrix)
  }

  keep_cols <- apply(
    block_matrix,
    MARGIN = 2,
    FUN = function(x) {
      length(stats::na.omit(x)) >= 2 && stats::sd(x, na.rm = TRUE) > 0
    }
  )
  block_matrix <- block_matrix[, keep_cols, drop = FALSE]
  if (ncol(block_matrix) == 0L) {
    stop("No informative features remained after filtering.", call. = FALSE)
  }

  if (!is.null(control_index)) {
    center <- colMeans(block_matrix[control_index, , drop = FALSE], na.rm = TRUE)
    scale <- apply(block_matrix[control_index, , drop = FALSE], 2, stats::sd, na.rm = TRUE)
  } else {
    center <- colMeans(block_matrix, na.rm = TRUE)
    scale <- apply(block_matrix, 2, stats::sd, na.rm = TRUE)
  }

  scale[!is.finite(scale) | scale == 0] <- 1
  block_matrix <- sweep(block_matrix, 2, center, "-")
  block_matrix <- sweep(block_matrix, 2, scale, "/")
  block_matrix[is.na(block_matrix)] <- 0

  block_matrix
}

#' Run Similarity Network Fusion and derive consensus subtypes
#'
#' @param omics Named list of omics matrices/data frames with shared subjects
#'   in rows and features in columns. May contain a single block, in which
#'   case there is nothing to fuse and clustering runs directly on that
#'   block's own affinity matrix (`SNFtool::SNF()` requires at least two
#'   blocks and is skipped in this case).
#' @param n_clusters Number of consensus subtypes.
#' @param K Number of neighbors for SNF.
#' @param t Number of fusion iterations. Ignored when `omics` has only one
#'   block, since there is nothing to fuse.
#' @param alpha Hyper-parameter for affinity matrix construction.
#' @param control_ids Optional subject IDs used to normalize each omics block
#'   against controls. Leave as `NULL` (default) to normalize each block
#'   against all subjects, which is required for label-free subtype
#'   discovery (see `filter_groups`).
#' @param log_transform Should each block be log-transformed before
#'   standardization.
#' @param filter_groups Optional group labels (e.g. treatment assignment) used
#'   for supervised, two-group t-test feature filtering. Leave as `NULL`
#'   (default) to select the top features by unsupervised dispersion
#'   (`feature_selection_method`) instead. Supplying `filter_groups` makes the
#'   resulting SNF subtypes circularly associated with that label, since the
#'   features fed into clustering were chosen because they discriminate the
#'   label — avoid this unless label-informed feature selection is explicitly
#'   intended.
#' @param top_features Optional number of top features retained per block.
#'   Selection is unsupervised (by `feature_selection_method`) unless
#'   `filter_groups` is supplied.
#' @param feature_selection_method Dispersion measure used for unsupervised
#'   feature selection when `filter_groups` is `NULL`. One of `"mad"`
#'   (median absolute deviation; default, robust to outliers) or
#'   `"variance"`. Ignored when `filter_groups` is supplied.
#'
#' @return A list containing processed blocks, affinity matrices, fused network,
#'   and subtype assignments.
#' @export
run_snf_subtypes <- function(
    omics,
    n_clusters = 2,
    K = 20,
    t = 20,
    alpha = 0.5,
    control_ids = NULL,
    log_transform = FALSE,
    filter_groups = NULL,
    top_features = NULL,
    feature_selection_method = c("mad", "variance")
) {
  feature_selection_method <- match.arg(feature_selection_method)
  if (!requireNamespace("SNFtool", quietly = TRUE)) {
    stop("Package 'SNFtool' is required. Install it with install.packages('SNFtool').", call. = FALSE)
  }

  if (!is.list(omics) || length(omics) < 1L) {
    stop("omics must be a list with at least one omics block.", call. = FALSE)
  }

  first_block <- as.matrix(omics[[1]])
  sample_ids <- rownames(first_block)
  if (is.null(sample_ids)) {
    sample_ids <- as.character(seq_len(nrow(first_block)))
  }

  control_index <- NULL
  if (!is.null(control_ids)) {
    control_index <- match(as.character(control_ids), sample_ids)
    control_index <- control_index[!is.na(control_index)]
    if (length(control_index) < 2L) {
      stop("control_ids must match at least two samples.", call. = FALSE)
    }
  }

  if (!is.null(filter_groups)) {
    if (is.null(names(filter_groups))) {
      stop("filter_groups must be named with sample IDs.", call. = FALSE)
    }

    filter_groups <- filter_groups[sample_ids]

    if (anyNA(filter_groups)) {
      stop("Some samples are missing from filter_groups.", call. = FALSE)
    }
  }

  processed_omics <- lapply(
    omics,
    FUN = function(block) {
      block <- as.matrix(block)

      if (nrow(block) != length(sample_ids)) {
        stop("All omics blocks must have the same number of rows.", call. = FALSE)
      }

      if (is.null(rownames(block))) {
        rownames(block) <- sample_ids
      } else if (!setequal(rownames(block), sample_ids)) {
        stop("All omics blocks must contain the same sample IDs in row names.", call. = FALSE)
      }

      row_order <- match(sample_ids, rownames(block))
      block <- block[row_order, , drop = FALSE]

      block <- .filter_uninformative_features(
        block_matrix = block,
        filter_groups = filter_groups,
        top_features = top_features,
        method = feature_selection_method
      )

      .normalize_snf_block(
        block_matrix = block,
        control_index = control_index,
        log_transform = log_transform
      )
    }
  )

  affinity_matrices <- lapply(
    processed_omics,
    FUN = function(block) {
      distance_matrix <- sqrt(SNFtool::dist2(block, block))
      SNFtool::affinityMatrix(
        distance_matrix,
        K = K,
        sigma = alpha
      )
    }
  )

  if (length(affinity_matrices) >= 2L) {
    fused_network <- SNFtool::SNF(Wall = affinity_matrices, K = K, t = t)
  } else {
    # SNFtool::SNF() divides by (length(Wall) - 1) during its diffusion
    # step, so it cannot run on a single block (would divide by zero).
    # With only one omics block there is nothing to "fuse" anyway, so
    # cluster directly on that block's own affinity matrix.
    fused_network <- affinity_matrices[[1]]
  }
  dimnames(fused_network) <- list(sample_ids, sample_ids)

  subtype_ids <- SNFtool::spectralClustering(fused_network, K = n_clusters)
  subtypes <- factor(paste("Subtype", subtype_ids))
  names(subtypes) <- sample_ids

  list(
    processed_omics = processed_omics,
    affinity_matrices = affinity_matrices,
    fused_network = fused_network,
    subtypes = subtypes
  )
}

#' Join SNF subtypes to clinical annotations
#'
#' @param subtypes Named subtype vector from [run_snf_subtypes()].
#' @param clinical_data Clinical annotation data frame.
#' @param id_col Subject identifier column in `clinical_data`.
#'
#' @return A data frame with the original annotations and SNF subtype labels.
#' @export
annotate_snf_subtypes <- function(subtypes, clinical_data, id_col) {
  stopifnot(is.data.frame(clinical_data))
  stopifnot(id_col %in% names(clinical_data))

  subtype_df <- data.frame(
    id = names(subtypes),
    snf_subtype = as.character(subtypes),
    stringsAsFactors = FALSE
  )
  names(subtype_df)[1] <- id_col

  merge(
    x = clinical_data,
    y = subtype_df,
    by = id_col,
    all.x = TRUE,
    sort = FALSE
  )
}

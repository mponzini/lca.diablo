## Verify that the calculate_difference() approach produces a valid X_diff
## with proper row names that allow the SNF annotation join to succeed.

devtools::load_all()
library(here)

## ── Load archive data ─────────────────────────────────────────────────────────

omics_list     <- readRDS(here("extdata", "archive", "DIABLO_X_list.rds"))
patient_design <- readRDS(here("extdata", "archive", "DiffExp_Design_20230105.rds"))
lca_result     <- readRDS(here("extdata", "archive", "Metformin.k2.LCA.NoOutlier.rds"))
Y              <- readRDS(here("extdata", "archive", "k2.Assignments.20230714.rds"))
unblinding_key <- lca.diablo::unblinding_key

## ── Build X_diff the new way ──────────────────────────────────────────────────

X_diff <- list(
  Metabolites = calculate_difference(omics_list$compounds,  patient_data = patient_design),
  Lipids      = calculate_difference(omics_list$lipids,     patient_data = patient_design),
  Peptides    = calculate_difference(omics_list$proteins,   patient_data = patient_design),
  Proteomics  = calculate_difference(omics_list$proteomics, patient_data = patient_design)
)
X_diff <- lapply(X_diff, tibble::column_to_rownames, var = "fxs_sts_id")

lca_ids    <- as.character(unique(lca_result$Data$id))
outlier_id <- setdiff(rownames(X_diff[[1]]), lca_ids)
cat("Outlier removed:", if (length(outlier_id)) outlier_id else "(none)", "\n")
if (length(outlier_id) == 1L)
  X_diff <- lapply(X_diff, function(m) m[rownames(m) != outlier_id, , drop = FALSE])

## ── Check 1: row names exist and are real IDs ─────────────────────────────────

cat("\n=== Check 1: X_diff has non-NULL row names ===\n")
has_rn <- !is.null(rownames(X_diff[[1]]))
cat("  rownames present:", has_rn, "\n")
cat("  first 5 IDs    :", paste(head(rownames(X_diff[[1]]), 5), collapse = ", "), "\n")

## ── Check 2: dimensions match Y ───────────────────────────────────────────────

cat("\n=== Check 2: nrow(X_diff) == length(Y) ===\n")
dim_ok <- nrow(X_diff[[1]]) == length(Y)
cat("  nrow(X_diff):", nrow(X_diff[[1]]), " | length(Y):", length(Y),
    " | MATCH:", dim_ok, "\n")

## ── Check 3: X_diff IDs match LCA NoOutlier IDs ──────────────────────────────

cat("\n=== Check 3: rownames(X_diff) match LCA NoOutlier subject IDs ===\n")
xdiff_ids  <- as.character(rownames(X_diff[[1]]))
ids_match  <- setequal(xdiff_ids, lca_ids)
cat("  Same set of IDs:", ids_match, "\n")
if (!ids_match) {
  cat("  In X_diff not in LCA:", setdiff(xdiff_ids, lca_ids), "\n")
  cat("  In LCA not in X_diff:", setdiff(lca_ids, xdiff_ids), "\n")
}

## ── Check 4: simulate compare_clusters and annotation join ───────────────────

cat("\n=== Check 4: SNF annotation join produces non-NA subtypes ===\n")

compare_clusters <- lca_result$Data |>
  dplyr::left_join(
    y = unblinding_key |> dplyr::select(fxs_sts_id, unmasked),
    by = c("id" = "fxs_sts_id")
  ) |>
  dplyr::select(id, cluster, unmasked) |>
  dplyr::distinct()

# Simulate SNF with dummy subtypes using X_diff row names as IDs
dummy_subtypes        <- factor(paste("Subtype", rep(1:2, length.out = nrow(X_diff[[1]]))))
names(dummy_subtypes) <- rownames(X_diff[[1]])

snf_annotations_sim <- annotate_snf_subtypes(
  subtypes      = dummy_subtypes,
  clinical_data = compare_clusters |> dplyr::mutate(id = as.character(id)),
  id_col        = "id"
)

n_na      <- sum(is.na(snf_annotations_sim$snf_subtype))
n_total   <- nrow(snf_annotations_sim)
join_ok   <- n_na == 0
cat("  Rows with NA snf_subtype:", n_na, "/", n_total,
    " | Join OK:", join_ok, "\n")
if (!join_ok)
  cat("  Missing IDs (in X_diff not in compare_clusters):",
      setdiff(xdiff_ids, as.character(compare_clusters$id)), "\n")

## ── Check 5: value comparison vs archived X_diff ─────────────────────────────

cat("\n=== Check 5: column means vs archived X_diff (value sanity) ===\n")

X_diff_arch <- readRDS(here("extdata", "archive", "DIABLO_X_Difference_list.rds"))
names(X_diff_arch) <- c("Metabolites", "Lipids", "Peptides", "Proteomics")
X_diff_arch <- lapply(X_diff_arch, function(m) m[-10, , drop = FALSE])

for (nm in names(X_diff)) {
  max_diff <- max(abs(
    colMeans(X_diff[[nm]], na.rm = TRUE) -
    colMeans(X_diff_arch[[nm]], na.rm = TRUE)
  ))
  cat("  ", nm, "— max col-mean diff:", format(max_diff, digits = 3), "\n")
}

## ── Summary ───────────────────────────────────────────────────────────────────

cat("\n=== Summary ===\n")
all_ok <- has_rn && dim_ok && ids_match && join_ok
cat(if (all_ok)
  "ALL CHECKS PASSED — calculate_difference() approach is valid.\n"
else
  "ONE OR MORE CHECKS FAILED — review output above.\n")


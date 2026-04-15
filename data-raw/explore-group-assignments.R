blinded_groups <- openxlsx::read.xlsx(
  xlsxFile = "C:/Users/mponzini/OneDrive - UC Davis Health/KyoungmiProjects/metformin/Data/DSMB_ IDS Blinded Data to date.xlsx"
)
id_key <- openxlsx::read.xlsx(
  xlsxFile = "C:/Users/mponzini/Documents/GitHub/lca.diablo/inst/extdata/fxs_study_key.xlsx"
)
metformin_lca_2 <- readRDS(
  paste0(
    "H:/Kyoungmi/Hagerman/Metformin_DIA-MS/Research/Metformin.",
    "k2.LCA.NoOutlier.rds"
  )
)

blinded_groups <- blinded_groups |>
  dplyr::mutate(
    unmasked = dplyr::case_when(
      Treatment.Code == "A" ~ "Metformin",
      Treatment.Code == "B" ~ "Placebo"
    )
  )

blinded_groups2 <- blinded_groups |>
  dplyr::select(`Sub..Number`, Treatment.Code, unmasked) |>
  dplyr::left_join(
    x = _,
    y = id_key,
    by = c("Sub..Number" = "childstudyid")
  )

compare_clusters <- metformin_lca_2$Data |>
  dplyr::left_join(
    x = _,
    y = blinded_groups2 |> dplyr::select(fxs_sts_id, unmasked),
    by = c("id" = "fxs_sts_id")
  )

table(compare_clusters$cluster, compare_clusters$unmasked, useNA = 'always')

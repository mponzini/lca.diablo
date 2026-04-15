#### Link groups to unblinded groups ####

blinded_groups <- openxlsx::read.xlsx(
  xlsxFile = "C:/Users/mponzini/OneDrive - UC Davis Health/KyoungmiProjects/metformin/Data/DSMB_ IDS Blinded Data to date.xlsx"
)
id_key <- openxlsx::read.xlsx(
  xlsxFile = "./inst/extdata/fxs_study_key.xlsx"
)

blinded_groups <- blinded_groups |>
  dplyr::mutate(
    unmasked = dplyr::case_when(
      Treatment.Code == "A" ~ "Metformin",
      Treatment.Code == "B" ~ "Placebo"
    )
  )

unblinding_key <- blinded_groups |>
  dplyr::select(`Sub..Number`, Treatment.Code, unmasked) |>
  dplyr::left_join(
    x = _,
    y = id_key,
    by = c("Sub..Number" = "childstudyid")
  )

usethis::use_data(unblinding_key, overwrite = TRUE)

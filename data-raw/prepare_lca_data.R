#### Prepare Clinical data for LCA ####
# run REDCap import script #
source(
  "./data-raw/CTSC4328ADoubleBlind-MetforminLongitudina_R_2023-06-02_1015.r"
)
# separate out demographic data
demog_data <- data |>
  dplyr::select(fxs_sts_id, redcap_event_name.factor, profile_gender.factor,
                profile_race___1:profile_race___5,
                profile_race___1.factor:profile_race___5.factor,
                profile_ethnic.factor) |>
  dplyr::filter(redcap_event_name.factor == "Baseline Visit (Visit 1)") |>
  dplyr::filter(fxs_sts_id != "Test") |>
  dplyr::mutate(
    race_selected = rowSums(
      dplyr::across(
        .cols = tidyselect::num_range(prefix = "profile_race___", range = 1:5)
      ),
      na.rm = TRUE
    ),
    Race = dplyr::case_when(
      race_selected > 1 ~ "Multiple",
      profile_race___1.factor == "Checked" ~ "White",
      profile_race___2.factor == "Checked" ~ "Native Hawaiian/PI",
      profile_race___3.factor == "Checked" ~ "Black/AA",
      profile_race___4.factor == "Checked" ~ "Asian",
      profile_race___5.factor == "Checked" ~ "Native American/Alaska Native",
      TRUE ~ "Other"
    ),
    Ethnicity = profile_ethnic.factor,
    sex.factor = profile_gender.factor
  )

demog_short <- demog_data |>
  dplyr::select(fxs_sts_id, sex.factor, Race, Ethnicity)

# set 999 to NA
data[data == 999] <- NA

# set ADAMS scores > 999 to missing
data <- data |>
  dplyr::mutate(
    dplyr::across(
      .cols = c(adams_manic, adams_depress, adams_avoid, adams_anxious,
                adams_obsess),
      ~ ifelse(.x > 999, NA, .x)
    ),
    dplyr::across(
      .cols = c(pql_pf_tot, pql_socf_tot, pql_schf_tot, pql_ef_tot),
      ~ ifelse(.x > 100, NA, .x)
    )
  ) |>
  # calculate overall scores for ADAMS and PedsQL
  dplyr::rowwise() |>
  dplyr::mutate(
    ADAMS_Total = ifelse(
      (is.na(adams_manic) & is.na(adams_depress) & is.na(adams_avoid) &
         is.na(adams_anxious) & is.na(adams_obsess)),
      NA_real_,
      sum(c(adams_manic, adams_depress, adams_avoid,
            adams_anxious, adams_obsess), na.rm = TRUE)
    ),
    PedsQL = ifelse(
      (is.na(pql_pf_tot) & is.na(pql_socf_tot) &
         is.na(pql_schf_tot) & is.na(pql_ef_tot)),
      NA_real_,
      sum(c(pql_pf_tot, pql_socf_tot, pql_schf_tot, pql_ef_tot),
          na.rm = TRUE)
    )
  ) |>
  dplyr::ungroup()

# labels dropped after previous step? redo
label(data$adams_manic)="Manic/Hyperactive Behavior Total"
label(data$adams_depress)="Depressed Mood Total"
label(data$adams_avoid)="Social Avoidance Total"
label(data$adams_anxious)="General Anxiety Total"
label(data$adams_obsess)="Obsessive/Compulsive Behavior Total"

# separate out clinical data
clin_data <- data |>
  dplyr::select(fxs_sts_id, redcap_event_name.factor,
                cgis_soi:pql_ef_tot_cor)
# split data by visit
clin_visit1 <- clin_data |>
  dplyr::filter(redcap_event_name.factor == "Baseline Visit (Visit 1)")
clin_visit2 <- clin_data |>
  dplyr::filter(redcap_event_name.factor == "Week 8 Visit (Visit 2)")
clin_visit3 <- clin_data |>
  dplyr::filter(redcap_event_name.factor == "Week 16 (Visit 3/Early Term)")

# separate out molecular data
molec_data <- data |>
  dplyr::select(fxs_sts_id, redcap_event_name.factor, fmrp_rhmc:tassone_meth)
# split data by visit
molec_visit1 <- molec_data |>
  dplyr::filter(redcap_event_name.factor == "Baseline Visit (Visit 1)")
molec_visit2 <- molec_data |>
  dplyr::filter(redcap_event_name.factor == "Week 8 Visit (Visit 2)")
molec_visit3 <- molec_data |>
  dplyr::filter(redcap_event_name.factor == "Week 16 (Visit 3/Early Term)")

# data for LCA
final_data <- data |>
  dplyr::filter(fxs_sts_id != "Test") |>
  dplyr::select(fxs_sts_id, redcap_event_name.factor, abc_comp_rev,
                ADAMS_Total, PedsQL) |>
  dplyr::mutate(
    redcap_event_name.factor = dplyr::case_when(
      redcap_event_name.factor == "Baseline Visit (Visit 1)" ~ 1,
      redcap_event_name.factor == "Week 8 Visit (Visit 2)" ~ 2,
      redcap_event_name.factor == "Week 16 (Visit 3/Early Term)" ~ 3
    )
  ) |>
  dplyr::rename(id = fxs_sts_id,
         visit = redcap_event_name.factor)

metformin_clinical <- final_data |>
  dplyr::filter(
    !(id %in% c("101597-100", "101597-200", "100014-100", "500005-586",
                "500010-107", "500008-020", "500011-406", "500011-433")
      )
  )

# import DIABLO design matrix to extract ID
group_design <- readRDS(
  "./inst/extdata/archive/mDIABLO_Design.rds"
)

# import proteomics design to link fxs id to flora id
proteomics_design <- readRDS(
  "./inst/extdata/archive/Proteomics_Design_Clinical_V3_20221212.rds"
)

id_link <- proteomics_design |>
  dplyr::select(fxs_sts_id, ID) |>
  dplyr::mutate(
    ID = paste0("x", gsub("-", ".", x = ID))
  ) |>
  dplyr::rename(Login = ID) |>
  dplyr::full_join(
    x = _,
    y = group_design |>
      dplyr::filter(Visit == "Visit3") |>
      dplyr::select(Login, ID)
  )

# merge id link with clinical data
metformin_clinical <- metformin_clinical |>
  dplyr::mutate(id = id |> as.character()) |>
  dplyr::rename(fxs_sts_id = id) |>
  dplyr::left_join(
    x = _,
    y = id_link
  )


usethis::use_data(metformin_clinical, overwrite = TRUE)

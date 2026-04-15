formData <- list("token"= Sys.getenv("redcap_key"),
                 content='record',
                 action='export',
                 format='json',
                 type='flat',
                 csvDelimiter='',
                 # import select forms
                 'forms[0]' = 'patient_demographics',
                 'forms[1]' = 'patient_profile',
                 'forms[2]' = 'cgiseverity',
                 'forms[3]' = 'cgiimprovement',
                 'forms[4]' = 'visual_analogue_scale',
                 'forms[5]' = 'child_sleep_habits_questionnaire',
                 'forms[6]' = 'pediatric_quality_of_life_questionnaire',
                 'forms[7]' = 'snap_iv',
                 'forms[8]' = 'abc_community',
                 'forms[9]' = 'adams',
                 'forms[10]' = 'vineland_3',
                 'forms[11]' = 'paul_fmrp_data',
                 'forms[12]' = 'tassone_molecular_data',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)
response <- httr::POST(Sys.getenv("redcap_url"), body = formData, encode = "form")
result <- httr::content(response)
dataset <- dplyr::bind_rows(result)

redcap_data <- dataset |>
  # create single race variable
  create_race() |>
  # clean variables from peds ql
  clean_pql() |>
  # clean variables from ADAMS
  clean_adams() |>
  # populate childstudyid across all visits from baseline value
  dplyr::group_by(fxs_sts_id) |>
  tidyr::fill(
    childstudyid,
    .direction = "downup"
  ) |>
  # populate sex across all visits from baseline value
  tidyr::fill(
    sex,
    .direction = "downup"
  ) |>
  dplyr::ungroup()


usethis::use_data(redcap_data, overwrite = TRUE)

library(here)
library(survey)

source(here('R/helpers/NSFG/formats.R'))
source(here('R/helpers/huxtable.R'))
source(here('R/helpers/survey.R'))

get_NSFG_survey_design <- function(dt) {
  svydesign(
    ids = ~SECU,
    strata = ~SEST,
    data = dt,
    nest = TRUE,
    weights = ~WGT2017_2019
  )
}

estimate_NSFG_mentions <- function(
  formats_table,
  survey_design,
  prefix,
  format,
  skip = NULL
) {
  estimates <- estimate_mentions(
    survey_design,
    prefix, 
    get_NSFG_format_indices(formats_table, format, skip)
  )
  estimates[, description := factorize(description, format, formats_table)]
  estimates
}

estimate_NSFG_mentions_in_subdomains <- function(
  formats_table,
  row_estimation_function,
  format,
  skip = NULL
) {
  estimates <- estimate_mentions_in_subdomains(
    row_estimation_function,
    get_NSFG_format_indices(formats_table, format, skip)
  )
  estimates[, description := factorize(description, format, formats_table)]
  estimates
}

estimate_and_combine_NSFG_totals_and_percentages <- function(fem_dt, male_dt, f, digits = 2) {
  style_and_combine_totals_and_percentages_vertically(
    'Females',
    estimate_totals_and_percentages(
      f,
      get_NSFG_survey_design(fem_dt)
    ),
    'Males',
    estimate_totals_and_percentages(
      f,
      get_NSFG_survey_design(male_dt)
    ),
    digits = digits
  )
}

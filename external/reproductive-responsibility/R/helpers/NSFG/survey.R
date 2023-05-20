library(here)
library(survey)

source(here('R/helpers/NSFG/formats.R'))
source(here('R/helpers/huxtable.R'))
source(here('R/helpers/survey.R'))

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

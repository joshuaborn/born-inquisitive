library(data.table)
library(here)
library(survey)

i_am('R/NSFG_helpers.R')

load_NSFG_data <- function(years, data_name) {
  this_list <- list(
    fread(
      here(sprintf('data/%sData_%s.csv', data_name, years)),
      key = ifelse(data_name == 'FemPreg', c('CASEID', 'PREGORDR'), 'CASEID')
    ),
    fread(
      here(sprintf('data/%sLabels_%s.csv', data_name, years)),
      select = c('name', 'label'),
      key = 'name'
    ),
    fread(
      here(sprintf('data/%sFormats_%s.csv', data_name, years)),
      select = c( 'FMTNAME', 'START', 'LABEL'),
      key = c('FMTNAME', 'START')
   )
  )

  setnames(this_list[[2]], c('column_name', 'column_label'))
  setnames(this_list[[3]], c('format_name', 'factor_value', 'factor_label'))

  names(this_list) <- c('Data', 'Labels', 'Formats')

  this_list
}


get_NSFG_survey_design <- function(dt) {
  svydesign(
    ids = ~SECU,
    strata = ~SEST,
    data = dt,
    nest = TRUE,
    weights = ~WGT2017_2019
  )
}

get_NSFG_format_indices <- function(formats_table, format, skip = NULL) {
  idxs <- formats_table[format_name == format, factor_value]
  if (!is.null(skip)) {
    idxs[!is.element(idxs, skip)]
  } else {
    idxs
  }
}

factorize_NSFG_variable <- function(formats_table, x, name, fill_na = TRUE) {
  if (fill_na) {
    nafill_value <- max(formats_table[format_name == name, as.integer(factor_value)]) + 1
    factor(
      nafill(x, fill = nafill_value),
      levels = c(
        formats_table[format_name == name, factor_value],
        nafill_value
      ),
      labels = c(
        formats_table[format_name == name, factor_label],
        'No answer provided'
      )
    )
  } else {
    factor(
      x,
      levels = formats_table[format_name == name, factor_value],
      labels = formats_table[format_name == name, factor_label]
    )
  }
}

set_NSFG_variables_as_factors <- function(raw_data, dt, formats_table, variables) {
  for (x in seq(length(variables))) {
    variable_name <- names(variables)[x]
    format_name <- variables[x]
    values <- factorize_NSFG_variable(
      formats_table,
      raw_data[, get(variable_name)],
      format_name
    )

    dt[, (variable_name) := ..values]
  }
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
  estimates[, description := factorize_NSFG_variable(formats_table, description, format)]
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
  estimates[, description := factorize_NSFG_variable(formats_table, description, format)]
  estimates
}

create_NSFG_pivot_table <- function(raw_data, prefix, series, id_vars = 'CASEID') {
  pivot_table <- create_pivot_table(prefix, series, id_vars, raw_data)
  setkeyv(pivot_table, id_vars)
}

century_month_comparison <- function(operator, x, y) {
  !is.na(x) &
    x != 9998 &
    x != 9999 &
    !is.na(y) &
    y != 9998 &
    y != 9999 &
    operator(x, y)
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

to_century_month <- function(x) {
  x_int <- as.integer(x)
  months <- factor(
    x_int %% 12,
    levels = 0:11,
    labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov')
  )
  years <- floor((x_int - 1) / 12) + 1900
  fifelse(
    x_int > 9000,
    fcase(
      x_int == 9997, 'Not ascertained',
      x_int == 9998, 'Refused',
      x_int == 9999, "Don't know"
    ),
    paste(as.character(months), years)
  )
}

names_to_century_month <- function(x) {
  names(x) <- to_century_month(names(x))
  x
}

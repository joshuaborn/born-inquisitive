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

get_NSFG_raw_import_for_sex <- function(sex) {
  if (startsWith(sex, 'f')) {
    fem1719
  } else {
    male1719
  }
}

get_NSFG_formats_for_sex <- function(sex) {
  if (startsWith(sex, 'f')) {
    setDT(fem1719formats)
  } else {
    setDT(male1719formats)
  }
}

get_NSFG_data_table_for_sex <- function(sex) {
  if (startsWith(sex, 'f')) {
    setDT(fem1719dt)
  } else {
    setDT(male1719dt)
  }
}

get_NSFG_survey_design_for_sex <- function(sex) {
  svydesign(
    ids = ~SECU,
    strata = ~SEST,
    data = get_NSFG_data_table_for_sex(sex),
    nest = TRUE,
    weights = ~WGT2017_2019
  )
}

get_NSFG_format_indices <- function(format, skip = NULL, sex = 'fem') {
  formats <- get_NSFG_formats_for_sex(sex)
  idxs <- formats[format_name == format, factor_value]
  if (!is.null(skip)) {
    idxs[!is.element(idxs, skip)]
  } else {
    idxs
  }
}

factorize_NSFG_variable <- function(x, name, sex = 'fem', fill_na = TRUE, formats_table = NULL) {
  if (is.null(formats_table)) {
    formats_table <- get_NSFG_formats_for_sex(sex)
  }
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

set_NSFG_variables_as_factors <- function(variables, sex = 'fem') {
  raw_import <- get_NSFG_raw_import_for_sex(sex)
  dt <- get_NSFG_data_table_for_sex(sex)

  for (x in seq(length(variables))) {
    variable_name <- names(variables)[x]
    format_name <- variables[x]
    values <- factorize_NSFG_variable(
      raw_import$Data[, get(variable_name)],
      format_name,
      sex
    )

    dt[, (variable_name) := ..values]
  }
}

set_NSFG_mentions_to_booleans <- function(
  out_prefix,
  in_prefix,
  in_series,
  format,
  skip = NULL,
  sex = 'fem'
) {
  set_mentions_to_booleans(
    in_dt = get_NSFG_raw_import_for_sex(sex)$Data,
    out_dt = get_NSFG_data_table_for_sex(sex),
    out_prefix = out_prefix,
    in_prefix = in_prefix,
    in_series = in_series,
    indices = get_NSFG_format_indices(format, skip, sex)
  )
}

estimate_NSFG_mentions <- function(
  survey_design,
  prefix,
  format,
  skip = NULL,
  sex = 'fem'
) {
  estimates <- estimate_mentions(
    survey_design,
    prefix, 
    get_NSFG_format_indices(format, skip, sex)
  )
  estimates[, description := factorize_NSFG_variable(description, format, sex)]
  estimates
}

estimate_NSFG_mentions_in_subdomains <- function(
  row_estimation_function,
  format,
  skip = NULL,
  sex = 'fem'
) {
  estimates <- estimate_mentions_in_subdomains(
    row_estimation_function,
    get_NSFG_format_indices(format, skip, sex)
  )
  estimates[, description := factorize_NSFG_variable(description, format, sex)]
  estimates
}

create_NSFG_pivot_table <- function(prefix, series, id_vars = 'CASEID', sex = 'fem') {
  raw_import <- get_NSFG_raw_import_for_sex(sex)
  pivot_table <- create_pivot_table(prefix, series, id_vars, raw_import$Data)
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

estimate_and_combine_NSFG_totals_and_percentages <- function(f, digits = 2) {
  style_and_combine_totals_and_percentages_vertically(
    'Females',
    estimate_totals_and_percentages(
      f,
      get_NSFG_survey_design_for_sex('fem')
    ),
    'Males',
    estimate_totals_and_percentages(
      f,
      get_NSFG_survey_design_for_sex('male')
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

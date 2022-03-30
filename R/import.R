library(data.table)
library(here)

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
     select = c('FMTNAME', 'START', 'LABEL'),
     key = c('FMTNAME', 'START')
   )
  )

  setnames(this_list[[2]], c('column_name', 'column_label'))
  setnames(this_list[[3]], c('format_name', 'factor_value', 'factor_label'))

  names(this_list) <- c('Data', 'Labels', 'Formats')

  this_list
}

factorize <- function(x, name, formats_table, fill_na = TRUE) {
  if (fill_na) {
    nafill_value <- max(formats_table[format_name == name, factor_value]) + 1
    factor(
      nafill(x, fill = nafill_value),
      levels = c(
        formats_table[format_name == name, factor_value],
        nafill_value
      ),
      labels = c(
        formats_table[format_name == name, factor_label],
        'Not Applicable'
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

cm_greater <- function(x, y) {
  is.na(x) &
    x != 9998 &
    x != 9999 &
    is.na(y) &
    y != 9998 &
    y != 9999 &
    x > y
}

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
  setnames(this_list[[3]], c('column_name', 'factor_value', 'factor_label'))

  names(this_list) <- c('Data', 'Labels', 'Formats')

  this_list
}

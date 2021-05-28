library(here)
library(readr)
library(stringr)

here::i_am(file.path('src', 'load_data.R'))


# This script loads the NSFG data into memory. See the README in for
# expectations.


get_list_of_col_names <- function(filename) {
  raw_string <- read_file(filename)
  digits_removed <- gsub('[[:space:]-]+[[:digit:]]+', '', raw_string)
  split_vector <- str_split(digits_removed, '[[:space:]]+')[[1]]
  split_vector[!grepl('^$', split_vector)]
}

get_list_of_col_widths <- function(filename) {
  raw_string <- read_file(filename)
  col_names_removed <- gsub('[[:alpha:]_]+[[:digit:]]*', '', raw_string)
  split_vector <- str_split(col_names_removed, '[[:space:]]+')[[1]]
  this_vector <- split_vector[!grepl('^$', split_vector)]
  start_end_list <- str_split(this_vector, '-')
  sapply(start_end_list, function(vec) {
    if (length(vec) == 1) {
      1
    } else if (length(vec) == 2) {
      as.integer(vec[2]) - as.integer(vec[1]) + 1
    } else {
      NA
    }
  })
}

load_nsfg_data_set <- function(filename, path = 'data') {
  data_definition_path <- here(path,
    paste(filename, 'vars', 'txt', sep = '.'))
  read_fwf(
    here(path, paste(filename, 'dat', sep = '.')),
    fwf_widths(
      widths = get_list_of_col_widths(data_definition_path),
      col_names = get_list_of_col_names(data_definition_path)
    ),
    col_types = cols(.default = col_number())
  )
}

load_nsfg_data <- function(year_string, data_set = NULL) {
  filename_sep <- '_'
  if (year_string == '2002') {
    filename_sep <- ''
  }
  if (is.null(data_set)) {
    data_sets <- c('FemRespData', 'FemPregData', 'MaleData')
    if (year_string == '2006_2010' || year_string =='2002') {
      data_sets <- sub('Data$', '', data_sets)
    }
    sapply(
      data_sets,
      function(data_string) {
        load_nsfg_data_set(paste(year_string, data_string, sep = filename_sep))
      }
    )
  } else {
    load_nsfg_data_set(paste(year_string, data_set, sep = filename_sep))
  }
}

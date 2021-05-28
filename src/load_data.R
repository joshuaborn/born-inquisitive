library(here)
library(readr)
library(stringr)

here::i_am(file.path('src', 'load_data.R'))


# This script loads the NSFG data into memory. See the README for expectations.


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

load_nsfg_data <- function(year_id, data_set = NULL) {
  year_id <- as.integer(year_id)
  years <- c(1973, 1976, 1982, 1988, 1995, 2002, 2010, 2013, 2015, 2017, 2019)
  if (!is.element(year_id, years)) {
    stop('There is no NSFG ending in year ', year_id, '.', sep = '')
  }
  filename_sep <- '_'
  if (year_id < 2010) {
    filename_sep <- ''
  }
  filename_suffix <- 'Data'
  if (year_id < 2013 && year_id > 1995) {
    filename_suffix <- ''
  }
  data_sets <- c('FemResp', 'Preg', 'Male')
  if (year_id < 2002) {
    data_sets <- c('FemResp', 'Preg')
  } else if (year_id < 1988) {
    data_sets <- c('NSFG')
  }
  if (!is.null(data_set)) {
    if (is.element(data_set, data_sets)) {
      data_sets <- c(data_set)
    } else {
      stop(
        paste(
          'There is no "',
          data_set,
          '" data set in the ',
          year_id,
          ' NSFG data. Try one of (',
          paste(data_sets, collapse = ', '),
          ') instead.',
          sep = ''
        )
      )
    }
  }
  year_strings <- list(
    '2019' = '2017_2019',
    '2017' = '2015_2017',
    '2015' = '2013_2015',
    '2013' = '2011_2013',
    '2010' = '2006_2010'
  )
  year_string <- as.character(year_id)
  if (is.element(year_string, names(year_strings))) {
    year_string <- year_strings[year_string]
  }
  data_set_helper <- function(data_set_string) {
    if (data_set_string == 'Preg' && year_id > 1995) {
      data_set_string <- 'FemPreg'
    }
    data_set_string <- paste(data_set_string, filename_suffix, sep = '')
    load_nsfg_data_set(
      paste(
        year_string,
        data_set_string,
        sep = filename_sep
      )
    )
  }
  if (length(data_sets) > 1) {
    sapply(data_sets, data_set_helper)
  } else {
    data_set_helper(data_set)
  }
}

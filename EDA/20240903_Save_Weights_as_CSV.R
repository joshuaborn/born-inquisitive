library(here)
library(readr)

input_files <- c(
  'adjusted_2006_2010_weights',
  'adjusted_2011_2015_weights',
  'adjusted_2015_2019_weights',
  'adjusted_cycle_3_weights',
  'adjusted_cycle_4_weights',
  'adjusted_cycle_5_weights',
  'adjusted_cycle_6_weights'
)

output_files <- c(
  'NSFG_2006_2010_weights',
  'NSFG_2011_2015_weights',
  'NSFG_2015_2019_weights',
  'NSFG_1982_1983_cycle_3_weights',
  'NSFG_1985_1987_cycle_4_weights',
  'NSFG_1995_cycle_5_weights',
  'NSFG_2002_2003_cycle_6_weights'
)

process_weights <- function(input_file, output_file) {
  readRDS(here(paste0('data/NSFG/', input_file, '.Rds'))) |>
    write_csv(
      here(paste0('blog/static/weights/', output_file, '.csv')),
      progress = TRUE
    )
}

for (i in 1:length(input_files)) {
  process_weights(input_files[i], output_files[i])
}

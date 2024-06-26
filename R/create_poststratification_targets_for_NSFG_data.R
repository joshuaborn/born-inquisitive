library(dplyr)
library(here)
library(readr)
library(tidyr)

here('R/create_postratification_targets_for_NSFG.R')


poststrat_pop_targets_2015_2019 <- readRDS(
  here('data/Census/prepared/census_for_2015_2019.Rds')
) |>
  filter(SEX_f == 'Female') |>
  mutate(
    poststrat_age_race = gsub(
      '[ -]',
      '_',
      paste0(
        gsub(' to | years', '_', AGEGROUP_f),
        race_ethn_f
      )
    )
  ) |>
  rename(Freq = pop) |>
  select(poststrat_age_race, Freq) |>
  as.data.frame()

poststrat_pop_total_2015_2019 <- sum(poststrat_pop_targets_2015_2019$Freq)

NVSS_births_2011_2014 <- read_tsv(
  here('data/NVSS/2011-2014/age.txt'),
  name_repair = 'universal',
  show_col_types = FALSE
) |>
  filter(Notes == 'Total' & !is.na(Year)) |>
  rename(Yes = Births) |>
  mutate(
    variable_name = paste0('birth_in_', Year),
    No = poststrat_pop_total_2015_2019 - Yes
  ) |>
  select(variable_name, Yes, No) |>
  pivot_longer(c(Yes, No), values_to = 'Freq')

poststrat_births_targets_2011_2014 <- NVSS_births_2011_2014 |>
  pull(variable_name) |>
  unique() |>
  lapply(function(this_variable_name) {
    this_df <- NVSS_births_2011_2014 |>
      filter(variable_name == this_variable_name) |>
      select(name, Freq) |>
      as.data.frame()
    colnames(this_df) <- c(this_variable_name, 'Freq')
    this_df
  })

poststrat_targets_2011_2014 <- c(
  list(poststrat_pop_targets_2015_2019),
  poststrat_births_targets_2011_2014
)

saveRDS(
  poststrat_targets_2011_2014,
  file = here('data/poststrat_targets_2015_2019.Rds')
)

guttmacher_APC_national <- read_csv(
    here('data/Guttmacher/NationalAndStatePregnancy_PublicUse.csv'),
    show_col_types = FALSE
  ) |>
  filter(state == 'US') |>
  mutate(interpolated = year %in%
      c(1983, 1986, 1989, 1990, 1993, 1994, 1997, 1998, 2001, 2002, 2003, 2006,
        2009, 2012, 2015)
  ) |>
  select('year', 'interpolated', 'abortionstotal', 'abortionslt20',
    'abortions2024', 'abortions2529', 'abortions3034', 'abortions3539',
    'abortions40plus')

saveRDS(
  guttmacher_APC_national,
  file = here('data/guttmacher_APC_national.Rds')
)

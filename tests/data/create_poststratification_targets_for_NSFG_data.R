library(dplyr)
library(here)
library(readr)
library(tidyr)

here('tests/data/create_postratification_targets_for_NSFG.R')


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


NVSS_age_2011_2014 <- read_tsv(
    here('data/NVSS/2011-2014/age.txt'),
    name_repair = 'universal',
    show_col_types = FALSE
  ) |>
  filter(!is.na(Year) | !is.na(Age.of.Mother.9)) |>
  mutate(
    Variable = paste0('birth_', Year, '_age'),
    Level = factor(
      case_when(
        Age.of.Mother.9.Code %in% c('15', '15-19') ~ 'Under 20 years',
        Age.of.Mother.9.Code %in% c('40-44', '45-49', '50+') ~ '40 years and over',
        is.na(Age.of.Mother.9.Code) ~ 'Other',
        .default = Age.of.Mother.9
      ),
      levels = c(
        'Under 20 years',
        '20-24 years',
        '25-29 years',
        '30-34 years',
        '35-39 years',
        '40 years and over',
        'Other'
      ),
      ordered = TRUE
    ),
    Freq = if_else(
      !is.na(Notes) & Notes == 'Total',
      poststrat_pop_total_2015_2019 - Births,
      Births
    )
  ) |>
  group_by(Variable, Level) |>
  summarize(Freq = sum(Freq), .groups = 'drop') |>
  arrange(Variable, Level) |>
  group_by(Variable) |>
  group_split() |>
  lapply(function(tbl) {
    variable_name <- first(pull(tbl, Variable))
    tbl <- select(tbl, -Variable)
    names(tbl) <- c(variable_name, 'Freq')
    tbl
  })


NVSS_marital_status_2011_2014 <- read_tsv(
    here('data/NVSS/2011-2014/marital_status.txt'),
    name_repair = 'universal',
    show_col_types = FALSE
  ) |>
  filter(!is.na(Year) | !is.na(Marital.Status)) |>
  mutate(
    Variable = paste0('birth_', Year, '_marital_status'),
    Level = factor(
      if_else(
        !is.na(Marital.Status),
        Marital.Status,
        'Other'
      ),
      levels = c(
        'Married',
        'Unmarried',
        'Other'
      ),
      ordered = TRUE
    ),
    Freq = if_else(
      !is.na(Notes) & Notes == 'Total',
      poststrat_pop_total_2015_2019 - Births,
      Births
    )
  ) |>
  select(Variable, Level, Freq) |>
  group_by(Variable) |>
  group_split() |>
  lapply(function(tbl) {
    variable_name <- first(pull(tbl, Variable))
    tbl <- select(tbl, -Variable)
    names(tbl) <- c(variable_name, 'Freq')
    tbl
  })


guttmacher_APC_national <- read_csv(
    here('data/Guttmacher/NationalAndStatePregnancy_PublicUse.csv'),
    show_col_types = FALSE
  ) |>
  filter(state == 'US') |>
  mutate(interpolated = year %in%
      c(1983, 1986, 1989, 1990, 1993, 1994, 1997, 1998, 2001, 2002, 2003, 2006,
        2009, 2012, 2015)
  ) |>
  select('year', 'interpolated', 'abortionslt20', 'abortions2024',
    'abortions2529', 'abortions3034', 'abortions3539', 'abortions40plus',
    'abortionstotal')


guttmacher_APC_2013_2014_age <- guttmacher_APC_national |>
  filter(year %in% 2013:2014) |>
  rename(
    `Other` = abortionstotal,
    `Under 20 years` = abortionslt20,
    `20-24 years` = abortions2024,
    `25-29 years` = abortions2529,
    `30-34 years` = abortions3034,
    `35-39 years` = abortions3539,
    `40 years and over` = abortions40plus
  ) |>
  summarize(across(3:9, sum)) |>
  pivot_longer(
    cols = everything(),
    names_to = 'abortion_2013_2014_age',
    values_to = 'Freq'
  ) |>
  mutate(
    abortion_2013_2014_age = factor(
      abortion_2013_2014_age,
      levels = c(
        'Under 20 years',
        '20-24 years',
        '25-29 years',
        '30-34 years',
        '35-39 years',
        '40 years and over',
        'Other'
      ),
      ordered = TRUE
    ),
    Freq = if_else(
      abortion_2013_2014_age == 'Other',
      poststrat_pop_total_2015_2019 - Freq,
      Freq
    )
  ) |>
  arrange(abortion_2013_2014_age)


poststrat_targets_2015_2019 <- c(
  list(poststrat_pop_targets_2015_2019),
  NVSS_age_2011_2014,
  NVSS_marital_status_2011_2014,
  list(guttmacher_APC_2013_2014_age)
)


saveRDS(
  poststrat_targets_2015_2019,
  file = here('data/poststrat_targets_2015_2019.Rds')
)

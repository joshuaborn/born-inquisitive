library(here)
library(srvyr)

source(here('tests/data/simple_survey.R'))


get_female_population_count <- function() {
  readRDS(
    here('data/Census/prepared/census_for_2015_2019.Rds')
  ) |>
    filter(SEX_f == 'Female') |>
    pull(pop) |>
    sum()
}


get_target_totals <- function() {
  tbl <- readRDS(here('data/guttmacher_APC_national.Rds')) |>
    filter(year %in% 2013:2014) |>
    summarize(across(starts_with('abortions'), sum)) |>
    select(-abortionstotal) |>
    rename(
      `Under 20 years` = abortionslt20,
      `20-24 years` = abortions2024,
      `25-29 years` = abortions2529,
      `30-34 years` = abortions3034,
      `35-39 years` = abortions3539,
      `40 years and over` = abortions40plus
    ) |>
    pivot_longer(
      everything(),
      names_to = 'abortion_2013_2014_age',
      values_to = 'Freq'
    )

  tbl |>
    bind_rows(
      data.frame(
        abortion_2013_2014_age = 'Other',
        Freq = get_female_population_count() - sum(pull(tbl, Freq))
      )
    ) |>
    mutate(
      abortion_2013_2014_age = factor(
        abortion_2013_2014_age,
        ordered = TRUE,
        levels = c(
          'Under 20 years',
          '20-24 years',
          '25-29 years',
          '30-34 years',
          '35-39 years',
          '40 years and over',
          'Other'
        )
      )
    )
}


test_that('only svyrep.design objects are accepted', {
  test_data <- get_simple_survey_test_data()

  expect_error(postStratify2(test_data$df), 'svyrep.design')
  expect_error(postStratify2(test_data$svy), 'svyrep.design')
  expect_no_error(
    postStratify2(
      readRDS(here('data/NSFG_2015_2019_fem_svy.Rds')),
      ~abortion_2013_2014_age,
      get_target_totals(),
      ~abortion_age
    )
  )
})


test_that('a svyrep.design object is returned', {
  expect_s3_class(
    postStratify2(
      readRDS(here('data/NSFG_2015_2019_fem_svy.Rds')),
      ~abortion_2013_2014_age,
      get_target_totals(),
      ~abortion_age
    ),
    'svyrep.design'
  )
})


test_that('output data have same number of rows as input', {
  original_svy <- readRDS(here('data/NSFG_2015_2019_fem_svy.Rds'))
  adjusted_svy <- postStratify2(
    original_svy,
    ~abortion_2013_2014_age,
    get_target_totals(),
    ~abortion_age
  )

  expect_equal(
    nrow(adjusted_svy),
    nrow(original_svy)
  )
})


test_that('each stratum level has its weights adjusted by the appropriate factor', {
  original_svy <- readRDS(here('data/NSFG_2015_2019_fem_svy.Rds'))

  original_est_adj <- original_svy |>
    as_survey_rep() |>
    group_by(abortion_2013_2014_age) |>
    summarize(Freq = survey_total(vartype = NULL))

  target_totals <- get_target_totals()

  adj_factors <- original_est_adj |>
    inner_join(
      target_totals,
      by = join_by(abortion_2013_2014_age),
      suffix = c('_original', '_target')
    ) |>
    mutate(adj_factor = Freq_target / Freq_original) |>
    select(abortion_2013_2014_age, adj_factor)

  pop_targets <- original_svy |>
    as_survey_rep() |>
    group_by(abortion_age) |>
    summarize(Freq = survey_total(vartype = NULL)) |>
    inner_join(
      adj_factors |>
        rename(abortion_age = abortion_2013_2014_age),
      by = join_by(abortion_age)
    ) |>
    mutate(
      pop_target = Freq * adj_factor
    )

  adjusted_svy <- postStratify2(
    original_svy,
    ~abortion_2013_2014_age,
    target_totals,
    ~abortion_age
  )

  expect_equal(
    adjusted_svy |>
      as_survey_rep() |>
      group_by(abortion_age) |>
      summarize(Freq = survey_total(vartype = NULL)),
    pop_targets |>
      select(abortion_age, pop_target) |>
      rename(Freq = pop_target)
  )
})


test_that('the adjustment factor is recorded in the result', {
  adjusted_svy <- postStratify2(
    readRDS(here('data/NSFG_2015_2019_fem_svy.Rds')),
    ~abortion_2013_2014_age,
    get_target_totals(),
    ~abortion_age
  )

  expect_snapshot_value(
    adjusted_svy$adj_factors,
    style = 'serialize'
  )
})


test_that('different levels in targets and adj_factor_strata results in an error', {
  expect_error(
    postStratify2(
      readRDS(here('data/NSFG_2015_2019_fem_svy.Rds')),
      ~abortion_2013_2014_age,
      get_target_totals()[-2,],
      ~abortion_age
    ),
    'The levels of the variable denoted by adj_factor_strata must be identical to the levels in the targets data frame'
  )
})

test_that('different levels in weight_adj_strata and adj_factor_strata results in an error', {
  expect_error(
    postStratify2(
      readRDS(here('data/NSFG_2015_2019_fem_svy.Rds')),
      ~abortion_2013_2014_age,
      get_target_totals(),
      ~birth_2014_marital_status
    ),
    'The levels of the variable denoted by adj_factor_strata must be identical to the levels of the variable denoted by weight_adj_strata'
  )
})

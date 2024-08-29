library(dplyr)
library(here)
library(srvyr)
library(survey)

source(here('tests/data/simple_survey.R'))


get_target_totals <- function() {
  poststrat_targets <- readRDS(here('data/poststrat_targets_2015_2019.Rds'))
  poststrat_targets[[length(poststrat_targets)]]
}


test_that('only svyrep.design objects are accepted', {
  test_data <- get_simple_survey_test_data()

  expect_error(postStratify2(test_data$df), 'svyrep.design')
  expect_error(postStratify2(test_data$svy), 'svyrep.design')
  expect_no_error(
    postStratify2(
      readRDS(here('data/NSFG_2015_2019_fem_svy.Rds')),
      ~abortion_2011_2014_age,
      get_target_totals(),
      ~abortion_age
    )
  )
})


test_that('a svyrep.design object is returned', {
  expect_s3_class(
    postStratify2(
      readRDS(here('data/NSFG_2015_2019_fem_svy.Rds')),
      ~abortion_2011_2014_age,
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
    ~abortion_2011_2014_age,
    get_target_totals(),
    ~abortion_age
  )

  expect_equal(
    nrow(adjusted_svy),
    nrow(original_svy)
  )
})


test_that('each replicate of weights is post-stratified separately', {
  original_svy <- readRDS(here('data/NSFG_2015_2019_fem_svy.Rds'))
  adjusted_svy <- postStratify2(
    original_svy,
    ~abortion_2011_2014_age,
    get_target_totals(),
    ~abortion_age
  )

  expect(
    !all(weights(adjusted_svy, 'replication') %in% c(0, 2)),
    'not all replicate weight factors should still be 0 or 2 after post-stratification'
  )
})


test_that('each stratum level has its weights adjusted by the appropriate factor', {
  original_svy <- readRDS(here('data/NSFG_2015_2019_fem_svy.Rds'))

  original_est_adj <- original_svy |>
    as_survey_rep() |>
    group_by(abortion_2011_2014_age) |>
    summarize(Freq = survey_total(vartype = NULL))

  target_totals <- get_target_totals()

  adj_factors <- original_est_adj |>
    inner_join(
      target_totals,
      by = join_by(abortion_2011_2014_age),
      suffix = c('_original', '_target')
    ) |>
    mutate(adj_factor = Freq_target / Freq_original) |>
    select(abortion_2011_2014_age, adj_factor)

  pop_targets <- original_svy |>
    as_survey_rep() |>
    group_by(abortion_age) |>
    summarize(Freq = survey_total(vartype = NULL)) |>
    inner_join(
      adj_factors |>
        rename(abortion_age = abortion_2011_2014_age),
      by = join_by(abortion_age)
    ) |>
    mutate(
      pop_target = Freq * adj_factor
    )

  adjusted_svy <- postStratify2(
    original_svy,
    ~abortion_2011_2014_age,
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
    ~abortion_2011_2014_age,
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
      ~abortion_2011_2014_age,
      get_target_totals()[-2,],
      ~abortion_age
    ),
    'The levels specified in the data set and those specified target totals must be identical'
  )
})


test_that('different levels in weight_adj_strata and adj_factor_strata results in an error', {
  expect_error(
    postStratify2(
      readRDS(here('data/NSFG_2015_2019_fem_svy.Rds')),
      ~abortion_2011_2014_age,
      get_target_totals(),
      ~HISPRACE2
    ),
    'The levels of the variable denoted by adj_factor_strata must be identical to the levels of the variable denoted by weight_adj_strata'
  )
})

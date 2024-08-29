library(dplyr)
library(here)
library(srvyr)

source(here('tests/data/simple_survey.R'))
source(here('R/rake2.R'))


test_that('only svyrep.design objects are accepted', {
  test_data <- get_simple_survey_test_data()

  targets <- list(
    data.frame(
      cat1 = paste0('level', 1:4),
      Freq = c(2150, 2500, 1825, 2200)
    )
  )

  expect_error(
    rake2(
      design = test_data$df,
      sample.margins = list(~cat1),
      population.margins = targets
    ),
    'svyrep.design'
  )
  expect_error(
    rake2(
      design = test_data$svy,
      sample.margins = list(~cat1),
      population.margins = targets
    ),
    'svyrep.design'
  )
  expect_no_error(
    rake2(
      design = test_data$rep_svy,
      sample.margins = list(~cat1),
      population.margins = targets
    )
  )
})


test_that('a svyrep.design object is returned', {
  test_data <- get_simple_survey_test_data()

  targets <- list(
    data.frame(
      cat1 = paste0('level', 1:4),
      Freq = c(2150, 2500, 1825, 2200)
    )
  )

  expect_s3_class(
    rake2(
      design = test_data$rep_svy,
      sample.margins = list(~cat1),
      population.margins = targets
    ),
    'svyrep.design'
  )
})


test_that('estimates equal provided population margins', {

  NSFG_2015_2019_fem_svy <- readRDS(here('data/NSFG_2015_2019_fem_svy.Rds'))

  all_poststrat_targets <- readRDS(here('data/poststrat_targets_2015_2019.Rds'))

  poststrat_targets <- c(
    all_poststrat_targets[1],
    all_poststrat_targets[2]
  )

  raked_svy <- rake2(
    design = NSFG_2015_2019_fem_svy,
    sample.margins = lapply(
      poststrat_targets,
      \(x) as.formula(paste0('~', colnames(x)[1]))
    ),
    population.margins = poststrat_targets,
    control = list(maxit=10, epsilon=1, verbose=FALSE)
  )

  expect(!any(is.nan(weights(raked_svy, 'analysis'))), 'some weights have NaN values after adjustment')

  for (i in 1:length(poststrat_targets)) {
    this_target <- poststrat_targets[[i]]
    this_join_column <- colnames(this_target)[1]

    raked_svy |>
      as_survey_rep() |>
      group_by(get(this_join_column)) |>
      summarize(Est = survey_total(one, vartype = 'ci')) |>
      full_join(
        this_target,
        by = c("get(this_join_column)" = this_join_column)
      ) |>
      (\(x) expect_equal(x$Est, x$Freq))()
  }

})

test_that('weight adjustment is done on alternative strata definition, if provided', {

  NSFG_2015_2019_fem_svy <- readRDS(here('data/NSFG_2015_2019_fem_svy.Rds'))

  all_poststrat_targets <- readRDS(here('data/poststrat_targets_2015_2019.Rds'))

  poststrat_targets <- c(
    all_poststrat_targets[1],
    all_poststrat_targets[length(all_poststrat_targets)]
  )

  raked_svy <- rake2(
    design = NSFG_2015_2019_fem_svy,
    sample.margins = lapply(
      poststrat_targets,
      \(x) as.formula(paste0('~', colnames(x)[1]))
    ),
    population.margins = poststrat_targets,
    weight_adj_strata = list(NULL, ~abortion_age),
    control = list(maxit=20, epsilon=1, verbose=FALSE)
  )

  expect(
    !any(is.nan(weights(raked_svy, 'analysis'))),
    'some weights have NaN values after adjustment'
  )

  for (i in 1:length(poststrat_targets)) {
    this_target <- poststrat_targets[[i]]
    this_join_column <- colnames(this_target)[1]

    raked_svy |>
      as_survey_rep() |>
      group_by(get(this_join_column)) |>
      summarize(Est = survey_total(one, vartype = 'ci')) |>
      full_join(
        this_target,
        by = c("get(this_join_column)" = this_join_column)
      ) |>
      (\(x) expect_equal(x$Est, x$Freq, tolerance = 3))()
  }

  unraked_estimates <- NSFG_2015_2019_fem_svy |>
    as_survey_rep() |>
    group_by(abortion_age) |>
    summarize(Est = survey_total(one, vartype = 'se')) |>
    pull(Est)

  raked_estimates_df <- raked_svy |>
    as_survey_rep() |>
    group_by(abortion_age) |>
    summarize(Est = survey_total(one, vartype = 'se'))

  raked_estimates <- pull(raked_estimates_df, Est)

  expect(
    all(head(raked_estimates, -1) / head(unraked_estimates, -1) > 1.7),
    'one or more strata have estimates that did not increase substantially due to raking'
  )

})

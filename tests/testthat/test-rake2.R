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
    all_poststrat_targets[2],
    all_poststrat_targets[3],
    all_poststrat_targets[4],
    all_poststrat_targets[5],
    all_poststrat_targets[10]
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

  raked_svy |>
    as_survey_rep() |>
    group_by(poststrat_age_race) |>
    summarize(Est = survey_total(one, vartype = 'ci')) |>
    full_join(
      all_poststrat_targets[[1]],
      by = join_by(poststrat_age_race)
    ) |>
    (\(x) expect_equal(x$Est, x$Freq))()

  raked_svy |>
    as_survey_rep() |>
    group_by(abortion_2013_2014_age) |>
    summarize(Est = survey_total(one, vartype = 'ci')) |>
    full_join(
      all_poststrat_targets[[10]],
      by = join_by(abortion_2013_2014_age)
    ) |>
    (\(x) expect_equal(x$Est, x$Freq))()

})


#test_that('the adjusted estimate for each stratum is roughly equal to the adjustment factor times the original estimates', {
#
#  NSFG_2015_2019_fem_svy <- readRDS(here('data/NSFG_2015_2019_fem_svy.Rds'))
#
#  postrat_targets <- readRDS(here('data/poststrat_targets_2015_2019.Rds'))
#
#  guttmacher_APC_national <- readRDS(here('data/guttmacher_APC_national.Rds'))
#
#  abortions_target <- guttmacher_APC_national |>
#    filter(year %in% 2013:2014) |>
#    pull(abortionstotal) |>
#    sum()
#
#  raked_svy <- rake2(
#    design = NSFG_2015_2019_fem_svy,
#    sample.margins = list(
#      ~poststrat_age_race,
#      ~birth_in_2011,
#      ~birth_in_2012,
#      ~birth_in_2013,
#      ~birth_in_2014
#    ),
#    population.margins = postrat_targets,
#    factor.estimation.vars = ~abortions_in_2013 + abortions_in_2014,
#    factor.estimation.target = abortions_target,
#    factor.adjusted.vars = ~any_abortion,
#    control = list(maxit=100, epsilon=1, verbose=FALSE)
#  )
#
#  weight_adjustments <- raked_svy$pweights[
#      pull(NSFG_2015_2019_fem_svy$variables, any_abortion)
#    ] /
#    NSFG_2015_2019_fem_svy$pweights[
#      pull(NSFG_2015_2019_fem_svy$variables, any_abortion)
#    ]
#
#  expect_lt(
#    mean(weight_adjustments) - 3 * sd(weight_adjustments),
#    prod(raked_svy$adj_factors)
#  )
#  expect_gt(
#    mean(weight_adjustments) + 3 * sd(weight_adjustments),
#    prod(raked_svy$adj_factors)
#  )
#
#})

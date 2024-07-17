library(here)
library(srvyr)

source(here('tests/data/simple_survey.R'))
source(here('R/deprecated/rake_with_factor_adjustment.R'))


test_that('only svyrep.design objects are accepted', {
  test_data <- get_simple_survey_test_data()

  targets <- list(
    data.frame(
      cat1 = paste0('level', 1:4),
      Freq = c(2150, 2500, 1825, 2200)
    )
  )

  expect_error(adjust_weights_by_factor(test_data$df), 'svyrep.design')
  expect_error(adjust_weights_by_factor(test_data$svy), 'svyrep.design')
  expect_no_error(
    rake_with_factor_adjustment(
      design = test_data$rep_svy,
      sample.margins = list(~cat1),
      population.margins = targets,
      factor.estimation.vars = ~count1 + count2,
      factor.estimation.target = 4100,
      factor.adjusted.vars = ~count1 + count2 + count3 + count4
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
    rake_with_factor_adjustment(
      design = test_data$rep_svy,
      sample.margins = list(~cat1),
      population.margins = targets,
      factor.estimation.vars = ~count1 + count2,
      factor.estimation.target = 4100,
      factor.adjusted.vars = ~count1 + count2 + count3 + count4
    ),
    'svyrep.design'
  )
})


test_that('estimated totals corresponding to of each level of categorical variables used in raking match target totals', {

  NSFG_2015_2019_fem_svy <- readRDS(here('data/NSFG_2015_2019_fem_svy.Rds'))

  postrat_targets <- readRDS(here('data/poststrat_targets_2015_2019.Rds'))

  guttmacher_APC_national <- readRDS(here('data/guttmacher_APC_national.Rds'))

  abortions_target <- guttmacher_APC_national |>
    filter(year %in% 2013:2014) |>
    pull(abortionstotal) |>
    sum()

  raked_svy <- rake_with_factor_adjustment(
    design = NSFG_2015_2019_fem_svy,
    sample.margins = list(
      ~poststrat_age_race,
      ~birth_in_2011,
      ~birth_in_2012,
      ~birth_in_2013,
      ~birth_in_2014
    ),
    population.margins = postrat_targets,
    factor.estimation.vars = ~abortions_in_2013 + abortions_in_2014,
    factor.estimation.target = abortions_target,
    factor.adjusted.vars = ~any_abortion,
    control = list(maxit=100, epsilon=1, verbose=FALSE)
  )

  raked_svy |>
    as_survey_rep() |>
    group_by(poststrat_age_race) |>
    summarize(Est = survey_total(one, vartype = 'ci')) |>
    full_join(postrat_targets[[1]], by = join_by(poststrat_age_race)) |>
    (\(x) expect_equal(x$Est, x$Freq))()

  raked_svy |>
    as_survey_rep() |>
    group_by(birth_in_2011) |>
    summarize(Est = survey_total(one, vartype = 'ci')) |>
    full_join(postrat_targets[[2]], by = join_by(birth_in_2011)) |>
    (\(x) expect_equal(x$Est, x$Freq))()

  raked_svy |>
    as_survey_rep() |>
    group_by(birth_in_2012) |>
    summarize(Est = survey_total(one, vartype = 'ci')) |>
    full_join(postrat_targets[[3]], by = join_by(birth_in_2012)) |>
    (\(x) expect_equal(x$Est, x$Freq))()

  raked_svy |>
    as_survey_rep() |>
    group_by(birth_in_2013) |>
    summarize(Est = survey_total(one, vartype = 'ci')) |>
    full_join(postrat_targets[[4]], by = join_by(birth_in_2013)) |>
    (\(x) expect_equal(x$Est, x$Freq))()

  raked_svy |>
    as_survey_rep() |>
    group_by(birth_in_2014) |>
    summarize(Est = survey_total(one, vartype = 'ci')) |>
    full_join(postrat_targets[[5]], by = join_by(birth_in_2014)) |>
    (\(x) expect_equal(x$Est, x$Freq))()

})


test_that('the estimated total for the factor adjustment target equals the target total', {

  NSFG_2015_2019_fem_svy <- readRDS(here('data/NSFG_2015_2019_fem_svy.Rds'))

  postrat_targets <- readRDS(here('data/poststrat_targets_2015_2019.Rds'))

  guttmacher_APC_national <- readRDS(here('data/guttmacher_APC_national.Rds'))

  abortions_target <- guttmacher_APC_national |>
    filter(year %in% 2013:2014) |>
    pull(abortionstotal) |>
    sum()

  raked_svy <- rake_with_factor_adjustment(
    design = NSFG_2015_2019_fem_svy,
    sample.margins = list(
      ~poststrat_age_race,
      ~birth_in_2011,
      ~birth_in_2012,
      ~birth_in_2013,
      ~birth_in_2014
    ),
    population.margins = postrat_targets,
    factor.estimation.vars = ~abortions_in_2013 + abortions_in_2014,
    factor.estimation.target = abortions_target,
    factor.adjusted.vars = ~any_abortion,
    control = list(maxit=100, epsilon=1, verbose=FALSE)
  )

  expect_equal(
    sum(svytotal(~abortions_in_2013 + abortions_in_2014, raked_svy)),
    abortions_target,
    tolerance = 0.00001
  )

})


test_that('the mean weight adjustment of factor adjusted observations is within three standard deviations of the target', {

  NSFG_2015_2019_fem_svy <- readRDS(here('data/NSFG_2015_2019_fem_svy.Rds'))

  postrat_targets <- readRDS(here('data/poststrat_targets_2015_2019.Rds'))

  guttmacher_APC_national <- readRDS(here('data/guttmacher_APC_national.Rds'))

  abortions_target <- guttmacher_APC_national |>
    filter(year %in% 2013:2014) |>
    pull(abortionstotal) |>
    sum()

  raked_svy <- rake_with_factor_adjustment(
    design = NSFG_2015_2019_fem_svy,
    sample.margins = list(
      ~poststrat_age_race,
      ~birth_in_2011,
      ~birth_in_2012,
      ~birth_in_2013,
      ~birth_in_2014
    ),
    population.margins = postrat_targets,
    factor.estimation.vars = ~abortions_in_2013 + abortions_in_2014,
    factor.estimation.target = abortions_target,
    factor.adjusted.vars = ~any_abortion,
    control = list(maxit=100, epsilon=1, verbose=FALSE)
  )

  weight_adjustments <- raked_svy$pweights[
      pull(NSFG_2015_2019_fem_svy$variables, any_abortion)
    ] /
    NSFG_2015_2019_fem_svy$pweights[
      pull(NSFG_2015_2019_fem_svy$variables, any_abortion)
    ]

  expect_lt(
    mean(weight_adjustments) - 3 * sd(weight_adjustments),
    prod(raked_svy$adj_factors)
  )
  expect_gt(
    mean(weight_adjustments) + 3 * sd(weight_adjustments),
    prod(raked_svy$adj_factors)
  )

})

library(here)
library(srvyr)

source(here('tests/data/simple_survey.R'))


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
    rake_with_factor_adjustment(
      design = test_data$rep_svy,
      sample.margins = list(~cat1),
      population.margins = targets
    ),
    'svyrep.design'
  )
})


test_that('estimated totals corresponding to of each level of categorical variables used in raking match target totals', {

  NSFG_2015_2019_fem_svy <- readRDS(here('data/NSFG_2015_2019_fem_svy.Rds'))

  postrat_targets <- readRDS(here('data/poststrat_targets_2015_2019.Rds'))

  raked_svy <- rake_with_factor_adjustment(
    design = NSFG_2015_2019_fem_svy,
    sample.margins = list(
      ~poststrat_age_race,
      ~birth_in_2011,
      ~birth_in_2012,
      ~birth_in_2013,
      ~birth_in_2014
    ),
    population.margins = postrat_targets
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

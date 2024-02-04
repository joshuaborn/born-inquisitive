library(here)

source(here('tests/data/simple_survey.R'))


test_that('only svyrep.design objects are accepted', {
  test_data <- get_simple_survey_test_data()

  expect_error(adjust_weights_by_factor(test_data$df), 'svyrep.design')
  expect_error(adjust_weights_by_factor(test_data$svy), 'svyrep.design')
  expect_no_error(
    adjust_weights_by_factor(
      test_data$rep_svy,
      ~count1 + count2 + count3 + count4 + count5 + count6,
      1.3
    )
  )
})


test_that('a svyrep.design object is returned', {
  test_data <- get_simple_survey_test_data()
  adjusted_svy <- adjust_weights_by_factor(
    test_data$rep_svy,
    ~count1 + count2 + count3 + count4 + count5 + count6,
    1.3
  )

  expect_contains(
    class(adjusted_svy),
    'svyrep.design'
  )
})


test_that('output data have same number of rows as input', {
  test_data <- get_simple_survey_test_data()
  adjusted_svy <- adjust_weights_by_factor(
    test_data$rep_svy,
    ~count1 + count2 + count3 + count4 + count5 + count6,
    1.3
  )

  expect_equal(
    nrow(adjusted_svy),
    nrow(test_data$rep_svy)
  )
})


test_that('observations that match the predicate have their weights adjusted', {
  adjustment_factor <- 1.3
  test_data <- get_simple_survey_test_data()
  adjusted_svy <- adjust_weights_by_factor(
    test_data$rep_svy,
    ~count1 + count2 + count3 + count4 + count5 + count6,
    adjustment_factor
  )

  predicate_columns <- grep('count', colnames(test_data$df))

  for (i in 1:nrow(adjusted_svy)) {
    predicate_sum <- sum(test_data$df[i, predicate_columns])
    if (predicate_sum > 0) {
      expect_equal(
        weights(adjusted_svy, type = 'sampling')[i],
        adjustment_factor * weights(test_data$rep_svy, type = 'sampling')[i]
      )
      expect_equal(
        weights(adjusted_svy, type = 'analysis')[i,],
        adjustment_factor * weights(test_data$rep_svy, type = 'analysis')[i,]
      )
    }
  }
})


test_that('sum of all weights before and after adjustment is equal', {
  adjustment_factor <- 1.3

  test_data <- get_simple_survey_test_data()

  adjusted_svy <- adjust_weights_by_factor(
    test_data$rep_svy,
    ~count1 + count2 + count3 + count4 + count5 + count6,
    adjustment_factor
  )

  expect_equal(
    sum(weights(adjusted_svy, type = 'sampling')),
    sum(weights(test_data$rep_svy, type = 'sampling'))
  )
})


test_that('the adjustment factor is recorded in the ', {
  test_data <- get_simple_survey_test_data()

  adjusted_svy <- adjust_weights_by_factor(
    test_data$rep_svy,
    ~count1 + count2 + count3 + count4 + count5 + count6,
    1.3
  )

  expect_equal(last(adjusted_svy$adj_factor), 1.3)

  readjusted_svy <- adjust_weights_by_factor(
    adjusted_svy,
    ~count1 + count2 + count3 + count4 + count5 + count6,
    0.7
  )

  expect_equal(last(readjusted_svy$adj_factor), 0.7)
})

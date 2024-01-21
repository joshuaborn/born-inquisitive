library(here)

source(here('tests/data/simple_survey.R'))


test_that('only survey.design objects are accepted', {
  test_data <- get_simple_survey_test_data()

  expect_error(find_ratio_for_poststratification(test_data$df), 'survey.design')
  expect_no_error(
    find_ratio_for_poststratification(test_data$svy, ~count1 + count2, 10000)
  )
  expect_no_error(
    find_ratio_for_poststratification(test_data$rep_svy, ~count1 + count2, 10000)
  )
})


test_that('returned ratio results in estimate that equal target total', {
  test_data <- get_simple_survey_test_data()
  total <- 10000

  expect_equal(
    total / sum(svytotal(~count1 + count2, test_data$svy)),
    find_ratio_for_poststratification(test_data$svy, ~count1 + count2, 10000)
  )
  expect_equal(
    total / sum(svytotal(~count1 + count2, test_data$rep_svy)),
    find_ratio_for_poststratification(test_data$svy, ~count1 + count2, 10000)
  )
})

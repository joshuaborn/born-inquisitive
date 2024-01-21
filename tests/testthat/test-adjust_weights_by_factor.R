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
      2.5
    )
  )
})


test_that('a svyrep.design object is returned', {
  test_data <- get_simple_survey_test_data()
  adjusted_svy <- adjust_weights_by_factor(
    test_data$rep_svy,
    ~count1 + count2 + count3 + count4 + count5 + count6,
    2.5
  )

  expect_contains(class(adjusted_svy), 'svyrep.design')
})


test_that('output data have same number of rows as input', {
  test_data <- get_simple_survey_test_data()
  adjusted_svy <- adjust_weights_by_factor(
    test_data$rep_svy,
    ~count1 + count2 + count3 + count4 + count5 + count6,
    2.5
  )
  df_after <- as_data_frame_with_weights(
    adjusted_svy,
    full_wgt_name = 'FULL_SAMPLE_WGT',
    rep_wgt_prefix = 'REP_WGT_'
  )

  expect_equal(
    nrow(df_after),
    nrow(test_data$df)
  )
})


test_that('observations that match the predicate have their weights adjusted', {
  adjustment_factor <- 2.5
  test_data <- get_simple_survey_test_data()
  df_before <- as_data_frame_with_weights(
    test_data$rep_svy,
    full_wgt_name = 'FULL_SAMPLE_WGT',
    rep_wgt_prefix = 'REP_WGT_'
  )
  adjusted_svy <- adjust_weights_by_factor(
    test_data$rep_svy,
    ~count1 + count2 + count3 + count4 + count5 + count6,
    adjustment_factor
  )
  df_after <- as_data_frame_with_weights(
    adjusted_svy,
    full_wgt_name = 'FULL_SAMPLE_WGT',
    rep_wgt_prefix = 'REP_WGT_'
  )

  weight_columns_before <- grep('_WGT', colnames(df_before))
  weight_columns_after <- grep('_WGT', colnames(df_after))
  predicate_columns <- grep('count', colnames(df_before))

  for (i in 1:nrow(df_after)) {
    predicate_sum <- sum(df_before[i, predicate_columns])
    if (predicate_sum > 0) {
      expect_equal(
        df_after[i, weight_columns_after],
        adjustment_factor * df_before[i, weight_columns_before]
      )
    }
  }
})


test_that('observations that do not match the predicate have no change in weights', {
  adjustment_factor <- 2.5
  test_data <- get_simple_survey_test_data()
  df_before <- as_data_frame_with_weights(
    test_data$rep_svy,
    full_wgt_name = 'FULL_SAMPLE_WGT',
    rep_wgt_prefix = 'REP_WGT_'
  )
  adjusted_svy <- adjust_weights_by_factor(
    test_data$rep_svy,
    ~count1 + count2 + count3 + count4 + count5 + count6,
    adjustment_factor
  )
  df_after <- as_data_frame_with_weights(
    adjusted_svy,
    full_wgt_name = 'FULL_SAMPLE_WGT',
    rep_wgt_prefix = 'REP_WGT_'
  )

  weight_columns_before <- grep('_WGT', colnames(df_before))
  weight_columns_after <- grep('_WGT', colnames(df_after))
  predicate_columns <- grep('count', colnames(df_before))

  for (i in 1:nrow(df_after)) {
    predicate_sum <- sum(df_before[i, predicate_columns])
    if (predicate_sum == 0) {
      expect_equal(
        df_after[i, weight_columns_after],
        df_before[i, weight_columns_before]
      )
    }
  }
})

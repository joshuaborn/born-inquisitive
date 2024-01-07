get_simple_test_data <- function() {
  simple_df <- data.frame(
    weight = c(1500, 750, 1250, 1100, 900, 1000, 1050, 950),
    stratum = c(1, 1, 2, 2, 3, 3, 4, 4),
    psu = c(1, 2, 1, 2, 1, 2, 1, 2),
    count1 = c(0, 0, 0, 1, 0, 0, 0, 1),
    count2 = c(0, 0, 0, 0, 0, 2, 0, 0),
    count3 = c(0, 0, 0, 0, 0, 0, 0, 0),
    count4 = c(0, 0, 0, 1, 0, 0, 0, 0),
    count5 = c(0, 1, 0, 0, 0, 0, 0, 0),
    count6 = c(0, 0, 0, 0, 0, 0, 0, 0)
  )

  simple_svy <- svydesign(
    data = simple_df,
    ids = ~psu,
    strata = ~stratum,
    weights = ~weight,
    nest = TRUE
  )

  simple_rep_svy <- as.svrepdesign(
    design = simple_svy,
    type = 'BRR'
  )

  list(
    df = simple_df,
    svy = simple_svy,
    rep_svy = simple_rep_svy
  )
}


get_parameters <- function(adjustment_factor) {
  test_data <- get_simple_test_data()

  counts_to_adjust <- test_data$df$count1 + test_data$df$count2

  unadjusted_estimate <- sum(test_data$df$weight * counts_to_adjust)

  target_total <- unadjusted_estimate * adjustment_factor

  weights_before <- as_data_frame_with_weights(
    test_data$rep_svy,
    full_wgt_name = 'FULL_SAMPLE_WGT',
    rep_wgt_prefix = 'REP_WGT_'
  )

  list(
    test_data = test_data,
    counts_to_adjust = counts_to_adjust,
    unadjusted_estimate = unadjusted_estimate,
    target_total = target_total,
    weights_before = weights_before
  )
}


get_results <- function(adjustment_factor) {
  parameters <- get_parameters(adjustment_factor)

  adjusted_svy <- adjust_weights_by_ratio(
    parameters$test_data$rep_svy,
    ~count1 + count2,
    parameters$target_total,
    ~count1 + count2 + count3 + count4 + count5 + count6
  )

  weights_after <- as_data_frame_with_weights(
    adjusted_svy,
    full_wgt_name = 'FULL_SAMPLE_WGT',
    rep_wgt_prefix = 'REP_WGT_'
  )

  list(
    parameters = parameters,
    adjusted_svy = adjusted_svy,
    weights_after = weights_after
  )
}


test_that('only svyrep.design objects are accepted', {
  test_data <- get_simple_test_data()
  expect_error(adjust_weights_by_ratio(test_data$df), 'svyrep.design')
  expect_error(adjust_weights_by_ratio(test_data$svy), 'svyrep.design')
  expect_no_error(
    adjust_weights_by_ratio(
      test_data$rep_svy,
      ~count1 + count2,
      10000,
      ~count1 + count2 + count3 + count4 + count5 + count6
    )
  )
})


test_that('a svyrep.design object is returned', {
  parameters <- get_parameters(2.5)

  adjusted_svy <- adjust_weights_by_ratio(
    parameters$test_data$rep_svy,
    ~count1 + count2,
    parameters$target_total,
    ~count1 + count2 + count3 + count4 + count5 + count6
  )

  expect_contains(class(adjusted_svy), 'svyrep.design')
})


test_that('output data have same number of rows as input', {
  results <- get_results(2.5)
  expect_equal(
    nrow(results$weights_after),
    nrow(results$parameters$weights_before)
  )
})


test_that('adjusted estimate equals target total', {
  results <- get_results(2.5)
  expect_equal(
    sum(
      svytotal(
        ~count1+count2,
        results$adjusted_svy
      )[1:2]
    ),
    results$parameters$target_total
  )
})


test_that('observations that match the predicate have their weights adjusted', {
  adjustment_factor <- 2.5
  results <- get_results(adjustment_factor)

  weight_columns_before <- grep('_WGT', colnames(results$parameters$weights_before))
  weight_columns_after <- grep('_WGT', colnames(results$weights_after))
  predicate_columns <- grep('count', colnames(results$parameters$weights_before))

  for (i in 1:nrow(results$weights_after)) {
    predicate_sum <- sum(results$parameters$weights_before[i, predicate_columns])
    if (predicate_sum > 0) {
      expect_equal(
        results$weights_after[i, weight_columns_after],
        adjustment_factor *
          results$parameters$weights_before[i, weight_columns_before]
      )
    }
  }
})


test_that('observations that do not match the predicate have no change in weights', {
  results <- get_results(2.5)

  weight_columns_before <- grep('_WGT', colnames(results$parameters$weights_before))
  weight_columns_after <- grep('_WGT', colnames(results$weights_after))
  predicate_columns <- grep('count', colnames(results$parameters$weights_before))

  for (i in 1:nrow(results$weights_after)) {
    predicate_sum <- sum(results$parameters$weights_before[i, predicate_columns])
    if (predicate_sum == 0) {
      expect_equal(
        results$weights_after[i, weight_columns_after],
        results$parameters$weights_before[i, weight_columns_before]
      )
    }
  }
})

get_simple_test_data <- function() {
  simple_df <- data.frame(
    weight = c(1500, 750, 1250, 1100, 900, 1000, 1050, 950),
    stratum = c(1, 1, 2, 2, 3, 3, 4, 4),
    psu = c(1, 2, 1, 2, 1, 2, 1, 2)
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


test_that('only svyrep.design objects are accepted', {
  test_data <- get_simple_test_data()
  expect_error(adjust_weights_by_ratio(test_data$df), 'svyrep.design')
  expect_error(adjust_weights_by_ratio(test_data$svy), 'svyrep.design')
  expect_no_error(adjust_weights_by_ratio(test_data$rep_svy))
})

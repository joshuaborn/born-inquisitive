get_simple_survey_test_data <- function() {
  simple_df <- data.frame(
    weight = c(1500, 750, 1250, 1100, 900, 1000, 1050, 950),
    stratum = c(1, 1, 2, 2, 3, 3, 4, 4),
    psu = c(1, 2, 1, 2, 1, 2, 1, 2),
    count1 = c(0, 0, 0, 1, 0, 0, 0, 1),
    count2 = c(0, 0, 0, 0, 0, 2, 0, 0),
    count3 = c(0, 0, 0, 0, 0, 0, 0, 0),
    count4 = c(0, 0, 0, 1, 0, 0, 0, 0),
    count5 = c(0, 1, 0, 0, 0, 0, 0, 0),
    count6 = c(0, 0, 0, 0, 0, 0, 0, 0),
    cat1 = factor(paste0('level', c(1, 1, 2, 2, 3, 3, 4, 4)))
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

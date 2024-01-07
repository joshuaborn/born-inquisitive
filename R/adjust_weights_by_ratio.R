library(survey)
library(svrep)


#' Adjust weights in survey design object with replicate weights
#'
#' `adjust_weights_by_ratio()` increases the weights of all observations that
#' match a specified predicate in a design object created by `survey` or `srvyr`
#' in order for an estimate to match a target total. The function only works
#' with survey design objects specified with replicate weights.
#'
#' @param svy A svyrep.design object as created by `survey` or `srvyr` packages
#' @param estimate A formula denoting the variables to be used to calculate the
#'  adjustment ratio
#' @param target The target total to be used to calculate the adjustment ratio
#' @param predicate A formula denoting which observations are going to have the
#'  weight adjustment applied
#'
#' @value A svyrep.design object with weights adjusted
#'

#test_data <- get_simple_test_data()
#
#svy <- test_data$rep_svy
#
#estimate <- formula(~count1+count2)
#
#predicate <- formula(~count1+count2+count3+count4+count5+count6)
#
#target <- 10000
#
#svy_adj <- adjust_weights_by_ratio(svy, estimate, target, predicate)
#
#
#temp <- svytotal(~count1+count2, svy_adj)
#
#svy_adj |>
#  as_data_frame_with_weights()
#
#sum(temp[1:2])

adjust_weights_by_ratio <- function(svy, estimate, target, predicate) {
  if (!any(class(svy) == 'svyrep.design')) {
    stop('Parameter svy must be of class svyrep.design')
  }

  df_before <- as_data_frame_with_weights(
    svy,
    full_wgt_name = 'wgt_full_sample',
    rep_wgt_prefix = 'wgt_rep_'
  )

  estimate_row_sum <- rowSums(
    df_before[, attr(terms(estimate), which = 'term.labels')]
  )

  estimate_sum <- sum(df_before$wgt_full_sample* estimate_row_sum)

  adjustment_factor <- target / estimate_sum

  weight_cols <- grep('wgt_rep_|wgt_full_sample', colnames(df_before))

  weights <- df_before[weight_cols]
  predicates <- df_before[, attr(terms(predicate), which = 'term.labels')]

  adjusted_weights <- cbind(weights)

  for (i in seq(1, nrow(weights))) {
    if (sum(predicates[i, ]) > 0) {
      adjusted_weights[i, ] <- adjusted_weights[i, ] * adjustment_factor
    }
  }

  svrepdesign(
    data = cbind(
      df_before[-weight_cols],
      adjusted_weights
    ),
    type = svy$type,
    weights = ~wgt_full_sample,
    repweights = 'wgt_rep_'
  )
}

library(survey)
library(svrep)


#' Adjust weights in a replicate weight survey design object
#'
#' `adjust_weights_by_factor()` adjusts the weights of all observations that
#' match a selection predicate in a design object created by `survey` or `srvyr`
#' by multiplying the weights by a given factor. Observations that do not match
#' the selection predicate are adjusted by multiplying their weights by a
#' complementary factor in order to keep the sum of all weights equal before
#' and after adjustment. This function only works with survey design objects
#' specified with replicate weights.
#'
#' @param svy A svyrep.design object as created by `survey` or `srvyr` packages
#' @param selection A formula denoting which observations to select for weight
#'  adjustment; any observation with a sum of the columns specified here greater
#'  than 0 is selected for weight adjustment
#' @param factor The factor to multiply to the weight of each selected
#'  observation
#'
#' @value A svyrep.design object with weights adjusted
#'

adjust_weights_by_factor <- function(svy, selection, factor) {
  if (!inherits(svy, 'svyrep.design')) {
    stop('Parameter svy must be of class svyrep.design')
  }

  df_before <- as_data_frame_with_weights(
    svy,
    full_wgt_name = 'wgt_full_sample',
    rep_wgt_prefix = 'wgt_rep_'
  )

  weight_cols <- grep('wgt_rep_|wgt_full_sample', colnames(df_before))
  weights <- df_before[weight_cols]

  selected_or_not <- (
    rowSums(df_before[attr(terms(selection), which = 'term.labels')]) > 0
  )

  sum_selected_weights <- sum(df_before[selected_or_not, 'wgt_full_sample'])
  sum_complementary_weights <- sum(df_before[!selected_or_not, 'wgt_full_sample'])

  complementary_factor <- 1 + (
    ((1 - factor) * sum_selected_weights) / sum_complementary_weights
  )

  adjusted_weights <- cbind(weights)

  for (i in seq(1, nrow(adjusted_weights))) {
    if (selected_or_not[i]) {
      adjusted_weights[i, ] <- adjusted_weights[i, ] * factor
    } else {
      adjusted_weights[i, ] <- adjusted_weights[i, ] * complementary_factor
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

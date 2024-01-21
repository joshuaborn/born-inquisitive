library(survey)
library(svrep)


#' Adjust weights in a replicate weight survey design object
#'
#' `adjust_weights_by_factor()` increases the weights of all observations that
#' match a specified predicate in a design object created by `survey` or `srvyr`
#' in order for an estimate to match a target total. The function only works
#' with survey design objects specified with replicate weights.
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
  if (!any(class(svy) == 'svyrep.design')) {
    stop('Parameter svy must be of class svyrep.design')
  }

  df_before <- as_data_frame_with_weights(
    svy,
    full_wgt_name = 'wgt_full_sample',
    rep_wgt_prefix = 'wgt_rep_'
  )

  weight_cols <- grep('wgt_rep_|wgt_full_sample', colnames(df_before))

  weights <- df_before[weight_cols]
  predicates <- df_before[, attr(terms(selection), which = 'term.labels')]

  adjusted_weights <- cbind(weights)

  for (i in seq(1, nrow(weights))) {
    if (sum(predicates[i, ]) > 0) {
      adjusted_weights[i, ] <- adjusted_weights[i, ] * factor
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

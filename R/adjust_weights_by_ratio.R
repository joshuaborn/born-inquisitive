library(survey)

#' Adjust weights in survey design object with replicate weights
#'
#' `adjust_weights_by_ratio()` increases the weights of all observations that
#' match a specified predicate in a design object created by `survey` or `srvyr`
#' in order for an estimate to match a target total. The function only works
#' with survey design objects specified with replicate weights.
#'
#' @param svy A svyrep.design object as created by `survey` or `srvyr` packages

adjust_weights_by_ratio <- function(svy) {
  if (!any(class(svy) == 'svyrep.design')) {
    stop('Parameter svy must be of class svyrep.design')
  }
}

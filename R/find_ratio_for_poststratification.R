library(survey)

#' Find a ratio for weight adjustment that makes estimate match target value
#'
#' @param svy A `survey.design` or `svyrep.design` object as created by `survey`
#'  or `srvyr` packages
#' @param estimators A formula denoting the variables to be used to calculate
#'  the estimate
#' @param target The target total to be used to calculate the ratio
#'
#' @value A numeric value of the calculated ratio
#'

find_ratio_for_poststratification <- function(svy, estimators, target) {
  if (!any(class(svy) %in% c('survey.design', 'svyrep.design'))) {
    stop('Parameter svy must be of class survey.design or svyrep.design')
  }

  target / sum(svytotal(estimators, svy))
}

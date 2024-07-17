library(survey)


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
#' @param equalize Whether or not to do a complementary adjustment to weights
#'  not selected in order that the sum of weights before and after adjustment
#'  are equal
#'
#' @value A svyrep.design object with weights adjusted
#'

adjust_weights_by_factor <- function(svy, selection, factor, equalize = TRUE) {
  if (!inherits(svy, 'svyrep.design')) {
    stop('Parameter svy must be of class svyrep.design')
  }

  selected_or_not <- (
    rowSums(svy$variables[attr(terms(selection), which = 'term.labels')]) > 0
  )

  sampling_weights <- weights(svy, type = 'sampling')

  sampling_weights[selected_or_not] <- factor *
    sampling_weights[selected_or_not]

  if (equalize) {
    sum_selected_weights <- sum(
      weights(svy, type = 'sampling')[selected_or_not]
    )
    sum_complementary_weights <- sum(
      weights(svy, type = 'sampling')[!selected_or_not]
    )

    complementary_factor <- 1 + (
      ((1 - factor) * sum_selected_weights) / sum_complementary_weights
    )

    sampling_weights[!selected_or_not] <- complementary_factor *
      sampling_weights[!selected_or_not]
  }

  svy$pweights <- sampling_weights

  svy$call <- sys.call()
  svy$adj_factors <- c(svy$adj_factors, factor)

  return(svy)

}

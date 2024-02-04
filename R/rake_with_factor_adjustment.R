library(here)
library(survey)

source(here('R/adjust_weights_by_factor.R'))
source(here('R/find_ratio_for_poststratification.R'))


#' Perform raking poststratification adjustment with special step of factor adjustment
#'
#' This function is based on the `rake` function in the `survey` package. In
#' addition to the regular `postStratify` calls, it also calls the
#' `adjust_weights_by_factor` with each iteration and includes the results of
#' the factor adjustment in the checks for convergence.
#'
#' @param design A svyrep.design object as created by `survey` or `srvyr` packages
#' @param sample.margins list of formulas or data frames describing sample margins, which must not contain missing values
#' @param population.margins list of tables or data frames describing corresponding population margins
#' @param control	`maxit` controls the number of iterations. Convergence is declared if the maximum change in a table entry is less than `epsilon`. If `epsilon`<1 it is taken to be a fraction of the total sampling weight.
#' @param compress If `design` has replicate weights, attempt to compress the new replicate weight matrix? When `NULL`, will attempt to compress if the original weight matrix was compressed
#' @param factor.estimation.vars A formula denoting the variables whose estimated total will be used to calculate the factor for weight adjustment
#' @param factor.estimation.target A numeric value for the estimated total of variables in `factor.estimated.vars` will equal after factor adjustment
#' @param factor.adjusted.vars A formula denoting the variables that denote what observations to select for weight factor adjustment. If the sum of these variables is greater than 0 for any observation, then that observation has its weight adjusted by the adjustment factor.
#'
#' @value A svyrep.design object with weights adjusted
#'

rake_with_factor_adjustment <- function(design,
  sample.margins, population.margins,
  factor.estimation.vars, factor.estimation.target, factor.adjusted.vars,
  control=list(maxit=10, epsilon=1, verbose=FALSE),
  compress=NULL) {

    if (!any(class(design) == 'svyrep.design')) {
      stop('Parameter design must be of class svyrep.design')
    }

    if (is.null(compress))
      compress <- inherits(design$repweights, 'repweights_compressed')

    design$degf <- NULL

    if (length(sample.margins) != length(population.margins))
      stop('sample.margins and population.margins do not match.')

    number_of_margins <- length(sample.margins)

    if (control$epsilon < 1)
        control$epsilon <- control$epsilon * sum(weights(design, 'sampling'))

    strata <- lapply(
      sample.margins,
      function(margin) {
        if (inherits(margin, 'formula')) {
          mf <- model.frame(margin, data=design$variables, na.action=na.fail)
        }
      }
    )

    allterms <- unlist(lapply(sample.margins, all.vars))
    ff <- formula(paste("~", paste(allterms, collapse="+"), sep=""))
    oldtable <- svytable(ff, design)

    old_estimate <- sum(svytotal(factor.estimation.vars, design))

    iter <- 0
    converged <- FALSE

    while(iter < control$maxit) {

      design <- adjust_weights_by_factor(
        design,
        factor.adjusted.vars,
        find_ratio_for_poststratification(
          design,
          factor.estimation.vars,
          factor.estimation.target
        )
      )

      new_estimate <- sum(svytotal(factor.estimation.vars, design))

      estimate_diff <- abs(old_estimate - new_estimate)

      for(i in 1:number_of_margins) {
        design <- postStratify(
          design,
          strata[[i]],
          population.margins[[i]],
          compress=FALSE
        )
      }

      newtable <- svytable(ff, design)

      delta <- max(abs(oldtable - newtable), estimate_diff)

      if (delta < control$epsilon){
        converged <- TRUE
        break
      }

      oldtable <- newtable

      old_estimate <- new_estimate

      iter <- iter + 1
    }

    design$call <- sys.call()

    if (compress)
      design$repweights <- compressWeights(design$repweights)

    design$degf <- degf(design)

    if(!converged)
      warning("Raking did not converge after ", iter, " iterations.\n")

    return(design)

}

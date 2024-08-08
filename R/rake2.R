library(here)
library(survey)

source(here('R/postStratify2.R'))


#' Raking post-stratification adjustment in which any of the post-statification
#' steps can have post-strata assignments used to calculate the adjustment
#' factors that differ from the post-strata assignments used for weight
#' adjustment
#'
#' This function is based on the `rake` function in the `survey` package. It
#' calls either the regular `postStratify` function or `postStatify2` with
#' each iteration.
#'
#' @param design A svyrep.design object as created by `survey` or `srvyr` packages
#' @param sample.margins list of formulas or data frames describing sample margins, which must not contain missing values
#' @param adjustment.strata list of formulas of post-strata to be used for the actual weight adjustment, with `NULL` elements in list where post-strata are identical to those used in `sample.margins`
#' @param population.margins list of tables or data frames describing corresponding population margins
#' @param control	`maxit` controls the number of iterations. Convergence is declared if the maximum change in a table entry is less than `epsilon`. If `epsilon`<1 it is taken to be a fraction of the total sampling weight.
#' @param compress If `design` has replicate weights, attempt to compress the new replicate weight matrix? When `NULL`, will attempt to compress if the original weight matrix was compressed
#'
#' @value A svyrep.design object with weights adjusted
#'

rake2 <- function(design,
  sample.margins, population.margins, weight_adj_strata=NULL,
  control=list(maxit=10, epsilon=1, verbose=FALSE),
  compress=NULL) {

    if (!inherits(design, 'svyrep.design')) {
      stop('Parameter design must be of class svyrep.design')
    }

    if (is.null(compress))
      compress <- inherits(design$repweights, 'repweights_compressed')

    original_weights <- weights(design, 'sampling')
    original_repweights <- weights(design, 'analysis')
    original_repfactors <- weights(design, 'replication')

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
    oldtable <- svytotal(ff, design)

    #if (control$verbose)
    #    print(oldtable)

    oldpoststrata <- design$postStrata
    iter <- 0
    converged <- FALSE

    while(iter < control$maxit) {

      if (control$verbose) {
        print(paste('rake2 iteration', iter))
      }

      design$postStrata <- NULL

      for(i in 1:number_of_margins) {
        if (!is.null(weight_adj_strata) & !is.null(weight_adj_strata[[i]])) {
          design <- postStratify2(
            design,
            strata[[i]],
            population.margins[[i]],
            weight_adj_strata[[i]],
            compress=FALSE,
            verbose=control$verbose
          )
        } else {
          design <- postStratify(
            design,
            strata[[i]],
            population.margins[[i]],
            compress=FALSE
          )
        }
      }

      newtable <- svytotal(ff, design)

      #if (control$verbose)
      #  print(newtable)

      delta <- max(abs(oldtable - newtable))

      if (delta < control$epsilon){
        converged <- TRUE
        if (control$verbose) {
          print('Converged!')
        }
        break
      }

      oldtable <- newtable

      iter <- iter + 1
    }

    ## changed in 3.6-3 to allow the projections to be iterated
    ## in svyrecvar
    rakestrata <- design$postStrata
    if(!is.null(rakestrata)) {
      class(rakestrata) <- "raking"
      design$postStrata <- c(oldpoststrata, list(rakestrata))
    }

    design$call <- sys.call()

    if (compress)
      design$repweights <- compressWeights(design$repweights)

    design$degf <- degf(design)

    if (!converged)
      warning("Raking did not converge after ", iter, " iterations.\n")

    return(design)

}

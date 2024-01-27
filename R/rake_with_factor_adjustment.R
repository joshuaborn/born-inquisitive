library(survey)


#' Perform raking poststratification adjustment with special step of factor adjustment
#'
#' @param design A svyrep.design object as created by `survey` or `srvyr` packages
#'
#' @value A svyrep.design object with weights adjusted
#'

rake_with_factor_adjustment <- function(design, sample.margins, population.margins,
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

    iter <- 0
    converged <- FALSE

    while(iter < control$maxit) {

      for(i in 1:number_of_margins) {
        design <- postStratify(
          design,
          strata[[i]],
          population.margins[[i]],
          compress=FALSE
        )
      }

      newtable <- svytable(ff, design)

      delta <- max(abs(oldtable - newtable))
      if (delta < control$epsilon){
        converged <- TRUE
        break
      }

      oldtable <- newtable

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

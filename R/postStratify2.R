library(survey)


#' Post-stratification in which the post-strata assignments used to calculate
#' the adjustment factors and the post-strata assignments used for weight
#' adjustment differ
#'
#' @param svy A svyrep.design object as created by `survey` or `srvyr` packages
#' @param adj_factor_strata Assignment of observations to strata that are used
#'  to calculate adjustment factors that would make estimates match `targets`
#' @param targets Target totals for each level of `adj_factor_strata`
#' @param weight_adj_strata Assignment of observations to strata that will be
#'  used in the actual adjustment of weights; levels must match those in
#'  `adj_factor_strata`
#'
#' @value A svyrep.design object with weights adjusted
#'

postStratify2 <- function(svy, adj_factor_strata, targets, weight_adj_strata, compress=NULL, verbose=FALSE) {

  if (!inherits(svy, 'svyrep.design')) {
    stop('Parameter svy must be of class svyrep.design')
  }

  if (is.null(compress)) {
    compress <- inherits(svy$repweights, 'repweights_compressed')
  }

  adj_factor_col <- attr(terms(adj_factor_strata), which = 'term.labels')
  weight_adj_col <- attr(terms(weight_adj_strata), which = 'term.labels')

  target_levels <- levels(targets[[adj_factor_col]])
  adj_factor_levels <- levels(svy$variables[[adj_factor_col]])

  if (!isTRUE(all.equal(target_levels, adj_factor_levels))) {
    stop('The levels of the variable denoted by adj_factor_strata must be identical to the levels in the targets data frame')
  }

  weight_adj_levels <- levels(svy$variables[[weight_adj_col]])

  if (!isTRUE(all.equal(adj_factor_levels, weight_adj_levels))) {
    stop('The levels of the variable denoted by adj_factor_strata must be identical to the levels of the variable denoted by weight_adj_strata')
  }

  estimates <- svytotal(adj_factor_strata, svy, return.replicates = TRUE)
  estimates_row_labels <- names(estimates$mean)
  targets_row_labels <- paste0(adj_factor_col, as.character(targets[[adj_factor_col]]))

  if (!isTRUE(all.equal(estimates_row_labels, targets_row_labels))) {
    stop('The levels specified in the data set and those specified target totals must be identical')
  }

  original_repfactors <- weights(svy, 'replication')

  adj_factors_main <- targets$Freq / as.vector(estimates$mean)
  adj_factors_replicates <- targets$Freq / t(as.matrix(estimates$replicates))
  n_replicates <- ncol(adj_factors_replicates)
  attributes(adj_factors_replicates) <- NULL
  adj_factors_replicates <- matrix(adj_factors_replicates, ncol = n_replicates)

  if (is.null(svy$adj_factors)) {
    svy$adj_factors <- list()
  }
  svy$adj_factors <- c(
    svy$adj_factors,
    list(
      main = adj_factors_main,
      replicates = adj_factors_replicates,
      poststrata = targets[[adj_factor_col]]
    )
  )

  strata_key <- as.integer(svy$variables[[weight_adj_col]])

  svy$pweights <- svy$pweights * adj_factors_main[strata_key]

  new_repweights <- (weights(svy, 'analysis') * adj_factors_replicates[strata_key,])

  if (svy$combined.weights) {
    svy$repweights <- new_repweights
  } else {
    svy$repweights <- (new_repweights / svy$pweights)
  }

  # When an estimate for one or more strata totals in a replicate subset is 0,
  # the adjusted factors for any observations in those strata will be Inf, and
  # the resulting weights will be NaN.
  #
  # In these cases, fall back on the less precise method of using the main
  # adjusted weight multiplied by the original replicate factors to regenerate
  # the replicate weight set.
  if (any(colSums(is.nan(svy$repweights)) > 0)) {
    which_replicates <- which(colSums(is.nan(svy$repweights)) > 0)
    if (verbose)
        print(paste(
          'Replicates',
          paste(which_replicates, collapse= ', '),
          'have NaN values after postStratification and will be recalculated based on the main adjusted weight and the original replicate weight factors.'
        ))
    svy$repweights[, which_replicates] <- svy$pweights * original_repfactors[, which_replicates]
  }

  if (compress) {
    svy$repweights <- compressWeights(svy$repweights)
  }

  svy$call <- sys.call()

  if (!is.null(svy$degf)) {
    svy$degf <- NULL
    svy$degf <- degf(svy)
  }

  return(svy)
}

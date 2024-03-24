library(srvyr)


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
#svy <- original_svy
#targets <- target_totals
#targets <- get_target_totals()[-2,]
#adj_factor_strata <- ~abortion_2013_2014_age

postStratify2 <- function(svy, adj_factor_strata, targets, weight_adj_strata) {
  if (!inherits(svy, 'svyrep.design')) {
    stop('Parameter svy must be of class svyrep.design')
  }

  adj_factor_col <- attr(terms(adj_factor_strata), which = 'term.labels')
  weight_adj_col <- attr(terms(weight_adj_strata), which = 'term.labels')

  if (!isTRUE(all.equal(
        as.character(pull(targets, adj_factor_col)),
        as.character(levels(pull(svy$variables, adj_factor_col)))
       ))
     ) {
    stop('The levels of the variable denoted by adj_factor_strata must be identical to the levels in the targets data frame')
  }

  if (!isTRUE(all.equal(
        levels(pull(svy$variables, weight_adj_col)),
        levels(pull(svy$variables, adj_factor_col))
       ))
     ) {
    stop('The levels of the variable denoted by adj_factor_strata must be identical to the levels of the variable denoted by weight_adj_strata')
  }

  orig_ests <- svy |>
    as_survey_rep() |>
    group_by(get(adj_factor_col)) |>
    summarize(Est = survey_total(vartype = NULL))

  adj_factors <- orig_ests |>
    inner_join(
      targets,
      by = c(`get(adj_factor_col)` = adj_factor_col)
    ) |>
    mutate(adj_factor = Freq / Est) |>
    select(`get(adj_factor_col)`, adj_factor)

  svy$pweights <- cbind(
      svy$variables[weight_adj_col],
      orig_sampling_weight = weights(svy, type = 'sampling')
    ) |>
    as_tibble() |>
    rename(join_col = all_of(weight_adj_col)) |>
    left_join(
      adj_factors |>
        rename(join_col = "get(adj_factor_col)"),
      by = "join_col"
    ) |>
    mutate(
      adj_sampling_weight = orig_sampling_weight * adj_factor
    ) |>
    pull(adj_sampling_weight)

  names(adj_factors) <- c(weight_adj_col, 'adj_factor')
  if (!is.null(svy$adj_factors)) {
    svy$adj_factors <- list(svy$adj_factors, adj_factors)
  } else {
    svy$adj_factors <- adj_factors
  }

  svy$call <- sys.call()

  return(svy)
}

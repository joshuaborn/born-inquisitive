library(survey)

svytotalci <- function(x, design) {
  this_stat <- svytotal(x, design)
  this_ci <- confint(this_stat, df = degf(design))

  cbind(
    data.frame(
      total = as.vector(this_stat),
      se = as.vector(SE(this_stat)),
      row.names = names(this_stat)
    ),
    this_ci
  )
}

svybyci <- function(formula, by, design, FUN) {
  this_stat <- svyby(formula, by, design, FUN, keep.names = FALSE)
  this_ci <- confint(this_stat, df = degf(design))

  cbind(
    this_stat,
    this_ci
  )
}

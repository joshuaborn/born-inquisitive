library(data.table)
library(knitr)
library(survey)

rename_CI_columns <- function(names) {
  these_names <- names
  these_names <- sub("2.5%", "2.5 %", these_names)
  these_names <- sub("97.5%", "97.5 %", these_names)
  these_names
}

svyci <- function(x, design, FUN, ordered = TRUE) {
  this_stat <- FUN(x, design)
  this_ci <- confint(this_stat, df = degf(design))
  prefix <- as.character(x)
  prefix <- prefix[seq(2, length(prefix))]

  dt <- cbind(
    data.table(
      level = sub(prefix, '', names(this_stat), fixed = TRUE),
      estimate = as.vector(this_stat),
      se = as.vector(SE(this_stat))
    ),
    this_ci
  )

  setnames(dt, rename_CI_columns)

  if (ordered) {
    dt[order(-estimate)]
  } else {
    dt
  }
}

svybyci <- function(formula, by, design, FUN, ordered = TRUE) {
  this_stat <- svyby(formula, by, design, FUN, keep.names = FALSE)
  this_ci <- confint(this_stat, df = degf(design))
  colnames(this_stat) <- c('level', 'estimate', 'se')

  dt <- as.data.table(cbind(
    this_stat,
    this_ci
  ))

  setnames(dt, rename_CI_columns)

  if (ordered) {
    dt[order(-estimate)]
  } else {
    dt
  }
}

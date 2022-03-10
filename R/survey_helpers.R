library(data.table)
library(knitr)
library(survey)

svyci <- function(x, design, FUN) {
  this_stat <- FUN(x, design)
  this_ci <- confint(this_stat, df = degf(design))
  prefix <- as.character(x)
  prefix <- prefix[seq(2, length(prefix))]

  cbind(
    data.table(
      level = sub(prefix, '', names(this_stat), fixed = TRUE),
      estimate = as.vector(this_stat),
      se = as.vector(SE(this_stat))
    ),
    this_ci
  )
}

ksvyci <- function(x, design, FUN) {
  kable(svyci(x, design, FUN))
}

svytotalci <- function(x, design) {
  svyci(x, design, svytotal)
}

ksvytotalci <- function(x, design) {
  kable(svytotalci(x, design))
}

svybyci <- function(formula, by, design, FUN) {
  this_stat <- svyby(formula, by, design, FUN, keep.names = FALSE)
  this_ci <- confint(this_stat, df = degf(design))
  colnames(this_stat) <- c('level', 'estimate', 'se')

  cbind(
    this_stat,
    this_ci
  )
}

ksvybyci <- function(formula, by, design, FUN) {
  kable(svybyci(formula, by, design, FUN))
}

tablena <- function(x) {
  table(x, useNA = 'always')
}

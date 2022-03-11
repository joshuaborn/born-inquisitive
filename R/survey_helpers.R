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

svybyci <- function(formula, by, design, FUN) {
  this_stat <- svyby(formula, by, design, FUN, keep.names = FALSE)
  this_ci <- confint(this_stat, df = degf(design))
  colnames(this_stat) <- c('level', 'estimate', 'se')

  cbind(
    this_stat,
    this_ci
  )
}

tablena <- function(x) {
  table(x, useNA = 'always')
}

in_millions <- function(dt, round_to = 3, do_round = TRUE) {
  dt <- as.data.table(dt)
  cols <- c('estimate', 'se', '2.5 %', '97.5 %')
  sub_dt <- dt[, cols, with = FALSE] / 10^6
  if (do_round) {
    sub_dt <- round(sub_dt, round_to)
  }
  dt[, (cols) := sub_dt]
  dt
}

rounded <- function(dt, round_to = 3) {
  cols <- c('estimate', 'se', '2.5 %', '97.5 %')
  sub_dt <- round(dt[, cols, with = FALSE], round_to)
  dt[, (cols) := sub_dt]
  dt
}

label_estimate <- function(est, title, domain = '', type = '', round_to = 3) {
  if (type == 'total') {
    dt <- in_millions(est, round_to)
  } else if (type == 'proportion') {
    dt <- rounded(est, round_to)
  } else {
    dt <- as.data.table(est)
  }
  dt[, .(
    domain = domain,
    title = title,
    type = type,
    level,
    estimate,
    se,
    `2.5 %`,
    `97.5 %`
  )]
}

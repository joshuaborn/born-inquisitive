library(data.table)
library(survey)

env <- new.env()

env$estimates <- data.table()

collect_estimate <- function(est, title, domain = '', type = '', round_to = 3, plot = FALSE) {
  this_title <- title
  this_domain <- domain
  this_type <- type

  if (type == 'total') {
    dt <- in_millions(est, round_to)
  } else if (type == 'proportion') {
    dt <- rounded(est, round_to)
  } else {
    dt <- as.data.table(est)
  }
  env$estimates <- rbindlist(list(
    env$estimates[
      !(
        title == this_title &
          domain == this_domain &
          type == this_type
      )
    ],
    dt[, .(
      domain = this_domain,
      title = this_title,
      type = this_type,
      level,
      estimate,
      se,
      `2.5 %`,
      `97.5 %`
    )]
  ))

  env$estimates[
    title == this_title &
      domain == this_domain &
      type == this_type
  ]
}

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

factorize <- function(x, name, formats_table, fill_na = TRUE) {
  if (fill_na) {
    nafill_value <- max(formats_table[format_name == name, factor_value]) + 1
    factor(
      nafill(x, fill = nafill_value),
      levels = c(
        formats_table[format_name == name, factor_value],
        nafill_value
      ),
      labels = c(
        formats_table[format_name == name, factor_label],
        'No answer provided'
      )
    )
  } else {
    factor(
      x,
      levels = formats_table[format_name == name, factor_value],
      labels = formats_table[format_name == name, factor_label]
    )
  }
}

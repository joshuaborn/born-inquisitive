library(data.table)
library(here)
library(survey)

i_am('R/general_helpers.R')


#
# GENERAL PURPOSE
# 

tablena <- function(x) {
  table(x, useNA = 'ifany')
}

has_level <- function(x, ids) {
  is.element(as.integer(x), ids)
}

na_to_false <- function(x) {
  fifelse(is.na(x), FALSE, x)
}


#
# DATA MANIPULATION
#
# These are helper functions that that perform more complicated manipulations
# on data tables.
#

set_mentions_to_booleans <- function(
  in_dt,
  out_dt,
  out_prefix,
  in_prefix,
  in_series,
  indices
) {
  in_vars <- paste0(in_prefix, in_series)

  for (x in indices) {
    set(
      out_dt,
      j = paste0(out_prefix, x),
      value = in_dt[
        ,
        lapply(.SD, \(col) has_level(col, x)),
        .SDcols = in_vars
      ] |> apply(1, any)
    )
  }

  set_count_variable_for_mentions(out_dt, out_prefix)
}

set_count_variable_for_mentions <- function(dt, prefix) {
  if (paste(prefix, 'count', sep = '_') %in% colnames(dt)) {
    dt[
      ,
      (paste(prefix, 'count', sep = '_')) := NULL
    ]
  }
  dt[
    ,
    (paste(prefix, 'count', sep = '_')) := rowSums(.SD),
    .SDcols = patterns(paste('^', prefix, sep = ''))
  ]
}

create_pivot_table <- function(
  prefix,
  series,
  id_vars,
  source_dt
) {
  dt <- copy(
    source_dt[
      ,
      c(id_vars, paste0(prefix, series)),
      with = FALSE
    ]
  )

  dt <- melt(
    dt,
    id.vars = id_vars,
    variable.factor = FALSE,
    value.name = prefix
  )

  dt[
    ,
    (paste0(prefix, 'ID')) :=
      as.integer(sub(prefix, '', variable))
  ]

  dt[, variable := NULL]

  dt
}



#
# SURVEY 
#
# These are helper functions that take output from survey library and organize
# the output into usable data tables.
#

estimate_totals_and_percentages <- function(f, svy) {
  totals <- svytotal(f, svy)
  totals_ci <- as.data.frame(confint(totals, df = degf(svy)))
  names(totals_ci) <- c('total_CI_low', 'total_CI_high')
  percentages <- svymean(f, svy)
  percentages_ci <- as.data.frame(confint(percentages, df = degf(svy)))
  names(percentages_ci) <- c('percentage_CI_low', 'percentage_CI_high')
  level_prefix <- as.character(f)
  level_prefix <- level_prefix[seq(2, length(level_prefix))]

  cbind(
    data.table(
      description = sub(level_prefix, '', names(totals), fixed = TRUE),
      total = as.vector(totals)
    ),
    totals_ci,
    data.table(
      percentage = as.vector(percentages)
    ),
    percentages_ci
  )
}

estimate_row_total_and_percentage <- function(f, svy) {
  totals <- svytotal(f, svy)
  totals_ci <- as.data.frame(confint(totals, df = degf(svy)))
  names(totals_ci) <- c('total_CI_low', 'total_CI_high')
  totals_df <- cbind(totals, totals_ci)
  level_prefix <- as.character(f)
  level_prefix <- level_prefix[seq(2, length(level_prefix))]
  rownames(totals_df) <- sub(level_prefix, '', rownames(totals_df), fixed = TRUE)
  totals_df <- totals_df[rownames(totals_df) == 'TRUE', -2]
  if (nrow(svy) > 0) {
    percentage <- svyciprop(f, svy)
    percentage_ci <- as.data.frame(confint(percentage, df = degf(svy)))
    names(percentage_ci) <- c('percentage_CI_low', 'percentage_CI_high')
    as.data.table(cbind(
      totals_df,
      data.table(
        percentage = as.vector(percentage)
      ),
      percentage_ci
    ))
  } else {
    as.data.table(cbind(
      totals_df,
      data.table(
        percentage = 0,
        percentage_CI_low = NaN,
        percentage_CI_high = NaN
      )
    ))
  }
}

round_estimates_of_totals_and_percentages <- function(dt) {
  cbind(
    dt[
      ,
      .SD,
      .SDcols = patterns('^[^(total)|(percentage)]')
    ],
    dt[
      ,
      lapply(.SD, \(col) round(col / 10^6, 3)),
      .SDcols = patterns('^total')
    ],
    dt[
      ,
      lapply(.SD, \(col) round(col, 3) * 100),
      .SDcols = patterns('^percentage')
    ]
  )
}

stringify_estimates_of_totals_and_percentages <- function(dt) {
  dt[,
    .(
      description,
      total = sprintf("%.3fM", total),
      total_CI = sprintf(
        "(%.3fM, %.3fM)",
        total_CI_low,
        total_CI_high
      ),
      percentage = sprintf("%0.1f%%", percentage),
      percentage_CI = sprintf(
        "(%0.1f%%, %0.1f%%)",
        percentage_CI_low,
        percentage_CI_high
      )
    )
  ]
}

prettify_estimates_of_totals_and_percentages <- function(dt) {
  dt |>
    round_estimates_of_totals_and_percentages() |>
    stringify_estimates_of_totals_and_percentages()
}

estimate_mentions <- function(
  survey_design,
  prefix,
  indices
) {
  estimates <- rbindlist(lapply(
    indices,
    function(x) {
      tryCatch(
        expr = {
          row <- estimate_row_total_and_percentage(
            as.formula(paste0('~', prefix, x)),
            survey_design
          )
          row[, description := x]
          row
        },
        error = function(err) {
          message(paste('Error with row', x, ':', err))
        }
      )
    }
  ))
}

estimate_mentions_in_subdomains <- function(row_estimation_function, indices) {
  estimates <- rbindlist(lapply(
    indices,
    function(x) {
      row <- row_estimation_function(x)
      row[, description := x]
      row
    }
  ))
}

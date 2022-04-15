library(data.table)
library(here)
library(huxtable)
library(survey)
library(tidyfst)

i_am('R/general_helpers.R')

options(huxtable.knit_print_df = FALSE)


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

equals <- function(x, y) {
  !is.na(x) & !is.na(y) & x == y
}

pretty_millions <- function(x, digits = 2) {
  formatC(
    round(x / 10e5, digits = digits),
    format = 'f',
    digits = digits
  )
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

theme_article_extra <- function(ht) {
  header_rows <- which(header_rows(ht))
  header_cols <- which(header_cols(ht))
  body_rows <- which(!header_rows(ht))
  body_cols <- which(!header_cols(ht))
  ht |>
    theme_article() |>
    set_wrap(everywhere, body_cols, FALSE) |>
    set_align(everywhere, body_cols, 'center') |>
    set_tb_padding(header_rows, everywhere, 0) |>
    set_all_padding(body_rows, everywhere, 3) |>
    set_bold(everywhere, header_cols, FALSE) |>
    set_left_padding(
      everywhere,
      seq(min(body_cols), ncol(ht), 2),
      16
    )
}

conditionally_stripe_rows <- function(ht) {
  body_rows <- which(!header_rows(ht))
  if (length(body_rows) > 4) {
    ht |>
      set_background_color(
        stripe(2, min(body_rows) + 1),
        everywhere,
        'grey93'
      )
  } else {
    ht
  }
}

style_totals_and_percentages <- function(dt, digits = 2) {
  dt |>
  mutate_vars('total', \(x) pretty_millions(x, digits)) |>
  mutate_dt(
    total = paste0(total, 'M'),
    total_CI = sprintf(
      "(%sM, %sM)",
      total_CI_low,
      total_CI_high
    ),
    percentage_CI = sprintf(
      "(%0.1f%%, %0.1f%%)",
      round(100 * percentage_CI_low, digits = 1),
      round(100 * percentage_CI_high, digits = 1)
    )
  ) |>
  select_dt(description, total, total_CI, percentage, percentage_CI) |>
  as_huxtable() |>
  set_number_format(everywhere, 'percentage', fmt_percent(1)) |>
  set_contents(1, everywhere, c('', rep(c('Est.', '95% C.I.'), 2))) |>
  insert_row('', 'Total', '', 'Percentage', '', after = 0) |>
  merge_cells(1, 2:3) |>
  merge_cells(1, 4:5) |>
  set_header_rows(1:2, TRUE) |>
  set_header_cols(1, TRUE) |>
  theme_article_extra() |>
  conditionally_stripe_rows()
}

combine_huxtables_vertically <- function(label1, ht1, label2, ht2) {
  if (!all.equal(ht1[header_rows(ht1),], ht2[header_rows(ht2),])) {
    stop('huxtables do not have identical headers and cannot be combined vertically')
  }
  ht <- rbind(
    ht1,
    ht2[!header_rows(ht2), ]
  )
  body_rows1 <- which(!header_rows(ht1))
  body_rows2 <- which(!header_rows(ht2))
  body_start_index1 <- min(body_rows1)
  body_start_index2 <- max(body_rows1) + 1
  label_column <- rep('', nrow(ht))
  label_column[body_start_index1] <- label1
  label_column[body_start_index2] <- label2
  header_column_index <- 1
  old_header_columns <- names(header_cols(ht))[header_cols(ht)]
  insert_column(ht, label_column) |>
    set_header_cols(header_column_index, TRUE) |>
    style_header_cols(valign = 'middle') |>
    set_rowspan(
      body_start_index1,
      header_column_index,
      length(body_rows1)
    ) |>
    set_rowspan(
      body_start_index2,
      header_column_index,
      length(body_rows2)
    ) |>
    theme_article_extra() |>
    set_bold(everywhere, header_column_index, TRUE) |>
    set_right_padding(everywhere, header_column_index, 12) |>
    set_top_border(body_start_index2, everywhere)
}

style_and_combine_totals_and_percentages_vertically <- function(
  label1, estimate1, label2, estimate2, digits = 2
) {
  combine_huxtables_vertically(
    label1,
    style_totals_and_percentages(estimate1, digits = digits),
    label2,
    style_totals_and_percentages(estimate2, digits = digits)
  )
}

combine_huxtables_horizontally <- function(label1, ht1, label2, ht2) {
  if (!all.equal(ht2[, header_cols(ht2)], ht1[, header_cols(ht1)])) {
    stop('huxtables do not have identical header columns and cannot be combined horizontally')
  }
  body_cols1 <- which(!header_cols(ht1))
  body_cols2 <- which(!header_cols(ht2))
  body_start_index1 <- min(body_cols1)
  body_start_index2 <- max(body_cols1) + 1
  superheader_row <- rep('', ncol(ht1) + sum(!header_cols(ht2)))
  superheader_row[body_start_index1] <- label1
  superheader_row[body_start_index2] <- label2
  cbind(
    ht1,
    ht2[, which(!header_cols(ht2))]
  ) |>
    insert_row(superheader_row, after = 0) |>
    set_header_rows(1, TRUE) |>
    set_colspan(
      1,
      body_start_index1,
      length(body_cols1)
    ) |>
    set_colspan(
      1,
      body_start_index2,
      length(body_cols2)
    ) |>
    theme_article_extra() |>
    set_left_border(everywhere, body_start_index1) |>
    set_left_padding(everywhere, body_start_index1, 4) |>
    set_left_border(everywhere, body_start_index2) |>
    set_left_padding(everywhere, body_start_index2, 4)
}

style_and_combine_totals_and_percentages_horizontally <- function(
  label1, estimate1, label2, estimate2, digits = 2
) {
  combine_huxtables_horizontally(
    label1,
    style_totals_and_percentages(estimate1, digits = digits),
    label2,
    style_totals_and_percentages(estimate2, digits = digits)
  )
}

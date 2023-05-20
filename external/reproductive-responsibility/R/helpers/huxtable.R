library(huxtable)
library(tidyfst)

options(huxtable.knit_print_df = FALSE)

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

pretty_millions <- function(x, digits = 2) {
  formatC(
    round(x / 10e5, digits = digits),
    format = 'f',
    digits = digits
  )
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

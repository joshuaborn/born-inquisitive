library(ggplot2)

plot_with_CIs <- function(
  data,
  val_col,
  reordering = TRUE,
  scale_factor = 1,
  label_col = 'description'
) {
  if (reordering) {
    y_expr <- expr(reorder(.data[[label_col]], .data[[val_col]]))
    y_end_expr <- expr(reorder(.data[[label_col]], .data[[val_col]]))
  } else {
    y_expr <- expr(.data[[label_col]])
    y_end_expr <- expr(.data[[label_col]])
  }
  ggplot(
    data = data,
    mapping = aes(
      y = !!y_expr
    )
  ) +
    geom_segment(
      aes(
        x = .data[[paste0(val_col, '_CI_low')]] * scale_factor,
        xend = .data[[paste0(val_col, '_CI_high')]] * scale_factor,
        yend = !!y_end_expr
      ),
      arrow = arrow(angle = 90, ends = 'both', length = unit(0.075, 'inches')),
      size = 0.5
    ) +
    geom_point(
      aes(
        x = .data[[val_col]] * scale_factor
      ),
      size = 2
    )
}

plot_totals_with_CIs <- function(data, reordering = TRUE) {
  plot_with_CIs(data, 'total', reordering, scale_factor = 10e-7) +
    scale_x_continuous(
      labels = scales::label_number(suffix = 'M', accuracy = 0.1)
    ) +
    scale_y_discrete(
      labels = scales::wrap_format(50)
    ) +
    theme_bw() +
    theme(
      axis.title.y = element_blank()
    ) +
    xlab('Total Persons')
}

plot_percentages_with_CIs <- function(data, reordering = TRUE) {
  plot_with_CIs(data, 'percentage', reordering) +
    scale_x_continuous(
      labels = scales::label_percent(1)
    ) +
    scale_y_discrete(
      labels = scales::wrap_format(50)
    ) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      legend.position = 'none'
    ) +
    xlab('Percentage of Persons')
}

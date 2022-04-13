library(ggplot2)

plot_totals_with_CIs <- function(data, reorder_factor = TRUE) {
  scale_factor <- 10e-7

  if (reorder_factor) {
    this_plot <- ggplot(
      data = data,
      mapping = aes(
        y = reorder(description, total)
      )
    ) +
    geom_segment(
      arrow = arrow(angle = 90, ends = 'both', length = unit(0.075, 'inches')),
      mapping = aes(
        x = total_CI_low * scale_factor,
        xend = total_CI_high * scale_factor,
        yend = reorder(description, total)
      ),
      size = 0.5
    )
  } else {
    this_plot <- ggplot(
      data = data,
      mapping = aes(
        y = description
      )
    ) +
    geom_segment(
      arrow = arrow(angle = 90, ends = 'both', length = unit(0.075, 'inches')),
      mapping = aes(
        x = total_CI_low * scale_factor,
        xend = total_CI_high * scale_factor,
        yend = description
      ),
      size = 0.5
    )
  }

  this_plot +
    geom_point(
      aes(
        x = total * scale_factor
      ),
      size = 2
    ) +
    scale_x_continuous(
      labels = scales::label_number(suffix = 'M', accuracy = 0.1),
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

plot_percentages_with_CIs <- function(data, reorder_factor = TRUE) {
  if (reorder_factor) {
    this_plot <- ggplot(
      data = data,
      mapping = aes(
        y = reorder(description, percentage)
      )
    ) +
    geom_segment(
      aes(
        yend = reorder(description, percentage),
        x = percentage_CI_low,
        xend = percentage_CI_high
      ),
      arrow = arrow(angle = 90, ends = 'both', length = unit(0.075, 'inches')),
      size = 0.5
    )
  } else {
    this_plot <- ggplot(
      data = data,
      mapping = aes(
        y = description
      )
    ) +
    geom_segment(
      aes(
        x = percentage_CI_low,
        xend = percentage_CI_high,
        yend = description
      ),
      arrow = arrow(angle = 90, ends = 'both', length = unit(0.075, 'inches')),
      size = 0.5
    )
  }

  this_plot +
    geom_point(
      aes(
        x = percentage
      ),
      size = 2
    ) +
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

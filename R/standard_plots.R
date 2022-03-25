library(ggplot2)

standard_millions_plot <- function(data) {
  ggplot(
    data = data,
    mapping = aes(
      y = reorder(level, estimate)
    )
  ) +
  geom_point(
    aes(
      x = estimate
    ),
    size = 2
  ) +
  geom_segment(
    aes(
      yend = reorder(level, estimate),
      x = `2.5 %`,
      xend = `97.5 %`
    ),
    arrow = arrow(angle = 90, ends = 'both', length = unit(0.075, 'inches')),
    size = 0.5
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
  )
}

standard_proportions_plot <- function(data) {
  ggplot(
  data = data,
  mapping = aes(
    y = reorder(level, estimate)
  )
) +
  geom_point(
    aes(
      x = estimate
    ),
    size = 2
  ) +
  geom_segment(
    aes(
      yend = reorder(level, estimate),
      x = `2.5 %`,
      xend = `97.5 %`
    ),
    arrow = arrow(angle = 90, ends = 'both', length = unit(0.075, 'inches')),
    size = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.1),
    expand = c(0, 0),
    labels = scales::label_percent(1),
    limits = c(0, 1)
  ) +
  scale_y_discrete(
    labels = scales::wrap_format(50)
  ) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    legend.position = 'none'
  )
}

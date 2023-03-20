library(dplyr)
library(ggplot2)
library(here)
library(readr)
library(showtext)


guttmacher <- read_csv(
    here('external/reproductive-responsibility/data/guttmacher/NationalAndStatePregnancy_PublicUse.csv')
  ) |>
  mutate(abortions_interpolated = year %in%
      c(1983, 1986, 1989, 1990, 1993, 1994, 1997, 1998, 2001, 2002, 2003, 2006,
        2009, 2012, 2015)
  ) |>
  filter(state == 'US' & !abortions_interpolated) |>
  select(year, abortionstotal)

guttmacher <- bind_rows(
  guttmacher,
  data.frame(
    year = 2020,
    abortionstotal = 930160
  )
)

years <- guttmacher$year


font_add_google('Open Sans', 'open')
showtext_auto()
showtext_opts(dpi = 200)
theme_update(
  text = element_text(family = 'open')
)


ggplot(
  data = guttmacher,
  mapping = aes(
    x = year,
    y = abortionstotal
  )
) +
  geom_line(
    alpha = 0.4,
    color = '#0375A7',
    size = 1
  ) +
  geom_point(
    fill = 'white',
    color = '#0375A7',
    shape = 21,
    size = 1.5
  ) +
  scale_y_continuous(
    breaks = \(x) seq(x[1], x[2], 20e4),
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::unit_format(unit = "M", scale = 1e-6),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    breaks = years[c(seq(1, 7, 2), seq(10, length(years), 2), length(years))],
    expand = expansion(add = 1),
    minor_breaks = years
  ) +
  labs(
    x = 'Year',
    y = 'Millions of Induced Abortions in United States'
  ) +
  theme_classic() +
  theme(
    axis.line.x.bottom = element_line(
      color = 'black',
      size = 0.2
    ),
    axis.line.y.left = element_line(
      color = 'black',
      size = 0.2
    ),
    axis.text.x = element_text(
      angle = 45,
      color = 'black',
      hjust = 0.5,
      vjust = 0.65
    ),
    axis.text.y = element_text(
      color = 'black',
      margin = margin(r = 0)
    ),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(
      color = 'black',
      size = 0.2
    ),
    axis.title.x = element_text(
      margin = margin(t = 2)
    ),
    axis.title.y = element_text(
      margin = margin(r = 10)
    ),
    legend.position = 'bottom',
    legend.title = element_blank(),
    panel.grid.major = element_line(
      color = 'gray',
      linetype = 3,
      linewidth = 0.25
    ),
    panel.grid.minor.y = element_line(
      color = 'gray',
      linetype = 3,
      linewidth = 0.25
    )
  )

ggsave(
  here('static/images/Guttmacher_APC_abortion_totals.png'),
  width = 1500,
  height = 1125,
  units = 'px'
)

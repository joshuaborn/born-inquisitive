library(dplyr)
library(ggplot2)
library(here)
library(readr)
library(showtext)
library(tidyr)



font_add_google('Open Sans', 'open')
showtext_auto()
showtext_opts(dpi = 200)
theme_update(
  text = element_text(family = 'open')
)



gallup_morality_data <- bind_rows(
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-abortion.csv')
  ) |> mutate(Topic = 'Abortion'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-animal-medical-testing.csv')
  ) |> mutate(Topic = 'Animal Medical Testing'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-baby-out-of-wedlock.csv')
  ) |> mutate(Topic = 'Baby Out of Wedlock'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-changing-gender.csv')
  ) |> mutate(Topic = 'Changing Gender'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-cloning-animals.csv')
  ) |> mutate(Topic = 'Cloning Animals'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-cloning-humans.csv')
  ) |> mutate(Topic = 'Cloning Humans'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-death-penalty.csv')
  ) |> mutate(Topic = 'Death Penalty'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-extramarital-affair.csv')
  ) |> mutate(Topic = 'Extramarital Affair'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-fur.csv')
  ) |> mutate(Topic = 'Clothing Made of Animal Fur'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-gambling.csv')
  ) |> mutate(Topic = 'Gambling'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-marijuana.csv')
  ) |> mutate(Topic = 'Marijuana'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-polygamy.csv')
  ) |> mutate(Topic = 'Polygamy'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-pornography.csv')
  ) |> mutate(Topic = 'Pornography'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-same-sex-relations.csv')
  ) |> mutate(Topic = 'Same-sex Relations'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-stem-cell-research.csv')
  ) |> mutate(Topic = 'Stem Cell Research'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-suicide.csv')
  ) |> mutate(Topic = 'Suicide'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-teenage-sex.csv')
  ) |> mutate(Topic = 'Sex between Teenagers'),
  read_csv(
    here('external/reproductive-responsibility/data/gallup/morality-unmarried-sex.csv')
  ) |> mutate(Topic = 'Sex between an Unmarried Man and Woman')
)

gallup_morality_data |>
  filter(Demographic == 'Aggregate') |>
  select(-Geography, -Demographic, -`Demographic Value`) |>
  mutate(
    acceptable = parse_number(`Morally acceptable`),
    wrong = parse_number(`Morally wrong`),
    depends = parse_number(`Depends on the situation (vol.)`)
  ) |>
  filter(Topic == 'Abortion') |>
  select(Time, acceptable, wrong, depends) |>
  arrange(Time) |>
  pivot_longer(cols = 2:4) |>
  ggplot(
    mapping = aes(
      x = Time,
      y = value,
      color = name,
      shape = name
    )
  ) +
    geom_line(
      alpha = 0.4,
      size = 1
    ) +
    geom_point(
      fill = 'white',
      size = 1.5
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      limits = c(0, NA)
    ) +
    scale_x_continuous(
      breaks = \(x) seq(x[1]+1, x[2]-1, 2),
      expand = expansion(add = 1),
      minor_breaks = \(x) seq(x[1]+1, x[2]-1, 1)
    ) +
    scale_color_hue(
      label = c(
        acceptable = 'Morally acceptable',
        wrong = 'Morally wrong',
        depends = 'Depends on the situation'
      )
    ) +
    scale_shape_manual(
      label = c(
        acceptable = 'Morally acceptable',
        wrong = 'Morally wrong',
        depends = 'Depends on the situation'
      ),
      values = c(21, 22, 23)
    ) +
    labs(
      x = 'Year',
      y = 'Estimated Percent of Population with Opinion'
    ) +
    theme_classic() +
    theme(
      axis.line.x.bottom = element_line(
        color = 'black',
        linewidth = 0.2
      ),
      axis.line.y.left = element_line(
        color = 'black',
        linewidth = 0.2
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
        linewidth = 0.2
      ),
      axis.title.x = element_text(
        margin = margin(t = 2)
      ),
      axis.title.y = element_text(
        margin = margin(r = 10)
      ),
      legend.margin = margin(t = 0, b = 0),
      legend.position = 'top',
      legend.title = element_blank(),
      panel.grid.major = element_line(
        color = 'gray',
        linetype = 3,
        linewidth = 0.25
      ),
      panel.grid.minor = element_line(
        color = 'gray',
        linetype = 3,
        linewidth = 0.25
      ),
      plot.margin = margin(t = 0)
  )

ggsave(
  here('static/images/Gallup_opinions_about_abortion.png'),
  width = 1500,
  height = 1125,
  units = 'px'
)

gallup_morality_data |>
  filter(Demographic == 'Aggregate') |>
  select(-Geography, -Demographic, -`Demographic Value`) |>
  mutate(
    acceptable = parse_number(`Morally acceptable`),
    wrong = parse_number(`Morally wrong`),
    difference = abs(acceptable - wrong)
  ) |>
  select(Topic, Time, difference) |>
  arrange(Time, difference) |>
  group_by(Time) |>
  mutate(rank = rank(difference, ties.method = 'first')) |>
  ungroup() |>
  filter(rank <= 4) |>
  select(Time, rank, Topic, difference) |>
  group_by(Topic) |>
  summarize(count = n()) |>
  arrange(count)

gallup_morality_data |>
  filter(Demographic == 'Aggregate') |>
  select(-Geography, -Demographic, -`Demographic Value`) |>
  mutate(
    acceptable = parse_number(`Morally acceptable`),
    wrong = parse_number(`Morally wrong`),
    difference = abs(acceptable - wrong),
    metric = 100 - difference
  ) |>
  select(Time, Topic, metric) |>
  arrange(Time, metric) |>
  filter(
    Topic %in%  c(
      "Abortion",
      "Baby Out of Wedlock",
      "Animal Medical Testing",
      "Extramarital Affair"
    )
  ) |>
  ggplot(
    mapping = aes(
      x = Time,
      y = metric,
      color = Topic,
      shape = Topic
    )
  ) +
    geom_line(
      alpha = 0.4,
      size = 1
    ) +
    geom_point(
      fill = 'white',
      size = 1.5
    ) +
    scale_y_continuous(
      limits = c(NA, 100)
    ) +
    scale_x_continuous(
      breaks = \(x) seq(x[1]+1, x[2]-1, 2),
      expand = expansion(add = 1),
      minor_breaks = \(x) seq(x[1]+1, x[2]-1, 1)
    ) +
    scale_shape_manual(
      values = c(21, 22, 23, 24)
    ) +
    labs(
      x = 'Year',
      y = 'Moral Controversy Metric'
    ) +
    theme_classic() +
    theme(
      axis.line.x.bottom = element_line(
        color = 'black',
        linewidth = 0.2
      ),
      axis.line.y.left = element_line(
        color = 'black',
        linewidth = 0.2
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
        linewidth = 0.2
      ),
      axis.title.x = element_text(
        margin = margin(t = 2)
      ),
      axis.title.y = element_text(
        margin = margin(r = 10)
      ),
      legend.margin = margin(t = 0, b = 0),
      legend.position = 'top',
      legend.title = element_blank(),
      panel.grid.major = element_line(
        color = 'gray',
        linetype = 3,
        linewidth = 0.25
      ),
      panel.grid.minor = element_line(
        color = 'gray',
        linetype = 3,
        linewidth = 0.25
      ),
      plot.margin = margin(t = 0)
  )

ggsave(
  here('static/images/Gallup_controversial_moral_opinions.png'),
  width = 1500,
  height = 1125,
  units = 'px'
)

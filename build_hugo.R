library(here)

i_am('build_hugo.R')

blogdown::build_site(
  build_rmd = FALSE,
  run_hugo = TRUE
)

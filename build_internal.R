library(here)

i_am('build_internal.R')


# Build internal Rmd files
blogdown::build_site(
  build_rmd = TRUE,
  run_hugo = FALSE
)

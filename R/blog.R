library(here)
library(blogdown)

i_am('R/blog.R')

site_dir <- here('blog')

build_hugo <- function() {
  build_site(
    build_rmd = FALSE,
    run_hugo = TRUE,
    args = c(paste('-s', site_dir))
  )
}

build_blog <- function() {
  setwd(site_dir)
  build_site(
    build_rmd = TRUE,
    run_hugo = TRUE,
    args = c(paste('-s', site_dir))
  )
  setwd(here())
}

# Preview in web browser
serve_blog <- function() {
  serve_site(.site_dir = site_dir)
}

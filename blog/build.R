library(here)

i_am('blog/build.R')


build_hugo <- function() {
  setwd(here('blog/'))
  blogdown::build_site(
    build_rmd = FALSE,
    run_hugo = TRUE,
    args = c(paste('-s', here('blog/')))
  )
  setwd(here())
}


build <- function() {
  setwd(here('blog/'))
  blogdown::build_site(
    build_rmd = TRUE,
    run_hugo = TRUE,
    args = c(paste('-s', here('blog/')))
  )
  setwd(here())
}


# Preview in web browser
preview <- function() {
  blogdown::serve_site()
}


# Shut down
shutdown <- function() {
  blogdown::stop_server()
}

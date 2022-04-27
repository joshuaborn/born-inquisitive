blogdown::build_site(
  build_rmd = function(x) {
    not_NSFG <- x[grep('content/NSFG/', x, invert = TRUE)]
    NSFG_reports <- x[grep('content/NSFG/reports', x)]
    c(not_NSFG, NSFG_reports)
  }
)

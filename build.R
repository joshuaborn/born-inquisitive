submodule_path <- file.path('content', 'NSFG')
submodule_inclusion_path <- file.path(submodule_path, 'reports')

blogdown::build_site(
  run_hugo = FALSE,
  build_rmd = function(x) {
    not_NSFG <- x[grep(submodule_path, x, invert = TRUE)]
    NSFG_reports <- x[grep(submodule_inclusion_path, x)]
    c(not_NSFG, NSFG_reports)
  }
)

filepaths <- list.files(submodule_inclusion_path, "\\.html$", full.names = TRUE)

blogdown::build_dir(submodule_inclusion_path, force = TRUE)

file.rename(
  filepaths,
  gsub(submodule_inclusion_path, 'content', filepaths)
)

blogdown::build_site(
  run_hugo = TRUE,
  build_rmd = FALSE
)

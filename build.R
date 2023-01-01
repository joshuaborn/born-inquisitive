library(here)

i_am('build.R')


build_external <- function() {

  external_path <- here('external', 'reproductive-responsibility', 'reports')

  # Build external articles
  blogdown::build_dir(external_path, force = TRUE)


  # Copy YAML front matter to HTML files
  external_html_files <- list.files(external_path, "\\.html$", full.names = TRUE)
  for (html_filename in external_html_files) {
    Rmd_filename <- gsub('\\.html$', '.Rmd', html_filename)
    html_contents <- readLines(html_filename)
    Rmd_contents <- readLines(Rmd_filename)
    yaml_header <- Rmd_contents[
      seq(
        grep('---', Rmd_contents)[1],
        grep('---', Rmd_contents)[2]
      )
    ]
    writeLines(c(yaml_header, "\n", html_contents), html_filename)
  }


  # Copy HTML from external articles to content/
  file.copy(
    external_html_files,
    'content/',
    overwrite = TRUE
  )


  # Copy supporting files from external articles to static/
  file.copy(
    list.files(external_path, "_files$", full.names = TRUE),
    'static/',
    recursive = TRUE
  )
}


build_internal <- function() {
  blogdown::build_site(
    build_rmd = TRUE,
    run_hugo = FALSE
  )
}


build_hugo <- function() {
  blogdown::build_site(
    build_rmd = FALSE,
    run_hugo = TRUE
  )
}


build <- function() {
  build_external()
  blogdown::build_site(
    build_rmd = TRUE,
    run_hugo = TRUE
  )
}


# Preview in web browser
preview <- function() {
  blogdown::serve_site()
}


# Shut down
shutdown <- function() {
  blogdown::stop_server()
}

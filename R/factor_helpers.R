tablena <- function(x) {
  table(x, useNA = 'always')
}

has_level <- function(x, ids) {
  is.element(as.integer(x), ids)
}

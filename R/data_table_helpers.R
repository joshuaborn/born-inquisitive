library(data.table)

na_to_false <- function(x) {
  fifelse(is.na(x), FALSE, x)
}

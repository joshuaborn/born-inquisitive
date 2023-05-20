tablena <- function(x) {
  table(x, useNA = 'ifany')
}

equals <- function(x, y) {
  !is.na(x) & !is.na(y) & x == y
}

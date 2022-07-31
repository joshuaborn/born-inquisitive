century_month_comparison <- function(operator, x, y) {
  !is.na(x) &
    x != 9998 &
    x != 9999 &
    !is.na(y) &
    y != 9998 &
    y != 9999 &
    operator(x, y)
}

to_century_month <- function(x) {
  x_int <- as.integer(x)
  months <- factor(
    x_int %% 12,
    levels = 0:11,
    labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov')
  )
  years <- floor((x_int - 1) / 12) + 1900
  fifelse(
    x_int > 9000,
    fcase(
      x_int == 9997, 'Not ascertained',
      x_int == 9998, 'Refused',
      x_int == 9999, "Don't know"
    ),
    paste(as.character(months), years)
  )
}

names_to_century_month <- function(x) {
  names(x) <- to_century_month(names(x))
  x
}

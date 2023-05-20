library(data.table)

get_NSFG_format_indices <- function(formats_table, format, skip = NULL) {
  idxs <- formats_table[format_name == format, factor_value]
  if (!is.null(skip)) {
    idxs[!is.element(idxs, skip)]
  } else {
    idxs
  }
}

factorize <- function(x, name, formats_table, fill_na = TRUE) {
  if (fill_na) {
    nafill_value <- max(formats_table[format_name == name, as.integer(factor_value)]) + 1
    factor(
      nafill(x, fill = nafill_value),
      levels = c(
        formats_table[format_name == name, factor_value],
        nafill_value
      ),
      labels = c(
        formats_table[format_name == name, factor_label],
        'No answer provided'
      )
    )
  } else {
    factor(
      x,
      levels = formats_table[format_name == name, factor_value],
      labels = formats_table[format_name == name, factor_label]
    )
  }
}

set_NSFG_variables_as_factors <- function(raw_data, dt, formats_table, variables) {
  for (x in seq(length(variables))) {
    variable_name <- names(variables)[x]
    format_name <- variables[x]
    values <- factorize(
      raw_data[, get(variable_name)],
      format_name,
      formats_table
    )

    dt[, (variable_name) := ..values]
  }
}

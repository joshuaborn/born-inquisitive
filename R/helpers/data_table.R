library(data.table)

source(here('R/helpers/general.R'))

na_to_false <- function(x) {
  fifelse(is.na(x), FALSE, x)
}

has_level <- function(x, ids) {
  is.element(as.integer(x), ids)
}

set_mentions_to_booleans <- function(
  in_dt,
  out_dt,
  out_prefix,
  in_prefix,
  in_series,
  indices
) {
  in_vars <- paste0(in_prefix, in_series)

  for (x in indices) {
    set(
      out_dt,
      j = paste0(out_prefix, x),
      value = in_dt[
        ,
        lapply(.SD, \(col) has_level(col, x)),
        .SDcols = in_vars
      ] |> apply(1, any)
    )
  }

  set_count_variable_for_mentions(out_dt, out_prefix)
}

set_count_variable_for_mentions <- function(dt, prefix) {
  if (paste(prefix, 'count', sep = '_') %in% colnames(dt)) {
    dt[
      ,
      (paste(prefix, 'count', sep = '_')) := NULL
    ]
  }
  dt[
    ,
    (paste(prefix, 'count', sep = '_')) := rowSums(.SD),
    .SDcols = patterns(paste('^', prefix, sep = ''))
  ]
}

create_pivot_table <- function(
  source_dt,
  prefix,
  series,
  id_vars
) {
  dt <- copy(
    source_dt[
      ,
      c(id_vars, paste0(prefix, series)),
      with = FALSE
    ]
  )

  dt <- melt(
    dt,
    id.vars = id_vars,
    variable.factor = FALSE,
    value.name = prefix
  )

  dt[
    ,
    (paste0(prefix, 'ID')) :=
      as.integer(sub(prefix, '', variable))
  ]

  dt[, variable := NULL]

  if (!is.null(id_vars)) {
    setkeyv(dt, id_vars)
  }

  dt
}
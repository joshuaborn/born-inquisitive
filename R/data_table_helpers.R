library(data.table)

na_to_false <- function(x) {
  fifelse(is.na(x), FALSE, x)
}

create_pivot_table <- function(
  column_prefix,
  idxs,
  source_dt,
  id_vars
) {
  dt <- copy(
    source_dt[
      ,
      c(id_vars, paste(column_prefix, idxs, sep = '')),
      with = FALSE
    ]
  )

  dt <- melt(
    dt,
    id.vars = id_vars,
    variable.factor = FALSE,
    value.name = column_prefix
  )

  dt[
    ,
    (paste(column_prefix, 'ID', sep = '')) :=
      as.integer(sub(column_prefix, '', variable))
  ]

  dt[, variable := NULL]

  dt
}

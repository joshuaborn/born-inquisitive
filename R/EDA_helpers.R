library(data.table)

env <- new.env()

env$estimates <- data.table()

collect_estimate <- function(est, title, domain = '', type = '', round_to = 3, plot = FALSE) {
  this_title <- title
  this_domain <- domain
  this_type <- type

  if (type == 'total') {
    dt <- in_millions(est, round_to)
  } else if (type == 'proportion') {
    dt <- rounded(est, round_to)
  } else {
    dt <- as.data.table(est)
  }
  env$estimates <- rbindlist(list(
    env$estimates[
      !(
        title == this_title &
          domain == this_domain &
          type == this_type
      )
    ],
    dt[, .(
      domain = this_domain,
      title = this_title,
      type = this_type,
      level,
      estimate,
      se,
      `2.5 %`,
      `97.5 %`
    )]
  ))

  env$estimates[
    title == this_title &
      domain == this_domain &
      type == this_type
  ]
}

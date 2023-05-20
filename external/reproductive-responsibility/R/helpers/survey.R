library(survey)

estimate_totals_and_percentages <- function(f, svy) {
  totals <- svytotal(f, svy)
  totals_ci <- as.data.frame(confint(totals, df = degf(svy)))
  names(totals_ci) <- c('total_CI_low', 'total_CI_high')
  percentages <- svymean(f, svy)
  percentages_ci <- as.data.frame(confint(percentages, df = degf(svy)))
  names(percentages_ci) <- c('percentage_CI_low', 'percentage_CI_high')
  level_prefix <- as.character(f)
  level_prefix <- level_prefix[seq(2, length(level_prefix))]

  cbind(
    data.table(
      description = sub(level_prefix, '', names(totals), fixed = TRUE),
      total = as.vector(totals)
    ),
    totals_ci,
    data.table(
      percentage = as.vector(percentages)
    ),
    percentages_ci
  )
}

estimate_mean <- function(f, svy) {
  y <- svymean(f, svy)
  y_ci <- as.data.frame(confint(y, df = degf(svy)))
  names(y_ci) <- c('CI_low', 'CI_high')

  cbind(
    data.table(
      description = names(y),
      mean = as.vector(y)
    ),
    y_ci
  )
}

estimate_row_total_and_percentage <- function(f, svy) {
  totals <- svytotal(f, svy)
  totals_ci <- as.data.frame(confint(totals, df = degf(svy)))
  names(totals_ci) <- c('total_CI_low', 'total_CI_high')
  totals_df <- cbind(totals, totals_ci)
  level_prefix <- as.character(f)
  level_prefix <- level_prefix[seq(2, length(level_prefix))]
  rownames(totals_df) <- sub(level_prefix, '', rownames(totals_df), fixed = TRUE)
  totals_df <- totals_df[rownames(totals_df) == 'TRUE', -2]
  if (nrow(svy) > 0) {
    percentage <- svyciprop(f, svy)
    percentage_ci <- as.data.frame(confint(percentage, df = degf(svy)))
    names(percentage_ci) <- c('percentage_CI_low', 'percentage_CI_high')
    as.data.table(cbind(
      totals_df,
      data.table(
        percentage = as.vector(percentage)
      ),
      percentage_ci
    ))
  } else {
    as.data.table(cbind(
      totals_df,
      data.table(
        percentage = 0,
        percentage_CI_low = NaN,
        percentage_CI_high = NaN
      )
    ))
  }
}

estimate_mentions <- function(
  survey_design,
  prefix,
  indices
) {
  estimates <- rbindlist(lapply(
    indices,
    function(x) {
      tryCatch(
        expr = {
          row <- estimate_row_total_and_percentage(
            as.formula(paste0('~', prefix, x)),
            survey_design
          )
          row[, description := x]
          row
        },
        error = function(err) {
          message(paste('Error with row', x, ':', err))
        }
      )
    }
  ))
}

estimate_mentions_in_subdomains <- function(row_estimation_function, indices) {
  estimates <- rbindlist(lapply(
    indices,
    function(x) {
      row <- row_estimation_function(x)
      row[, description := x]
      row
    }
  ))
}

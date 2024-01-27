library(dplyr)
library(haven)
library(here)
library(survey)


here('tests/data/create_NSFG_data_for_unit_tests.R')


NSFG_2017_2019_preg_raw <- read_sas(
  data_file = here('data/NSFG/d2017_2019fempreg.sas7bdat'),
  catalog_file = here('data/NSFG/d2017_2019fempreg.sas7bcat')
)
NSFG_2015_2017_preg_raw <- read_sas(
  data_file = here('data/NSFG/d2015_2017fempreg.sas7bdat'),
  catalog_file = here('data/NSFG/d2015_2017fempreg.sas7bcat')
)
NSFG_2011_2019_wgts <- read_sas(
  data_file = here('data/NSFG/d2011_2019femwgt.sas7bdat')
)
NSFG_2017_2019_fem_raw <- read_sas(
  data_file = here('data/NSFG/d2017_2019femresp.sas7bdat'),
  catalog_file = here('data/NSFG/d2017_2019femresp.sas7bcat')
)
NSFG_2015_2017_fem_raw <- read_sas(
  data_file = here('data/NSFG/d2015_2017femresp.sas7bdat'),
  catalog_file = here('data/NSFG/d2015_2017femresp.sas7bcat')
)

NSFG_2015_2019_preg_data <- bind_rows(
  select(
    NSFG_2015_2017_preg_raw,
    SECU, SEST, CASEID,
    DATEND,
    OUTCOME,
    PREGORDR
  ),
  select(
    NSFG_2017_2019_preg_raw,
    SECU, SEST, CASEID,
    DATEND,
    OUTCOME,
    PREGORDR
  )
) |>
  left_join(
    select(NSFG_2011_2019_wgts, CASEID, WGT2015_2019),
    by = join_by(CASEID)
  ) |>
  mutate(
    one = 1,
    OUTCOME = as_factor(labelled(OUTCOME, c(
      "LIVE BIRTH" = 1,
      "INDUCED ABORTION" = 2,
      "STILLBIRTH" = 3,
      "MISCARRIAGE" = 4,
      "ECTOPIC PREGNANCY" = 5,
      "CURRENT PREGNANCY" = 6
    )))
  ) |>
  rename(
    Year = DATEND
  ) |>
  arrange(
    CASEID, PREGORDR
  )

NSFG_2015_2019_fem_data <- bind_rows(
  select(NSFG_2017_2019_fem_raw, CASEID, SEST, SECU, AGER, HISPRACE2),
  select(NSFG_2015_2017_fem_raw, CASEID, SEST, SECU, AGER, HISPRACE2)
) |>
  mutate(
    one = 1,
    age_group = cut(
      AGER,
      breaks = c(14, 19, 24, 29, 34, 39, 44, 50),
      labels = c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49')
    ),
    HISPRACE2 = as_factor(labelled(HISPRACE2, c(
      'Hispanic' = 1,
      'Non-Hispanic White' = 2,
      'Non-Hispanic Black' = 3,
      'Non-Hispanic Other' = 4
    )))
  ) |>
  left_join(
    select(NSFG_2011_2019_wgts, CASEID, WGT2015_2019),
    by = join_by(CASEID)
  )

NSFG_2015_2019_for_calibration <- NSFG_2015_2019_fem_data |>
  mutate(
    poststrat_age_race = factor(gsub(
      '[ -]',
      '_',
      paste(
        'Age',
        age_group,
        case_when(
          grepl('^Hispanic', HISPRACE2) ~ 'Hispanic',
          grepl('Black', HISPRACE2) ~ 'Non-Hispanic Black',
          .default = 'Non-Hispanic Non-Black'
        ),
        sep = '_'
      )
    ))
  ) |>
  left_join(
    NSFG_2015_2019_preg_data |>
      group_by(CASEID) |>
      summarize(
        birth_in_2014 =  if_else(
          any(OUTCOME == 'LIVE BIRTH' & Year == 2014),
            'Yes',
            'No'
          ),
        birth_in_2013 = if_else(
          any(OUTCOME == 'LIVE BIRTH' & Year == 2013),
          'Yes',
          'No'
        ),
        birth_in_2012 = if_else(
          any(OUTCOME == 'LIVE BIRTH' & Year == 2012),
          'Yes',
          'No'
        ),
        birth_in_2011 = if_else(
          any(OUTCOME == 'LIVE BIRTH' & Year == 2011),
          'Yes',
          'No'
        ),
        abortion_in_2014 = if_else(
          any(OUTCOME == 'INDUCED ABORTION' & Year == 2014),
          'Yes',
          'No'
        ),
        abortion_in_2013 = if_else(
          any(OUTCOME == 'INDUCED ABORTION' & Year == 2013),
          'Yes',
          'No'
        ),
        abortion_in_2012 = if_else(
          any(OUTCOME == 'INDUCED ABORTION' & Year == 2012),
          'Yes',
          'No'
        ),
        abortion_in_2011 = if_else(
          any(OUTCOME == 'INDUCED ABORTION' & Year == 2011),
          'Yes',
          'No'
        )
      ),
    by = 'CASEID'
  ) |> mutate(across(
    c(starts_with('birth_in_'), starts_with('abortion_in_')),
    \(x) factor(coalesce(x, 'No'), levels = c('Yes', 'No'))
  ))

NSFG_2015_2019_fem_svy <- as.svrepdesign(
  svydesign(
    data = NSFG_2015_2019_for_calibration,
    strata = ~SEST,
    id = ~SECU,
    weights = ~WGT2015_2019,
    nest = TRUE
  ),
  type = 'BRR'
)

saveRDS(NSFG_2015_2019_fem_svy, here('data/NSFG_2015_2019_fem_svy.Rds'))

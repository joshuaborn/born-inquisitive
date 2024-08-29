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
    PREGORDR,
    FMAROUT5,
    AGEPREG
  ),
  select(
    NSFG_2017_2019_preg_raw,
    SECU, SEST, CASEID,
    DATEND,
    OUTCOME,
    PREGORDR,
    FMAROUT5,
    AGEPREG
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
    ))),
    FMAROUT5 = as_factor(labelled(FMAROUT5, c(
      "MARRIED" = 1,
      "DIVORCED" = 2,
      "WINDOWED" = 3,
      "SEPARATED" = 4,
      "NEVER MARRIED" = 5
    ))),
    marital_status = as.factor(if_else(
      FMAROUT5 == 'MARRIED',
      'Married',
      'Unmarried'
    )),
  ) |>
  rename(
    Year = DATEND
  ) |>
  arrange(
    CASEID, PREGORDR
  )

NSFG_2015_2019_fem_data <- bind_rows(
  select(NSFG_2017_2019_fem_raw, CASEID, SEST, SECU, AGER, HISPRACE2, ABORTION),
  select(NSFG_2015_2017_fem_raw, CASEID, SEST, SECU, AGER, HISPRACE2, ABORTION)
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
        birth_2014_age = case_when(
          any(OUTCOME == 'LIVE BIRTH' & Year == 2014 & AGEPREG < 20) ~ 'Under 20 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2014 & AGEPREG <= 24) ~ '20-24 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2014 & AGEPREG <= 29) ~ '25-29 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2014 & AGEPREG <= 34) ~ '30-34 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2014 & AGEPREG <= 39) ~ '35-39 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2014 & AGEPREG >= 40) ~ '40 years and over',
          .default = 'Other'
        ),
        birth_2013_age =  case_when(
          any(OUTCOME == 'LIVE BIRTH' & Year == 2013 & AGEPREG < 20) ~ 'Under 20 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2013 & AGEPREG <= 24) ~ '20-24 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2013 & AGEPREG <= 29) ~ '25-29 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2013 & AGEPREG <= 34) ~ '30-34 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2013 & AGEPREG <= 39) ~ '35-39 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2013 & AGEPREG >= 40) ~ '40 years and over',
          .default = 'Other'
        ),
        birth_2012_age =  case_when(
          any(OUTCOME == 'LIVE BIRTH' & Year == 2012 & AGEPREG < 20) ~ 'Under 20 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2012 & AGEPREG <= 24) ~ '20-24 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2012 & AGEPREG <= 29) ~ '25-29 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2012 & AGEPREG <= 34) ~ '30-34 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2012 & AGEPREG <= 39) ~ '35-39 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2012 & AGEPREG >= 40) ~ '40 years and over',
          .default = 'Other'
        ),
        birth_2011_age =  case_when(
          any(OUTCOME == 'LIVE BIRTH' & Year == 2011 & AGEPREG < 20) ~ 'Under 20 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2011 & AGEPREG <= 24) ~ '20-24 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2011 & AGEPREG <= 29) ~ '25-29 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2011 & AGEPREG <= 34) ~ '30-34 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2011 & AGEPREG <= 39) ~ '35-39 years',
          any(OUTCOME == 'LIVE BIRTH' & Year == 2011 & AGEPREG >= 40) ~ '40 years and over',
          .default = 'Other'
        ),
        abortion_age =  case_when(
          any(OUTCOME == 'INDUCED ABORTION' & AGEPREG < 20)  ~ 'Under 20 years',
          any(OUTCOME == 'INDUCED ABORTION' & AGEPREG <= 24) ~ '20-24 years',
          any(OUTCOME == 'INDUCED ABORTION' & AGEPREG <= 29) ~ '25-29 years',
          any(OUTCOME == 'INDUCED ABORTION' & AGEPREG <= 34) ~ '30-34 years',
          any(OUTCOME == 'INDUCED ABORTION' & AGEPREG <= 39) ~ '35-39 years',
          any(OUTCOME == 'INDUCED ABORTION' & AGEPREG >= 40) ~ '40 years and over',
          .default = 'Other'
        ),
        abortion_2011_2014_age = factor(
          case_when(
            any(OUTCOME == 'INDUCED ABORTION' & Year %in% 2011:2014 & AGEPREG < 20) ~ 'Under 20 years',
            any(OUTCOME == 'INDUCED ABORTION' & Year %in% 2011:2014 & AGEPREG <= 24) ~ '20-24 years',
            any(OUTCOME == 'INDUCED ABORTION' & Year %in% 2011:2014 & AGEPREG <= 29) ~ '25-29 years',
            any(OUTCOME == 'INDUCED ABORTION' & Year %in% 2011:2014 & AGEPREG <= 34) ~ '30-34 years',
            any(OUTCOME == 'INDUCED ABORTION' & Year %in% 2011:2014 & AGEPREG <= 39) ~ '35-39 years',
            any(OUTCOME == 'INDUCED ABORTION' & Year %in% 2011:2014 & AGEPREG >= 40) ~ '40 years and over',
            .default = 'Other'
          ),
          ordered = TRUE,
          levels = c(
            'Under 20 years',
            '20-24 years',
            '25-29 years',
            '30-34 years',
            '35-39 years',
            '40 years and over'
          )
        ),
        abortions_2011_2014 = sum(OUTCOME == 'INDUCED ABORTION' & Year %in% 2011:2014)
      ),
    by = 'CASEID'
  ) |>
  mutate(
    across(
      ends_with('_age'),
      function(x) {
        factor(
          coalesce(x, 'Other'),
          ordered = TRUE,
          levels = c(
            'Under 20 years',
            '20-24 years',
            '25-29 years',
            '30-34 years',
            '35-39 years',
            '40 years and over',
            'Other'
          )
        )
      }
    ),
    across(
      starts_with('abortions_'),
      \(x) coalesce(x, 0)
    )
  )

NSFG_2015_2019_fem_svy <- as.svrepdesign(
  svydesign(
    data = NSFG_2015_2019_for_calibration,
    strata = ~SEST,
    id = ~SECU,
    weights = ~WGT2015_2019,
    nest = TRUE
  ),
  type = 'JKn'
)

saveRDS(NSFG_2015_2019_fem_svy, here('data/NSFG_2015_2019_fem_svy.Rds'))

library(data.table)

fempreg <- fread(
  'data/2017_2019_FemPregData.csv',
  key = c('CASEID', 'PREGORDR')
)

fempreg_labels <- fread(
  'data/2017_2019_FemPregLabels.csv',
  select = c('name', 'label'),
  key = 'name'
)
setnames(fempreg_labels, c('column_name', 'column_label'))

fempreg_formats <- fread(
  'data/2017_2019_FemPregFormats.csv',
  select = c('FMTNAME', 'START', 'LABEL'),
  key = c('FMTNAME', 'START')
)
setnames(fempreg_formats, c('column_name', 'factor_value', 'factor_label'))

femresp <- fread(
  'data/2017_2019_FemRespData.csv',
  key = 'CASEID'
)

femresp_labels <- fread(
  'data/2017_2019_FemRespLabels.csv',
  select = c('name', 'label'),
  key = 'name'
)
setnames(femresp_labels, c('column_name', 'column_label'))

femresp_formats <- fread(
  'data/2017_2019_FemRespFormats.csv',
  select = c('FMTNAME', 'START', 'LABEL'),
  key = c('FMTNAME', 'START')
)
setnames(femresp_formats, c('column_name', 'factor_value', 'factor_label'))

male <- fread(
  'data/2017_2019_MaleData.csv',
  key = 'CASEID'
)

male_labels <- fread(
  'data/2017_2019_MaleLabels.csv',
  select = c('name', 'label'),
  key = 'name'
)
setnames(male_labels, c('column_name', 'column_label'))

male_formats <- fread(
  'data/2017_2019_MaleFormats.csv',
  select = c('FMTNAME', 'START', 'LABEL'),
  key = c('FMTNAME', 'START')
)
setnames(male_formats, c('column_name', 'factor_value', 'factor_label'))

female_weights <- fread(
  'data/2011_2019_FemaleWgtData.csv',
  key = 'CASEID'
)

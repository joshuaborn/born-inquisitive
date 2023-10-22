data(api)

library(here)

load(here('R/frs.rda'))

clus2_design <- svydesign(
  id=~dnum+snum,
  fpc=~fpc1+fpc2,
  data=apiclus2
)

pop.types <- data.frame(
  stype=c("E", "H", "M"),
  Freq=c(4421, 755, 1018)
)

apiclus2$stype

pop.types

ps_design <- postStratify(
  clus2_design,
  strata=~stype,
  population=pop.types
)

svytotal(~enroll, clus2_design, na.rm=TRUE)
svytotal(~enroll, ps_design, na.rm=TRUE)
svymean(~api00, clus2_design)
svymean(~api00, ps_design)


frs.des <- svydesign(id=~PSU, weights=~GROSS2, data=frs)

pop.ctband <- data.frame(
  CTBAND=1:9,
  Freq=c(515672, 547548, 351599, 291425, 266257, 147851, 87767, 9190, 19670)
)

pop.tenure <- data.frame(
  TENURE=1:4,
  Freq=c(1459205, 493237, 128189, 156348)
)

frs.raked <- rake(
  frs.des,
  sample=list(~CTBAND, ~TENURE),
  population=list(pop.ctband, pop.tenure)
)

pop.size <- sum(pop.ctband$Freq)

pop.totals <- c(
  `(Intercept)` = pop.size,
  pop.ctband$Freq[-1],
  pop.tenure$Freq[-1]
)

frs.cal <- calibrate(
  frs.des,
  formula = ~factor(CTBAND) + factor(TENURE),
  population = pop.totals,
  calfun = "raking"
)

model.matrix(~factor(CTBAND) + factor(TENURE) - 1, frs)


test_data <- data.frame(
  abortion_respondent = c(0, 1, 1),
  abortions_critical_period = c(0, 2, 0),
  births = c(0, 0, 0)
)

model.matrix(~ abortion_respondent:abortions_critical_period - 1, test_data)

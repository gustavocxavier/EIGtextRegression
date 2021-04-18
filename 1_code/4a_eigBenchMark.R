## >> COMPUTE EIG FAMA-MECBATH APPROACH ## ####################################
library(data.table)
library(tidyverse)
source("1_code/functions.R")
## Cross-sectional forecasting ## #############################################
# We winsorize all variables at the 1st and 99th percentiles of their
# distributions each month.

# Winsorize all variables at the 1st and 99th percentiles of their distributions
# each month.
crsp4 <- readRDS("2_pipeline/1b_crsp4.rds")

DT <- crsp4 %>% select(-rdq) %>% filter(date>"1969-12-31") %>% as.data.table
DT[, d1_ia  := winsorizar(d1_ia),  by = c("date")]
DT[, cop    := winsorizar(cop),    by = c("date")]
DT[, q      := winsorizar(q),      by = c("date")]
DT[, dROE   := winsorizar(dROE),   by = c("date")]
DT <- DT[complete.cases(d1_ia, cop, q, dROE)]
crsp5 <- DT %>% as_tibble %>% select(permno:date, fiscaldate, d1_ia, cop, q, dROE, me)
rm(DT)

saveRDS(crsp5, "2_pipeline/4a_igmodel_winsorized.rds")

# crsp5 %>% select(d1_ia, q, cop, dROE) %>% drop_na
# summary(lm(d1_ia ~ q + cop + dROE, data = crsp5, weights = me))
# summary(lm(d1_ia ~ q, data = crsp5, weights = me))
# summary(lm(d1_ia ~ cop, data = crsp5, weights = me))
# summary(lm(d1_ia ~ dROE, data = crsp5, weights = me))

# x <- summary(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))
# x$coefficients["q","t value"]
# summary(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))

# Ver
# http://arelbundock.com/blog/2020-08-17-nesting-datatable.html

setDT(crsp5)

crsp5 %>% group_by(gvkey) %>%
  mutate(d1_ia   = dplyr::lag(d1_ia, 12)) %>%
  mutate(q       = dplyr::lag(q, 12)) %>%
  mutate(cop     = dplyr::lag(cop, 12)) %>%
  mutate(dROE    = dplyr::lag(dROE, 12)) %>%
  mutate(me      = dplyr::lag(me,12)) %>% na.omit -> crsp6

setDT(crsp6)

bHMXZ <- crsp5[,list(
  intercept = round(coef(lm(d1_ia ~ q + cop + dROE, weights = me))[1],3),
  q    = round(coef(lm(d1_ia ~      q + cop + dROE, weights = me))["q"],3),
  cop  = round(coef(lm(d1_ia ~      q + cop + dROE, weights = me))["cop"],3),
  dROE = round(coef(lm(d1_ia ~      q + cop + dROE, weights = me))["dROE"],3),
  R2   = round(summary(lm(d1_ia ~   q + cop + dROE, weights = me))$r.squared,3)
), by=date][order(date)]

tHMXZ <- crsp5[,.(
  intercept= round(summary(lm(d1_ia ~ q + cop + dROE, weights = me))$coefficients[1,"t value"],3),
  q        = round(summary(lm(d1_ia ~ q + cop + dROE, weights = me))$coefficients["q","t value"],3),
  cop      = round(summary(lm(d1_ia ~ q + cop + dROE, weights = me))$coefficients["cop","t value"],3),
  dROE     = round(summary(lm(d1_ia ~ q + cop + dROE, weights = me))$coefficients["dROE","t value"],3)
), by=date][order(date)]
# tHMXZ <- crsp5[,.(
#   intercept=round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["(Intercept)",   "t value"],3),
#   q    = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["q",   "t value"],3),
#   cop  = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["cop", "t value"],3),
#   dROE = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["dROE","t value"],3)
# ), by=date][order(date)]

result_monthly_cs_reg <- rbind(round(bHMXZ[, .(q, cop, dROE)][, lapply(.SD, mean, na.rm=TRUE)],3),
                               round(tHMXZ[, .(q, cop, dROE)][, lapply(.SD, mean, na.rm=TRUE)],3))
result_monthly_cs_reg <- as.data.frame(result_monthly_cs_reg)
rownames(result_monthly_cs_reg) <- c("Slopes", "t-stat")
result_monthly_cs_reg <- cbind(result_monthly_cs_reg, R2=c(round(mean(bHMXZ$R2),3),""))
result_monthly_cs_reg

bHMXZ[, b0    := computeAverageSlopes(intercept)]
bHMXZ[, bQ    := computeAverageSlopes(q)]
bHMXZ[, bCOP  := computeAverageSlopes(cop)]
bHMXZ[, bDROE := computeAverageSlopes(dROE)]
# bHMXZ
# bHMXZ %>% data.frame %>% head(30)
bHMXZ <- na.omit(bHMXZ)

## Prever EIG out-of-sample ---------------------------------------------------
# EIGt+1 = b0+ bq,t*qt + bCop,t x Copt
# + bMOM,t x MOMt
HMXZ <- crsp5 %>%
  inner_join(bHMXZ[,.(date, b0, bQ, bCOP, bDROE)], by = "date") %>%
  setDT
HMXZ[, EIG := b0 + bQ * q + bCOP * cop + bDROE * dROE]
# HMXZ[, EIG := bQ * q + bCOP * cop + bDROE * dROE]

summary(HMXZ)
# HMXZ <- HMXZ %>% filter(date>="1972-06-30")

HMXZ %>% group_by(date) %>%
  summarise(correl = cor(d1_ia, EIG)) %>%
  summarise(mean(correl))

HMXZ[,deciles := cut(EIG, quantile(EIG, probs = 0:10/10), labels = FALSE, include.lowest = TRUE), by=date]
HMXZ %>% group_by(deciles) %>% summarise(EIG  = mean(EIG,   ), d1_ia = mean(d1_ia), .groups = 'drop')

corPearson <- cor.test(HMXZ$d1_ia, HMXZ$EIG, method ="pearson")
Sys.setenv(LANG = "en")
corSpearman <- cor.test(HMXZ$d1_ia, HMXZ$EIG, method ="spearman") # Rank
result_monthly_cs_reg <- cbind(result_monthly_cs_reg,
                               Pearson=round(c(corPearson$estimate, corPearson$p.value),3),
                               Rank=round(c(corSpearman$estimate, corSpearman$p.value),3))
result_monthly_cs_reg

saveRDS(na.omit(HMXZ), file = "2_pipeline/4a_hmxz.rds")

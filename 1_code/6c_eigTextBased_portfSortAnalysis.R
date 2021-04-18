# TODO Organize the output table of portfolio analysis


library(lubridate)
library(data.table)
library(tidyverse)

filterLifeCycle = NULL
# filterLifeCycle = c(1,2)
# filterLifeCycle = 3

# VARIABLE <- "CAPEX"
VARIABLE <- "PROXY"

source('functions_eig.R')


## Portfolio usando d1_ia -----------------------------------------------------
crsp4 <- readRDS("~/Data/EIG/crsp4.rds")
lubridate::day(crsp4$date) <- lubridate::days_in_month(crsp4$date)

DT <- crsp4 %>% select(-rdq) %>% filter(date>"1969-12-31") %>% as.data.table
DT[, d1_ia  := winsorizar(d1_ia),  by = c("date")]
crsp4 <- DT %>% na.omit %>% as_tibble %>%
  select(permno:date, d1_ia, IGc1)
rm(DT)

eg_m <- crsp4 %>% select(permno, gvkey, date, eg=d1_ia) %>%
  na.omit %>% filter(is.finite(eg)) %>% as_tibble

if (!is.null(filterLifeCycle)) {
  # lc   <- readRDS("Data/WRDS/raw_comp_a_lifecycle_data.rds")
  lc   <- readRDS("~/Data/EIG/life_cycle_faff.rds")
  setDT(lc)
  lc <- lc %>% mutate(date = datadate) %>% select(gvkey, date, LC=faff) %>% as_tibble
  eg_m <- eg_m %>% left_join(lc, by = c("gvkey", "date")) %>%
    group_by(gvkey) %>% fill(LC, .direction = c("down")) %>% na.omit
}
py_ccm4 <- arrow::read_feather("~/Data/eig/ccm4.feather")
py_ccm4 <- py_ccm4 %>%
  select(permno, date=jdate, szport, retadj, wt) %>%
  mutate(date = as.Date(date))

eg_m <- eg_m %>% left_join(py_ccm4, by = c("permno", "date")) %>% na.omit

if (is.null(filterLifeCycle)) {
  DT <- as.data.table(eg_m)
} else {
  DT <- as.data.table(eg_m %>% filter(LC %in% filterLifeCycle))
  DT <- DT[year(date)>=1975][order(date)]
}
summary(DT)
DT[,portfolio := cut(eg, quantile(eg, probs = 0:10/10),
                     labels = FALSE, include.lowest = TRUE), by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt),
            eg = mean(eg)) %>%
  arrange(date) -> eg_10_ia
setDT(eg_10_ia)

## Portfolio usando capx -------------------------------------------------------

if (VARIABLE == "CAPEX") {
  crsp4 <- readRDS("~/Data/EIG/crsp4.rds")
  lubridate::day(crsp4$date) <- lubridate::days_in_month(crsp4$date)

  DT <- crsp4 %>% select(-rdq) %>% filter(date>"1969-12-31") %>% as.data.table
  DT[, IGc1  := winsorizar(IGc1),  by = c("date")]
  crsp4 <- DT %>% na.omit %>% as_tibble %>%
    select(permno:date, d1_ia, IGc1)
  rm(DT)

  eg_m <- crsp4 %>% select(permno, gvkey, date, eg=IGc1) %>%
    na.omit %>% filter(is.finite(eg)) %>% as_tibble
}
if (VARIABLE == "PROXY") {
  # eg_m <- readRDS("~/Data/eig/hmxz.rds") %>% select(permno, gvkey, date, eg=EIG) %>% na.omit %>% as_tibble
  EIG_ENet_m <- readRDS("~/Data/EIG/EIG_ENet_m.rds")
  eg_m <- EIG_ENet_m %>% select(permno, gvkey, date, eg=EIG) %>% filter(is.finite(eg)) %>% na.omit
  lubridate::day(eg_m$date) <- lubridate::days_in_month(eg_m$date)
}

if (!is.null(filterLifeCycle)) {
  # lc   <- readRDS("Data/WRDS/raw_comp_a_lifecycle_data.rds")
  lc   <- readRDS("~/Data/EIG/life_cycle_faff.rds")
  setDT(lc); lc
  lc <- lc %>% mutate(date = datadate) %>% select(gvkey, date, LC=faff) %>% as_tibble
  eg_m <- eg_m %>% left_join(lc, by = c("gvkey", "date")) %>%
    group_by(gvkey) %>% fill(LC, .direction = c("down")) %>% na.omit
}

py_ccm4 <- arrow::read_feather("~/Data/eig/ccm4.feather")
py_ccm4 <- py_ccm4 %>%
  select(permno, date=jdate, szport, retadj, wt) %>%
  mutate(date = as.Date(date))

eg_m <- eg_m %>% left_join(py_ccm4, by = c("permno", "date")) %>%  drop_na

if (is.null(filterLifeCycle)) {
  DT <- as.data.table(eg_m)
} else {
  DT <- as.data.table(eg_m %>% filter(LC %in% filterLifeCycle))
  DT <- DT[year(date)>=1975][order(date)]
}
# DT %>% group_by(date) %>% count %>% filter(year(date)>1975) %>% arrange(n)
DT[,portfolio := cut(eg, quantile(eg, probs = 0:10/10),
                      labels = FALSE, include.lowest = TRUE), by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt),
            eg = mean(eg)) %>%
  arrange(date) -> eg_10_cpx
setDT(eg_10_cpx)

## Confront Portfiolios --------------------------------------------------------
eg_10_ia  %>% group_by(portfolio) %>% summarise(mean(vwret)*100)
eg_10_cpx %>% group_by(portfolio) %>% summarise(mean(vwret)*100)
eg_10_ia  %>% group_by(portfolio) %>% summarise(mean(eg))
eg_10_cpx %>% group_by(portfolio) %>% summarise(mean(eg))

ia_HiLo <- (eg_10_ia[ portfolio==10,]$vwret - eg_10_ia[ portfolio==1,]$vwret)*100
cp_HiLo <- (eg_10_cpx[portfolio==10,]$vwret - eg_10_cpx[portfolio==1,]$vwret)*100

ia_HiLo <- eg_10_ia[portfolio==10, .(date, ia_High = round(vwret*100,2))] %>%
  left_join(eg_10_ia[portfolio==1, .(date, ia_Low = round(vwret*100,2))], by="date") %>%
  mutate(ia_HiLo = ia_High - ia_Low)

cp_HiLo <- eg_10_cpx[portfolio==10, .(date, cp_High = round(vwret*100,2))] %>%
  left_join(eg_10_cpx[portfolio==1, .(date, cp_Low = round(vwret*100,2))], by="date") %>%
  mutate(cp_HiLo = cp_High - cp_Low)

mean(ia_HiLo$ia_HiLo)
mean(cp_HiLo$cp_HiLo)
as.dist(cor(ia_HiLo %>% inner_join(cp_HiLo, by="date") %>% select(ia_HiLo, cp_HiLo)))

## Fama-French 5 factors Alpha -------------------------------------------------

ff5factors <- read.csv("Data/F-F_Research_Data_5_Factors_2x3.CSV", skip = 3)
setDT(ff5factors)
names(ff5factors)[1] <- "date"
ff5factors$date <- as.Date(paste0(as.character(ff5factors$date), '01'), format='%Y%m%d')
dates <- ff5factors$date
ff5factors$date <- (seq(as.Date(dates[1]),length=length(dates)+1,by="months")-1)[-1]
ff5factors[(date>="1972-10-31" & date<="2019-12-31"),]

eg_10_ia[portfolio==10, .(date, High = round(vwret*100,2))] %>%
  left_join(eg_10_ia[portfolio==1, .(date, Low = round(vwret*100,2))], by="date") %>%
  mutate(EIG = High - Low) %>%
  left_join(ff5factors, by="date") -> db_ffIA

eg_10_cpx[portfolio==10, .(date, High = round(vwret*100,2))] %>%
  left_join(eg_10_cpx[portfolio==1, .(date, Low = round(vwret*100,2))], by="date") %>%
  mutate(EIG = High - Low) %>%
  left_join(ff5factors, by="date") -> db_ffCP

## q-model  Alpha --------------------------------------------------------------
qmodel <- read.csv("Data/q5_factors_monthly_2019a.csv")
setDT(qmodel)
qmodel

eg_10_ia[portfolio==10, .(date, High = round(vwret*100,2))] %>%
  left_join(eg_10_ia[portfolio==1, .(date, Low = round(vwret*100,2))], by="date") %>%
  mutate(EIG = High - Low) %>%
  mutate(year = year(date)) %>% mutate(month = month(date)) %>%
  select(year, month, EIG) %>%
  left_join(qmodel, by=c("year", "month")) -> db_qmIA


eg_10_cpx[portfolio==10, .(date, High = round(vwret*100,2))] %>%
  left_join(eg_10_ia[portfolio==1, .(date, Low = round(vwret*100,2))], by="date") %>%
  mutate(EIG = High - Low) %>%
  mutate(year = year(date)) %>% mutate(month = month(date)) %>%
  select(year, month, EIG) %>%
  left_join(qmodel, by=c("year", "month")) -> db_qmCP

## Todos os Resultados Juntos --------------------------------------------------
eg_10_ia %>% group_by(portfolio) %>% summarise(mean(vwret)*100)
eg_10_cpx %>% group_by(portfolio) %>% summarise(mean(vwret)*100)
mean(ia_HiLo$ia_HiLo)
mean(cp_HiLo$cp_HiLo)
as.dist(cor(ia_HiLo %>% inner_join(cp_HiLo, by="date") %>% select(ia_HiLo, cp_HiLo)))

NW(lm(EIG ~ Mkt.RF + SMB + HML + RMW + CMA, data = db_ffIA), Lag = 24)
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE, data = db_qmIA), Lag = 24)
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG, data = db_qmIA), Lag = 24)

NW(lm(EIG ~ Mkt.RF + SMB + HML + RMW + CMA, data = db_ffCP), Lag = 24)
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE, data = db_qmCP), Lag = 24)
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG, data = db_qmCP), Lag = 24)
################################################################################
# eg_m %>% group_by(date) %>% summarise(mean(eg))
# tail(seq.Date(from = as.Date("1972-06-30"),length.out = 571, by="month"))

# > mean(ia_HiLo$ia_HiLo)
# [1] 1.310353
# > mean(cp_HiLo$cp_HiLo)
# [1] 2.127879
# > NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG, data = db_qmCP), Lag = 24)
# (Intercept)  1.500802   0.277855  5.4014 1.006e-07 ***
# R_ROE       -0.266490   0.096871 -2.7510 0.0061482 **
#   R_EG         0.508474   0.134832  3.7712 0.0001811 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

ISent <- readxl::read_xlsx("Data/Investor_Sentiment_Data_20190327_POST.xlsx", sheet = 2)
names(ISent)[2] <- "ISent"
ISent <- ISent %>% select(yearmo, ISent) %>%
  mutate(ISent = as.numeric(ISent)) %>%
  filter(!is.na(ISent)) %>%
  mutate(date = as.Date(paste0(as.character(yearmo), '01'), format='%Y%m%d')) %>%
  select(date, ISent)
lubridate::day(ISent$date) = lubridate::days_in_month(ISent$date)

db_ffCP2 <- db_ffCP %>% left_join(ISent, by="date")
NW(lm(EIG ~ Mkt.RF + SMB + HML + RMW + CMA + ISent, data = db_ffCP2))

ISent <- ISent %>% mutate(year = year(date)) %>% mutate(month=month(date)) %>%
  select(year, month, ISent)
db_qmCP2 <- db_qmCP %>% left_join(ISent, by=c("year", "month"))
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG + ISent, data = db_qmCP2), Lag=24)

MSent <- readxl::read_xlsx("Data/JLMZ_MS_Index2017.xlsx")
names(MSent)[2] <- "MSent"
MSent$date <- lubridate::parse_date_time(MSent$date, "%Y-%m")
lubridate::day(MSent$date) = lubridate::days_in_month(MSent$date)

db_ffCP2 <- db_ffCP2 %>% left_join(MSent, by="date")
NW(lm(EIG ~ Mkt.RF + SMB + HML + RMW + CMA + MSent, data = na.omit(db_ffCP2)))

MSent <- MSent %>% mutate(year = year(date)) %>% mutate(month=month(date)) %>%
  select(year, month, MSent)
db_qmCP2 <- db_qmCP2 %>% left_join(MSent, by=c("year", "month"))
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG + MSent, data = na.omit(db_qmCP2)))

## Results Stargazer -----------------------------------------------------------
if (is.null(filterLifeCycle)) {
  dataALL <- list(eg_10_ia, eg_10_cpx, db_ffCP, db_qmCP, db_ffCP2, db_qmCP2)
  saveRDS(dataALL, "~/Data/EIG/dataPortALL.rds")
}
if (2 %in% filterLifeCycle) {
  dataGROWTH <- list(eg_10_ia, eg_10_cpx, db_ffCP, db_qmCP, db_ffCP2, db_qmCP2)
  saveRDS(dataGROWTH, "~/Data/EIG/dataPortGROWTH.rds")
}

if (3 %in% filterLifeCycle) {
  dataMATURE <- list(eg_10_ia, eg_10_cpx, db_ffCP, db_qmCP, db_ffCP2, db_qmCP2)
  saveRDS(dataMATURE, "~/Data/EIG/dataPortMATURE.rds")
}

rm(list=ls())
## Results Stargazer -----------------------------------------------------------
dbALL    <- readRDS("~/Data/EIG/dataPortALL.rds")
dbGROWTH <- readRDS("~/Data/EIG/dataPortGROWTH.rds")
dbMATURE <- readRDS("~/Data/EIG/dataPortMATURE.rds")

names(dbALL)    <- c("FRGserie", "EIGserie", "ff_eig", "qm_eig", "ff_eig_sent", "qm_eig_sent")
names(dbGROWTH) <- c("FRGserie", "EIGserie", "ff_eig", "qm_eig", "ff_eig_sent", "qm_eig_sent")
names(dbMATURE) <- c("FRGserie", "EIGserie", "ff_eig", "qm_eig", "ff_eig_sent", "qm_eig_sent")


RIG <- dbALL$FRGserie %>% group_by(portfolio) %>% summarise("d1I/A" = round(mean(eg),3))
EIG <- dbALL$EIGserie %>% group_by(portfolio) %>% summarise("E[IG]" = round(mean(eg),3))
vwRIG <- dbALL$FRGserie %>% group_by(portfolio) %>% summarise("r d1I/A"=round(mean(vwret)*100,2))
vwEIG <- dbALL$EIGserie %>% group_by(portfolio) %>% summarise("r E[IG]"=round(mean(vwret)*100,2))

# dbALL$EIGserie %>% group_by(portfolio) %>% summarise("t" = round(NW(lm(eg~1))[,"t value"],3))
# dbALL$EIGserie %>% group_by(portfolio) %>% summarise("t" = round(NW(lm(vwret~1))[,"t value"],3))

RIG %>%
  left_join(EIG, by="portfolio") %>%
  left_join(vwRIG, by="portfolio") %>%
  left_join(vwEIG, by="portfolio")

# Result q-factor --------------------------------------------------------------
tsmodel1 <- lm(EIG ~ R_MKT  + R_ME + R_IA + R_ROE   , data = dbALL$qm_eig_sent)
tsmodel2 <- lm(EIG ~ R_MKT  + R_ME + R_IA + R_ROE   , data = dbGROWTH$qm_eig_sent)
tsmodel3 <- lm(EIG ~ R_MKT  + R_ME + R_IA + R_ROE   , data = dbMATURE$qm_eig_sent)
tsmodel4 <- lm(EIG ~ R_MKT  + R_ME + R_IA + R_ROE + R_EG , data = dbALL$qm_eig_sent)
tsmodel5 <- lm(EIG ~ R_MKT  + R_ME + R_IA + R_ROE + R_EG , data = dbGROWTH$qm_eig_sent)
tsmodel6 <- lm(EIG ~ R_MKT  + R_ME + R_IA + R_ROE + R_EG , data = dbMATURE$qm_eig_sent)

# Adjust standard errors
robust.se.ts <- function(x, Lag=NULL) {
  require(lmtest)
  require(sandwich)
  coeff <- coeftest(x, vcov=NeweyWest(x, lag = Lag, prewhite = FALSE))
  output <- list()
  output$t.stat <- coeff[,"t value"]
  output$p.val <- coeff[,"Pr(>|t|)"]
  return(output)
}
robust_se1 <- robust.se.ts(tsmodel1)
robust_se2 <- robust.se.ts(tsmodel2)
robust_se3 <- robust.se.ts(tsmodel3)
robust_se4 <- robust.se.ts(tsmodel4)
robust_se5 <- robust.se.ts(tsmodel5)
robust_se6 <- robust.se.ts(tsmodel6)
stargazer(tsmodel1, tsmodel2, tsmodel3, tsmodel4, tsmodel5, tsmodel6,
          title = "EIG across the life-cycle stages and q-factor models",
          column.labels=c("All", "Growth", "Mature", "All", "Growth", "Mature"),
          se = list(robust_se1$t.stat,robust_se2$t.stat,robust_se3$t.stat,
                    robust_se4$t.stat,robust_se5$t.stat,robust_se6$t.stat),
          p = list(robust_se1$p.val, robust_se2$p.val,robust_se3$p.val,
                   robust_se4$p.val, robust_se5$p.val,robust_se6$p.val),
          omit.stat=c("f", "ser"),
          type="text")

# Result Fama-French factor ----------------------------------------------------
tsmodel1 <- lm(EIG ~ Mkt.RF + SMB  + HML  + RMW + CMA   , data = dbALL$ff_eig_sent)
tsmodel2 <- lm(EIG ~ Mkt.RF + SMB  + HML  + RMW + CMA   , data = dbGROWTH$ff_eig_sent)
tsmodel3 <- lm(EIG ~ Mkt.RF + SMB  + HML  + RMW + CMA   , data = dbMATURE$ff_eig_sent)
robust_se1 <- robust.se.ts(tsmodel1)
robust_se2 <- robust.se.ts(tsmodel2)
robust_se3 <- robust.se.ts(tsmodel3)
stargazer(tsmodel1, tsmodel2, tsmodel3,
          title = "EIG across the life-cycle stages and Fama-French 5 factor model",
          column.labels=c("All", "Growth", "Mature"),
          se = list(robust_se1$t.stat,robust_se2$t.stat,robust_se3$t.stat),
          p = list(robust_se1$p.val, robust_se2$p.val,robust_se3$p.val),
          type="text")

# Result ISent explain EIG -----------------------------------------------------
tsmodel1 <- lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG + ISent , data = dbALL$qm_eig_sent)
tsmodel2 <- lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG + ISent , data = dbGROWTH$qm_eig_sent)
tsmodel3 <- lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG + ISent , data = dbMATURE$qm_eig_sent)

robust_se1 <- robust.se.ts(tsmodel1)
robust_se2 <- robust.se.ts(tsmodel2)
robust_se3 <- robust.se.ts(tsmodel3)
stargazer(tsmodel1, tsmodel2, tsmodel3,
          title = "EIG and Investor Sentiment Index",
          column.labels=c("All", "Growth", "Mature"),
          se = list(robust_se1$t.stat,robust_se2$t.stat,robust_se3$t.stat),
          p = list(robust_se1$p.val, robust_se2$p.val,robust_se3$p.val),
          type="text")

# Modelos Explicam o Alpha?
NW(lm(EIG ~ Mkt.RF + SMB  + HML  + RMW + CMA   , data = db_ffCP))
NW(lm(EIG ~ R_MKT  + R_ME + R_IA + R_ROE       , data = db_qmCP))
NW(lm(EIG ~ R_MKT  + R_ME + R_IA + R_ROE + R_EG, data = db_qmCP))

# Variavel sentimento ajuda a explicar o alpha?
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG + ISent       , data = dbALL$qm_eig_sent))

# Variavel sentimento ajuda a explicar o alpha?
NW(lm(EIG ~ Mkt.RF + SMB + HML + RMW + CMA                   , data = dbALL$ff_eig_sent))
NW(lm(EIG ~ Mkt.RF + SMB + HML + RMW + CMA + ISent           , data = dbALL$ff_eig_sent))
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE                      , data = dbALL$qm_eig_sent))
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + ISent              , data = dbALL$qm_eig_sent))
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG               , data = dbALL$qm_eig_sent))
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG + ISent       , data = dbALL$qm_eig_sent))
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG + R_EG        , data = dbALL$qm_eig_sent))
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG + R_EG + ISent, data = dbALL$qm_eig_sent))

# Variavel manament Sent ajuda a explicar o alpha?
NW(lm(EIG ~ Mkt.RF + SMB + HML + RMW + CMA + MSent           , data = na.omit(dbALL$ff_eig_sent)))
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG + MSent       , data = na.omit(dbALL$qm_eig_sent)))
NW(lm(EIG ~ R_MKT + R_ME + R_IA + R_ROE + R_EG + R_EG + MSent, data = na.omit(dbALL$qm_eig_sent)))

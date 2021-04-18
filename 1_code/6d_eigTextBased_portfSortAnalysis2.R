# TODO Analyze why this file exist

library(tidyverse)
library(data.table)

lc   <- readRDS("~/Data/WRDS/raw_comp_life_cycle.rds")
HMXZ <- readRDS("~/Data/eig/hmxz.rds")

## Merge Life cycle Proxy and EIG (HMXZ)
lc <- lc %>% mutate(date = datadate) %>% select(gvkey, date, LC) %>% as_tibble
eg_m <- HMXZ %>% select(permno, gvkey, date, eg=EIG) %>% na.omit %>% as_tibble
eg_m <- eg_m %>% left_join(lc, by = c("gvkey", "date"))

### Criar carteiras ------------------------------------------------------------
#
# Codigo em desenvolvimento baseado no criaCarteiras
# https://github.com/kleberformiga/criaCarteiras/blob/master/R/criaCartFatRisco.R
#

## Carregar dados crsp
# crsp <- readRDS("~/data/WRDS/crsp4.rds")
# crsp_retadj <- readRDS("~/data/WRDS/cleaned_crsp_m.rds")
# crsp_retadj <- crsp_retadj %>% select(permno, date, retadj)
# crsp <- crsp %>% left_join(crsp_retadj, by = c("permno", "date"))
# crsp

py_ccm4 <- arrow::read_feather("~/Data/eig/ccm4.feather")
py_ccm4 <- py_ccm4 %>%
  select(permno, date=jdate, szport, retadj, wt) %>%
  mutate(date = as.Date(date))
eg_m <- eg_m %>% left_join(py_ccm4, by = c("permno", "date")) %>%  drop_na

# Importar ccm4 from python

## All
DT <- as.data.table(eg_m)
DT[                      eg<=quantile(eg,0.2), portfolio := 1, by=date]
DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.4), portfolio := 2, by=date]
DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.6), portfolio := 3, by=date]
DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.8), portfolio := 4, by=date]
DT[eg>quantile(eg,0.8),                        portfolio := 5, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) -> eg_5portAll

DT <- as.data.table(eg_m)
DT[                      eg<=quantile(eg,0.1), portfolio := 1, by=date]
DT[eg>quantile(eg,0.1) & eg<=quantile(eg,0.2), portfolio := 2, by=date]
DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.3), portfolio := 3, by=date]
DT[eg>quantile(eg,0.3) & eg<=quantile(eg,0.4), portfolio := 4, by=date]
DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.5), portfolio := 5, by=date]
DT[eg>quantile(eg,0.5) & eg<=quantile(eg,0.6), portfolio := 6, by=date]
DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.7), portfolio := 7, by=date]
DT[eg>quantile(eg,0.7) & eg<=quantile(eg,0.8), portfolio := 8, by=date]
DT[eg>quantile(eg,0.8) & eg<=quantile(eg,0.9), portfolio := 9, by=date]
DT[eg>quantile(eg,0.9),                        portfolio := 10, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) -> eg_10portAll

## Growth Ports
DT <- as.data.table(eg_m)[LC <= 2] # Introduction and Growth
DT[                      eg<=quantile(eg,0.2), portfolio := 1, by=date]
DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.4), portfolio := 2, by=date]
DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.6), portfolio := 3, by=date]
DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.8), portfolio := 4, by=date]
DT[eg>quantile(eg,0.8),                        portfolio := 5, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) -> eg_5portGrowth

DT <- as.data.table(eg_m)[LC <= 2]
DT[                      eg<=quantile(eg,0.1), portfolio := 1, by=date]
DT[eg>quantile(eg,0.1) & eg<=quantile(eg,0.2), portfolio := 2, by=date]
DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.3), portfolio := 3, by=date]
DT[eg>quantile(eg,0.3) & eg<=quantile(eg,0.4), portfolio := 4, by=date]
DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.5), portfolio := 5, by=date]
DT[eg>quantile(eg,0.5) & eg<=quantile(eg,0.6), portfolio := 6, by=date]
DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.7), portfolio := 7, by=date]
DT[eg>quantile(eg,0.7) & eg<=quantile(eg,0.8), portfolio := 8, by=date]
DT[eg>quantile(eg,0.8) & eg<=quantile(eg,0.9), portfolio := 9, by=date]
DT[eg>quantile(eg,0.9),                        portfolio := 10, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) -> eg_10portGrowth

## Mature Ports
DT <- as.data.table(eg_m)[LC == 3] # Mature
DT[                      eg<=quantile(eg,0.2), portfolio := 1, by=date]
DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.4), portfolio := 2, by=date]
DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.6), portfolio := 3, by=date]
DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.8), portfolio := 4, by=date]
DT[eg>quantile(eg,0.8),                        portfolio := 5, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) -> eg_5portMature

DT <- as.data.table(eg_m)[LC == 3]
DT[                      eg<=quantile(eg,0.1), portfolio := 1, by=date]
DT[eg>quantile(eg,0.1) & eg<=quantile(eg,0.2), portfolio := 2, by=date]
DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.3), portfolio := 3, by=date]
DT[eg>quantile(eg,0.3) & eg<=quantile(eg,0.4), portfolio := 4, by=date]
DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.5), portfolio := 5, by=date]
DT[eg>quantile(eg,0.5) & eg<=quantile(eg,0.6), portfolio := 6, by=date]
DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.7), portfolio := 7, by=date]
DT[eg>quantile(eg,0.7) & eg<=quantile(eg,0.8), portfolio := 8, by=date]
DT[eg>quantile(eg,0.8) & eg<=quantile(eg,0.9), portfolio := 9, by=date]
DT[eg>quantile(eg,0.9),                        portfolio := 10, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) -> eg_10portMature


## Analyse portfolios Deciles --------------------------------------------------
eg_10portAll %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3)) %>% select(retorno) %>% data.frame
eg_10portGrowth %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3)) %>% select(retorno) %>% data.frame
eg_10portMature %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3)) %>% select(retorno) %>% data.frame

## Analyse portfolios Quintiles
eg_5portAll %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3)) %>% select(retorno) %>% data.frame
eg_5portGrowth %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3)) %>% select(retorno) %>% data.frame
eg_5portMature %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3)) %>% select(retorno) %>% data.frame


## Test c/ quintis -------------------------------------------------------------
# lc   <- readRDS("~/Data/WRDS/raw_comp_life_cycle.rds")
# HMXZ <- readRDS("~/Data/eig/hmxz.rds")
#
# ## Merge Life cycle Proxy and EIG (HMXZ)
# lc <- lc %>% mutate(date = datadate) %>% select(gvkey, date, LC) %>% as_tibble
# eg_m <- HMXZ %>% select(permno, gvkey, date, eg=EIG) %>% na.omit %>% as_tibble
# eg_m <- eg_m %>% left_join(lc, by = c("gvkey", "date"))
#
# ## Pode filtrar para analisar individualmente
# # eg_m <- eg_m %>% filter(LC == 1) # Introduction and Growth
# # eg_m <- eg_m %>% filter(LC <= 2) # Introduction and Growth
# # eg_m <- eg_m %>% filter(LC == 2) # Only growths
# # eg_m <- eg_m %>% filter(LC == 3) # Mature
# # eg_m <- eg_m %>% filter(LC == 4) # Shake-Out
# # eg_m <- eg_m %>% filter(LC >= 3) # no_growths
#
# # eg_m <- eg_m %>% filter(LC <= 2) # Introduction and Growth
# # ## retorno
# # ## 1  -0.447
# # ## 2  -0.240
# # ## 3  -0.479
# # ## 4   0.831
# # ## 5   0.511 ## Relacao quase inversa
#
# # eg_m <- eg_m %>% filter(LC == 3) # Mature
# # ## retorno
# # ## 1   0.511
# # ## 2   1.054
# # ## 3   0.883
# # ## 4   0.657
# # ## 5   0.535 ## Relacao quase inversa5   0.535 ## Relacao quase inversa
#
# DT <- as.data.table(eg_m)
# DT[                      eg<=quantile(eg,0.2), portfolio := 1, by=date]
# DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.4), portfolio := 2, by=date]
# DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.6), portfolio := 3, by=date]
# DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.8), portfolio := 4, by=date]
# DT[eg>quantile(eg,0.8),                        portfolio := 5, by=date]
# as_tibble(DT) %>% group_by(portfolio, date) %>%
#   summarise(ewret = mean(retadj),
#             vwret = weighted.mean(retadj, wt)) %>%
#   arrange(date) -> eg_5portAll
# eg_5portAll %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3)) %>% select(retorno) %>% data.frame

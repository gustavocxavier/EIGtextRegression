library(tidyverse)
library(data.table)
library(TextForecast)
library(edgar)
library(foreach)

# source("C:\\Dropbox\\[projetos]\\docCode\\textForecast_funs.R")
# source("/home/gcx/Dropbox/[projetos]/docCode/textForecast_funs.R")
source('functions_eig.R')

## Check the data --------------------------------------------------------------
setwd("D:/Data_SEC")
# setwd("/home/gcx/Dropbox/[projetos]/docCode")

## Load all years index
t0 <- Sys.time()         ## Registrando inicio da execucao
# load(file = "Master Indexes/1994_2019master.RData") # 118 MB / 18 sec
db <- fst::read.fst("~/Data/SEC_EDGAR/1994_2019master.fst") # 611 MB / 12 sec /2.9 mins
db <- as.data.table(db)
Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao

## Checando o que ja foi baixado e o que falta (Codigo bem lento)
# Codigo lento (De 10 a 15 min.)
t0 <- Sys.time()         ## Registrando inicio da execucao
db[year==2002, file_status := file.exists(file_name)]
Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao

## Verificar
# db %>% select(edgar.link, file_name) %>% data.frame %>% head(3)

db[, date.filed := as.Date(date.filed)]
db[, year := year(date.filed)]

# ## Ver frequencia de arquivos baixados/nao baixados por ano
# db %>%
#   group_by(year, file_status) %>%
#   summarise(n = n()) %>%
#   mutate(freq = round((n / sum(n))*100) ) %>%
#   # filter(freq != 100) %>%
#   data.frame

## Lista anos que estao completo
db %>%
  group_by(year, file_status) %>%
  summarise(n = n()) %>% 
  mutate(freq = round((n / sum(n))*100) ) %>%
  filter( file_status==T & freq==100 )  %>% 
  data.frame

# ## Salvar RData
# t0 <- Sys.time()         ## Registrando inicio da execucao
# save(db, file = "Master Indexes/1994_2019master.RData")
# Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao

## Salvar fst
t0 <- Sys.time()         ## Registrando inicio da execucao
fst::write.fst(db, "~/Data/SEC_EDGAR/1994_2019master.fst")
Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao

# ## Para fazer somente um ano especifico
# db2 <- db[year(as.Date(date.filed))==1996]
# system.time(db2[, file_status := file.exists(file_name)])
# db2 %>% group_by(file_status) %>% count
# rm(db2)



## Baixar os que estao faltando ------------------------------------------------

## Lista anos que estao faltando (do que falta menos para o que falta mais)
db %>%
  group_by(year, file_status) %>%
  summarise(n = n()) %>%
  mutate(percentual = round((n / sum(n))*100) ) %>%
  # filter(file_status == F) %>% filter(n < 1000) %>% # Faltando menos que 10
  # filter(freq != 100) %>%
  filter(file_status == F) %>%
  # arrange(n) %>%
  arrange(year) %>%
  data.frame
# 
# library(edgar)
# setwd("D:/Data_SEC")
# ANO <- 2006
# db[(file_status == FALSE) & (year == ANO)]
# baixar <- db %>% filter( (file_status == FALSE) & (year == ANO))
# getFilings(cik.no = baixar$cik, form.type = "10-K", filing.year = ANO, downl.permit = "y")
# db[(file_status == FALSE) & (year == ANO), file_status := file.exists(file_name)]
# db[(file_status == FALSE) & (year == ANO)]

## MD&A section text -----------------------------------------------------------
ano <- 2002
empresas <- db[year(as.Date(date.filed))==ano]$cik %>% as.character
rm(db)
output <- getMgmtDisc(cik.no = empresas, filing.year = ano)
saveRDS(output, paste0("MD&A output/MD&Aoutput", ano, ".rds"))


## Organize files and directories ----------------------------------------------

## Copiar os arquivos de cada empresa para sua respectiva pasta

original_path <- paste0("MD&A section text") ; original_path
original_files <- dir(original_path) ; original_files[1:2]
original_files[-grep(".txt", original_files)] # So as pastas
original_files <- original_files[grep(".txt", original_files)] # So os TXTs
original_files <- original_files[grep(ano, original_files)] # somente do ano desejado

# # copiar so os arquivos txt
# original_path <- paste0("MD&A section text/_", ano) ; original_path
# original_files <- dir(original_path) ; original_files[1:2]

empresas <- strsplit(original_files, "_") ; head(empresas, 2)
empresas <- sapply(empresas, "[[", 1) ; head(empresas, 2)

## Criar uma pasta do ano e uma subpasta para cada empresa
destination_path <- paste0("MD&A section text/y", ano)
dir.create(destination_path)
progress.bar <- txtProgressBar(min = 0, max = length(empresas), style = 3)
for (i in 1:length(empresas)) {
  dir.create(paste0(destination_path,"/", empresas[i]))
  setTxtProgressBar(progress.bar, i)
}
rm(i, progress.bar)

## Copiar da pasta temporaria para pasta destino
destination_files <- paste0(destination_path, "/", empresas, "/", original_files)
destination_files[1:2]

original_files <- paste0(original_path, "/", original_files)
original_files[1:2]

progress.bar <- txtProgressBar(min = 0, max = length(destination_files), style = 3)
for (i in 1:length(destination_files)) {
  file.copy(original_files[i], destination_files[i])
  setTxtProgressBar(progress.bar, i)
}
rm(i, progress.bar)

progress.bar <- txtProgressBar(min = 0, max = length(original_files), style = 3)
for (i in 1:length(original_files)) {
  file.remove(original_files[i])
  setTxtProgressBar(progress.bar, i)
}
rm(i, progress.bar)

## Merge Data with COMP_A ------------------------------------------------------
# saveRDS(comp_a, file = "c:\\Data\\comp_a.rds")
comp_a <- readRDS("c:\\Data\\WRDS\\comp_a.rds")

comp_a <- comp_a %>%
  mutate(year=year(datadate)) %>%
  filter(year==1994) %>% 
  select(gvkey, datadate, year, d1_ia)

comp_a <- comp_a %>% arrange(gvkey, datadate)

## Remover duplicadas
comp_a <- comp_a[!duplicated(comp_a[,c("gvkey", "year")]),] %>% 
  select(gvkey, year, d1_ia)

## Baixar o ultimo link feito
load("D:/Data_SEC/Outros .RData/2020-03-10.RData")
rm(cik, cik2, comp, raw_comp, raw_scm, res, scm, wrds)

comp_linked2 <- comp_linked %>% filter(fyear==1994)
ig <- comp_a %>%
  left_join(comp_linked2, by = c("gvkey")) %>% 
  select(year, gvkey, d1_ia, cik) %>% na.omit %>% 
  filter(cik %in% empresas) %>% arrange(cik)
ig <- ig[!duplicated(ig[,c("cik")]),] 
empresas <- empresas[(empresas %in% ig$cik)]

identical(empresas, ig$cik)
data.frame("cik" = head(empresas),
           "y"   = head(ig$d1_ia))

## Match CIK with PERMNO from CCM #############################################
# Use the Wharton Research Data Services (WRDS) CIK-PERMNO file to match CIK
# with PERMNO from the CRSP-COMPUSTAT Merged database
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user='gxavier',
                  password=getPass::getPass(),
                  dbname='wrds',
                  sslmode='require')
res <- dbSendQuery(wrds, "SELECT * FROM WRDSSEC.WCIKLINK_GVKEY")
# Salvar SEC Compustat Merge (CIK link GVKEY)
scm <- as_tibble(dbFetch(res, n=-1)) ; dbClearResult(res)
dbDisconnect(wrds)



## Merge with CCM (d1I/A availeble) ## ########################################
# Winsorize all variables at the 1st and 99th percentiles of their distributions
# each month. 
setwd("C:\\Dropbox\\[projetos]\\docCode")
library(data.table)
library(dplyr)
source("C:\\Dropbox\\[projetos]\\docCode\\textForecast_funs.R")

crsp4 <- readRDS("WRDS\\crsp4.rds")

DT <- as.data.table(select(crsp4,-cop, -q, -dROE))
DT[, d1_ia  := winsorizar(d1_ia),  by = c("date")]
DT <- DT[!is.na(d1_ia)]
DT[, ig      := d1_ia]
DT[, ig_lag := shift(ig), by=c("permno", "fiscaldate")]

DT %>% select(permno, fiscaldate, ig, ig_lag) %>%
  filter(permno==10001) %>% data.frame %>% head(50)
DT %>% filter(permno==10001 & is.na(ig_lag)) %>% 
  select(permno, fiscaldate, ig)

ig <- DT[is.na(ig_lag), .(permno, gvkey, date, monthlink, fiscaldate, rdq, ig, me)]

ig <- ig %>% mutate(year = year(fiscaldate)) %>% filter(year>1992)

ig %>% group_by(permno, year) %>% count %>% filter(n>1)
ig %>% filter(permno==11042 & year==1999)
ig %>% filter(permno==10200 & year==2011)

load("D:\\Data_SEC\\Master Indexes\\2011master.Rda") ; sec <- year.master; rm(year.master)

View(sec %>% filter(form.type=="10-K"))


## >>> Ger merged data.base ---------------------------------------------------
# sccm_a <- readRDS("c:\\Dropbox\\[projetos]\\docCode\\WRDS\\sccm_a.rds")
sccm_a <- readRDS("WRDS/sccm_a.rds")

## Quantidade total de observacoes year - firms
nrow(sccm_a)

## Retirar as observacoes que nao tem disponivel variavel dependente d1 I/A
sccm_a <- sccm_a %>% filter(!is.na(d1_ia)) ; nrow(sccm_a)

## Retirar as observacoes que nao tem disponivel cik valido
sccm_a <- sccm_a %>% filter(!is.na(cik)) ; nrow(sccm_a)

## Criar year.filed e ver quantidade por ano
sccm_a <- sccm_a %>% mutate(year.filed = fyear+1)
sccm_a %>% ungroup %>% select(fyear, year.filed) %>% 
  group_by(fyear, year.filed) %>%
  count %>% filter(n>0) %>% data.frame


# ## Load one specific year
# load("Master Indexes/1994master.Rda")
# db <- year.master %>% filter(form.type == "10-K") %>% as_tibble
# rm(year.master)
#
# db <- db %>%
#   filter(form.type=="10-K") %>%
#   mutate(file_name = as.character(edgar.link) ) %>%
#   mutate(file_name = str_remove(file_name, paste0("edgar/data/",cik,"/")) ) %>%
#   mutate(file_name = paste0("Edgar filings_full text/Form 10-K/",
#                             cik,"/", cik, "_10-K_",
#                             date.filed,"_", file_name)) %>% as.data.table
#
# db[, file_status := file.exists(file_name)]

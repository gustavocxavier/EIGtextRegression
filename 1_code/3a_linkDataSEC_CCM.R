## Link CIK with GVKEY
#

library(data.table)
library(dplyr)
source('1_code/functions.R')

## Load raw data --------------------------------------------------------------
wciklink_gvkey <- readRDS("0_data/wrds/wciklink_gvkey.rds")
ccm_a          <- readRDS("2_pipeline/2_out/1b_ccm_a.rds")
# sec_index      <- fst::read_fst("~/data/SEC_EDGAR/1994_2019master.fst")

## Select link validation type: flag == (2) or (3) ----------------------------
#
# Validation type has the following values:
#   0. - is for unconfirmed links, or invalid CUSIPs that were not found in the
#        CUSIP Master dataset. Users should not use those links, especially when
#        the frequency of occurrences (TMATCH variable) is less than 10.
#   1. - denotes links with valid CUSIP but with different company names, and
#        are considered unconfirmed links.
#   2. - represents CIK-CUSIP links for companies that have a valid 8-digit
#        CUSIP and matching company name in the CUSIP Master dataset.
#   3. - is for CIK-CUSIP links with 9-digit CUSIPs that were found in SEC
#        filings that match the CUSIPs and respective company names in the CUSIP
#        bureau dataset.
# 
# We advise researchers to use only (2) and (3) type links when using this table
# for linking purposes.
#
scm_link <- wciklink_gvkey %>% filter(flag %in% c(2,3))

## Select only lines with at least one 10-k
scm_link <- scm_link %>% filter(n10k>0) %>% select(-(n10k_nt:flag))

## Remove NA 2.629 links em que o datadate tá NA
scm_link %>% filter_at(vars(everything()), any_vars(is.na(.)))
scm_link <- na.omit(scm_link)



## Select CIKs with valid MD&A ------------------------------------------------
years <- 1994:2019
available_cik <- tibble(filingY = years[1],
                        cik = readRDS(paste0("0_data/DTM/corpus_firms",
                                             years[1], ".rds")) )
for (i in 2:length(years)) {
  available_cik <- rbind(available_cik,
                         tibble(filingY = years[i],
                                cik = readRDS(paste0("0_data/DTM/corpus_firms",
                                                     years[i], ".rds")) 
                         )
  )
} 
rm(i, years)

## Select GVKEYS with valid d1I/A ---------------------------------------------
ccm_a %>%
  mutate(fiscalY = year(datadate)) %>% 
  mutate(filingY = fiscalY + 1) %>% 
  select(filingY, gvkey:datadate, fiscalY, d1_ia, d0_ia) %>%
  na.omit -> ccm_a2

crsp4 <- readRDS("2_pipeline/2_out/1b_crsp4.rds")
crsp4 <- crsp4 %>% select(permno, gvkey, monthlink, q, cop, dROE,
                          Ret, SG, CFG, CF, cop, PG, EG, dROE, Ie, Id,
                          me)

ccm_a2 %>% left_join(crsp4, by = c("permno", "gvkey", "monthlink")) %>% 
  # filter(filingY >= 1994) %>% 
  na.omit %>% group_by(datadate)    %>%
  mutate(d1_ia = winsorizar(d1_ia)) %>%
  mutate(d0_ia = winsorizar(d0_ia)) %>%
  mutate(q     = winsorizar(q))     %>% 
  mutate(cop   = winsorizar(cop))   %>% 
  mutate(dROE  = winsorizar(dROE))  %>%
  
  mutate(Ret = winsorizar(Ret )) %>% 
  mutate(SG  = winsorizar(SG  )) %>% 
  mutate(CFG = winsorizar(CFG )) %>% 
  mutate(CF  = winsorizar(CF  )) %>% 
  mutate(PG  = winsorizar(PG  )) %>% 
  mutate(EG  = winsorizar(EG  )) %>% 
  mutate(Ie  = winsorizar(Ie  )) %>% 
  mutate(Id  = winsorizar(Id  )) %>% 
  
  ungroup %>% na.omit %>% 
  filter(filingY %in% available_cik$filingY) -> available_gvkey

as.data.table(available_gvkey)
as.data.table(available_cik)

# Numero de companhias por ano com CIK e GVKEY
available_gvkey %>% group_by(filingY) %>% count %>% 
  left_join(available_cik %>% group_by(filingY) %>% count, by="filingY") %>%
  select(Filing.Year=filingY, GVKEY=n.x, CIK=n.y) %>% data.frame

## Organize scm link ----------------------------------------------------------

## Transform scm_link to organize
scm_link <- scm_link %>%
  mutate(fiscaly0 = year(fndate) - 1) %>%
  mutate(fiscalyT = year(lndate) - 1) %>%
  mutate(cik = as.numeric(cik)) %>% 
  select(gvkey, cik, source, n10k, fiscaly0, fiscalyT,
         SECname=coname, COMPname=conm)

# CIKs linkados a + de 1 GVKEYs: # 843
duplicated_cik   <- scm_link %>% select(gvkey, cik) %>% group_by(cik) %>% count %>% filter(n>1)
duplicated_cik %>% nrow
# GVKEYs linkados a + de 1 CIK: # 1.168
duplicated_gvkey <- scm_link %>% select(gvkey, cik) %>% group_by(gvkey) %>% count %>% filter(n>1)
duplicated_gvkey %>% nrow

scm_link %>% filter(gvkey %in% duplicated_gvkey$gvkey)
scm_link %>% filter(cik   %in% duplicated_cik$cik) %>% arrange(cik)

# # Lead with overlap -----------------------------------------------------------
# two_or_more_gvkey <- scm_link %>%
#   select(gvkey, cik) %>%
#   group_by(gvkey) %>%
#   count %>%
#   filter(n>1)
# two_or_more_gvkey <- two_or_more_gvkey$gvkey
# 
# ## Analisa gvkey duplicados com overlap
# scm_link %>% filter(gvkey %in% two_or_more_gvkey) %>% arrange(as.numeric(gvkey)) %>%
#   group_by(gvkey) %>%
#   filter( (fiscaly0 < dplyr::lag(fiscalyT)) | (dplyr::lead(fiscalyT) > fiscalyT) )
# # As you can see (in some cases), the "First Filing Date" might in some
# # instances overlap, or exhibit a gap with the "Last Filing Date" of the
# # previous name structure. For example, in the case of Six Flags Inc, its first
# # filing date is March 27, 20x07, while the last filing date with the previous
# # name record was on March 16, 2007.


## Create data.table to operate large data
DT <- as.data.table(scm_link %>% select(gvkey, cik, source, fiscaly0, fiscalyT) ) # com todas

DT[, y := (fiscalyT - fiscaly0 + 1)] # Qtd. de anos
DT <- DT[y>0,]
DT <- DT %>% slice(rep(1:n(), times = y)) 

DT[, fiscalY := sequence(.N)+fiscaly0-1, by = c("gvkey", "cik")]
DT <- DT %>% select(-fiscaly0, -fiscalyT)
DT <- DT %>% arrange(as.numeric(gvkey), fiscalY) %>% as.data.table

## Filter only gvkey and cik with valid data
available_gvkey %>% mutate(valid_gvkey = T) -> available_gvkey
available_cik   %>% mutate(valid_cik   = T) -> available_cik
DT %>%
  left_join(available_gvkey, by = c("fiscalY", "gvkey")) %>%
  mutate(cik = as.numeric((cik)) ) %>% 
  left_join(available_cik, by = c("filingY", "cik")) %>% 
  filter(valid_gvkey == TRUE) %>%
  filter(valid_cik == TRUE) %>% 
  select(permno, gvkey:source, filingY, y, everything()) %>% 
  select(-valid_gvkey, -valid_cik) -> DT

# ## Duplicated gvkeys and ciks
# DT %>% select(gvkey, cik, filingY) %>% group_by(filingY, cik) %>% count %>% filter(n>1) %>% nrow
# DT %>% select(gvkey, cik, filingY) %>% group_by(filingY, gvkey) %>% count %>% filter(n>1) %>% nrow

# DT <- DT[filingY==1994 & (cik ==48305 | gvkey=="001311")] # Toy data to test
DT[source == "FDATE", priority_source := 4]
DT[source == "CUSIP", priority_source := 3]
DT[source == "COMPN", priority_source := 2]
DT[source == "COMPH", priority_source := 1]


# Choose only one gvkey for each cik based on number of years and priorities sources
DT <- DT %>% group_by(filingY) %>% arrange (gvkey, -y, priority_source) %>% filter (!duplicated(gvkey))

# Choose only one cik for each gvkey based on number of years and priorities sources
DT <- DT %>% group_by(filingY) %>% arrange (cik, -y, priority_source) %>% filter (!duplicated(cik))

## Final Sample ---------------------------------------------------------------

total_by_year <- DT %>% group_by(filingY) %>% count %>% data.frame
total_by_year
round(mean(total_by_year$n)) # 3394(1,2,3) / 3398(1,3,2) / 3399 (1,2,3)

DT

sccm_a <- DT %>% select(filing.year=filingY, cik, permno, gvkey, d1_ia:me) %>% arrange(filing.year)

rm(available_cik, available_gvkey, ccm_a, ccm_a2, crsp4, DT, duplicated_cik, duplicated_gvkey,
   scm_link, total_by_year, wciklink_gvkey)

saveRDS(sccm_a, "2_pipeline/2_out/3a_sccm_a.rds")

# ## Analyse specific companies
# scm_link %>% filter(gvkey=="006066")     # IBM
# scm_link %>% filter(gvkey=="160329")     # Google
# scm_link %>% filter(gvkey == "006307")   # K-Mart
# scm_link %>% filter(gvkey == "004034")   # DOMTAR INC
# scm_link %>% filter(cik == "0000005907") # AT&T
# scm_link %>% group_by(source) %>%  count
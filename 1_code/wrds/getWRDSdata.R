library(RPostgres)
## >> RETRIEVE WRDS DATA ## ####################################################
## Load data from WRDS Server

# Connect with WRDS Server -----------------------------------------------------
wrdsConnection <- dbConnect(Postgres(),
                            host='wrds-pgdata.wharton.upenn.edu',
                            port=9737,
                            user='gxavier',
                            password=getPass::getPass(),
                            dbname='wrds',
                            sslmode='require')

# Retrieve Compustat annual data -----------------------------------------------
myQuery <- paste0("select GVKEY, DATADATE, AT, DLTT, DLC,
                   TXDITC, SEQ, CEQ, PSTK, LT, PSTKL, PSTK, PSTKRV,
                   REVT, COGS, XSGA, XRD, RECT, INVT, XPP, DRC, DRLT, AP, XACC,
                   IB, CAPX, SICH, CIK,
                   SALE, NI, DP, PPEGT, SSTK, CSHO, AJEX, EBITDA, XINT, IDIT,
                   TXT, TXDC, FYEAR
                   from COMP.FUNDA
                   where INDFMT='INDL'
                   and DATAFMT='STD'
                   and POPSRC='D'
                   and CONSOL='C'
                   and DATADATE between \'", begdate, "\' and \'",enddate,"\'")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
comp_a <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)

# Retrieve GICS from Compustat data --------------------------------------------
myQuery <- paste0("select GVKEY, GIND, GSECTOR, GSUBIND
                   from comp.company
                  ")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
gics <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)

# Retrieve Name Data Compustat -------------------------------------------------
myQuery <- paste0("select TIC, GVKEY, CUSIP, CIK, CONM,
                   GIND, GSUBIND, NAICS, SIC, YEAR1, YEAR2
                   from comp.names
                  ")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
comp_names <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)
comp_names %>% filter(tic=="GOOG")


# Retrieve Compustat quartely data ---------------------------------------------
myQuery <- paste0("select GVKEY, DATADATE, IBQ, RDQ,
                          PSTKQ, PSTKRQ, SEQQ, CEQQ, ATQ, LTQ, TXDITCQ
                          FQTR, FYEARQ
                   from COMP.FUNDQ
                   where INDFMT='INDL'
                   and DATAFMT='STD'
                   and POPSRC='D'
                   and CONSOL='C'
                   and DATADATE between \'", begdate, "\' and \'",enddate,"\'")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
comp_q <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)

# Retrieve Merged Compustat/CRSP link table ------------------------------------
wrdsResult <- dbSendQuery(wrdsConnection,"select GVKEY, LPERMNO, LINKDT,
                                          LINKENDDT, LINKTYPE, LINKPRIM
                                          from crsp.ccmxpf_lnkhist")
wrdsResult <- dbSendQuery(wrdsConnection,"SELECT gvkey, lpermno as permno,
                                          linktype, linkprim,
                                          linkdt, linkenddt
                                          FROM crsp.ccmxpf_linktable
                                          WHERE substr(linktype,1,1)='L'
                                          AND (linkprim ='C' OR linkprim='P')")
ccmlink <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)

# Retrieve CRSP data at once ---------------------------------------------------
myQuery <- paste0("SELECT a.permno, a.permco, a.date,
                          b.shrcd, b.exchcd, b.siccd,
                          a.ret, a.retx, a.shrout, a.prc
                   FROM crsp.msf as a
                   LEFT JOIN crsp.msenames as b
                          ON a.permno=b.permno
                          AND b.namedt<=a.date
                          AND a.date<=b.nameendt
                   WHERE a.date   BETWEEN \'", begdate, "\' AND \'",enddate,"\'
                   AND   b.exchcd BETWEEN 1 AND 3")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
crsp_m <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)

# Retrieve delisting returns ---------------------------------------------------
myQuery <- paste0("select permno, dlret, dlstdt
                   from crsp.msedelist")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
dlret <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)

# Retrieve CRSP names ----------------------------------------------------------
myQuery <- paste0("SELECT permno, permco, ticker, tsymbol, comnam, namedt,
                          nameendt, ncusip, cusip, siccd, naics 
                   FROM crsp.msenames")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
crsp_names <- dbFetch(wrdsResult, n = -1)
setDT(crsp_names)
crsp_names %>% arrange(permno, namedt) %>% filter(ticker=="GOOG")
dbClearResult(wrdsResult)

## Retrieve WRDS_CIK LINK
## SEC Compustat Merge (CIK link GVKEY)
myQuery <- "SELECT * FROM WRDSSEC.WCIKLINK_GVKEY"
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
scm <- as_tibble(dbFetch(wrdsResult, n=-1))
dbClearResult(wrdsResult)


# Retrieve Compustat annual data to compute life cycle proxies -----------------
myQuery <- paste0("SELECT gvkey, datadate, fyear,
                   at, sale, re, ebit, prcc_c, csho,
                   oancf, ivncf, fincf
                   FROM COMP.FUNDA
                   WHERE INDFMT='INDL'
                   AND DATAFMT='STD'
                   AND POPSRC='D'
                   AND CONSOL='C'
                   AND DATADATE between \'", begdate, "\' and \'",enddate,"\'")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
lcycle <- dbFetch(wrdsResult, n = -1)

dbClearResult(wrdsResult)

dbDisconnect(wrdsConnection)
setDT(lcycle)

## Save db localy --------------------------------------------------------------
saveRDS(lcycle, "Data/WRDS/raw_comp_a_lifecycle_data.rds")


saveRDS(comp_names,   file = "Data/WRDS/raw_comp_names.rds")
saveRDS(comp_a,   file = "Data/WRDS/raw_comp_a.rds")
saveRDS(gics,  file = "Data/WRDS/raw_gics.rds")
saveRDS(comp_q,   file = "Data/WRDS/raw_comp_q.rds")
saveRDS(ccmlink,  file = "Data/WRDS/raw_ccmlink.rds")
saveRDS(crsp_m,   file = "Data/WRDS/raw_crsp_m.rds")
saveRDS(crsp_names,   file = "Data/WRDS/raw_crsp_names.rds")
saveRDS(dlret,    file = "Data/WRDS/raw_dlret.rds")
saveRDS(scm, "Data/WRDS/wciklink_gvkey.rds")

dbDisconnect(wrdsConnection)

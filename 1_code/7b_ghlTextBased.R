# Namely variables as:
#
# FIG: Future Investment Growth
# EIG: Expected Investment Growth
# HMXZ (2020)
# LWI (2020)
# GHL (2018)
# Model 1: cross-section estimation
# Model 2: industry estimation
# Model 3: life-cycle estimation


# FIGhmxz      / FIGhmxz_fy2  / FIGhmxz_fy3
# EIGhmxz1     / EIGhmxz2     / EIGhmxz3
# EIGhmxz1_fy2 / EIGhmxz2_fy2 / EIGhmxz3_fy2
# EIGhmxz1_fy3 / EIGhmxz2_fy3 / EIGhmxz3_fy3
#
# FIGlwi      / FIGlwi_fy2  / FIGlwi_fy3
# EIGlwi1     / EIGlwi2     / EIGlwi3     # Model 1: cross-section estimation
# EIGlwi1_fy2 / EIGlwi2_fy2 / EIGlwi3_fy2 # Model 2: industry estimation
# EIGlwi1_fy3 / EIGlwi2_fy3 / EIGlwi3_fy3 # Model 3: life-cycle estimation
#
# FIGghl      / FIGghl_fy2  / FIGghl_fy3
# EIGghl1     / EIGghl2     / EIGghl3     # Model 1: cross-section estimation
# EIGghl1_fy2 / EIGghl2_fy2 / EIGghl3_fy2 # Model 2: industry estimation
# EIGghl1_fy3 / EIGghl2_fy3 / EIGghl3_fy3 # Model 3: life-cycle estimation
#
# FIG1
# EIG1
# FIG2
# EIG2
# FIG3
# EIG3

## Load libraries and data -----------------------------------------------------
library(data.table)
library(glmnet)
library(foreach)
library(lubridate)
library(skimr)
library(tidyverse)
library(ggplot2)
# tidyverse_conflicts()

# Winsorize
winsorize <- function (x, fraction=0.01) {
  # Source: https://www.r-bloggers.com/winsorization/
  #
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) { stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction), na.rm = T)
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}

## Organize Data ---------------------------------------------------------------
comp_a <- readRDS("0_data/wrds/raw_comp_a.rds") %>% as_tibble

db_a <- comp_a %>%
  select(gvkey, datadate, fyear, at, capx, sppe, K=ppent) %>%
  mutate(sppe = if_else(is.na(sppe), 0, sppe)) %>%
  mutate(I    = capx - sppe) %>%
  select(gvkey:at, K, I) %>%
  filter(K!=0) %>%
  na.omit %>% setDT
# summary(db_a %>% select(at:I))

## Compute Investment Growth as GHL proxy
db_a <- db_a[order(gvkey, fyear)]
db_a[, I := winsorize(I), by=fyear]
db_a[, K := winsorize(K), by=fyear]
db_a[, I_K := I / K]
# summary(db_a %>% select(at:I_K))
db_a[, I_K := winsorize(I_K), by=fyear]
db_a[, I_Klag  := shift(I_K, n = 1, type = "lag"), by=gvkey]
db_a[, IG      := (1+I_K)/(1+I_Klag)]
db_a[, IG      := log(IG)]
db_a[, IG      := winsorize(IG), by=fyear]
db_a <- db_a %>% na.omit
db_a[, IGt1    := shift(IG, n=1, type="lead"), by=gvkey]

ccm_a  <- readRDS("2_pipeline/1b_ccm_a.rds")

ccm_a  <- ccm_a %>% arrange(gvkey, fyear) %>%
  group_by(gvkey) %>%
  mutate(roe = ib/dplyr::lag(be)) %>% ungroup

library(data.table)

crsp_m <- readRDS(file = "2_pipeline/1a_cleaned_crsp_m.rds")
crsp_m0 <- readRDS("0_data/wrds/raw_crsp_m.rds")
crsp_m <- crsp_m %>%
  left_join(select(crsp_m0, permno, date, prc), by = c("permno", "date")) %>%
  setDT

## calcular o maior preco dos ultimos 12 meses
highValue <- function (x) return(sort(x, decreasing = T)[1])
# ATENCAO: CODIGO LENTO!!!
crsp_m[, highPrice := as.numeric(frollapply(prc, n = 12, FUN = highValue, fill = NA)), by = permno]

## calcular o menor preco dos ultimos 12 meses
lowValue  <- function (x) return(sort(x, decreasing = F)[1])
# ATENCAO: CODIGO LENTO!!!
crsp_m[, lowPrice  := as.numeric(frollapply(prc, n = 12, FUN = lowValue,  fill = NA)), by = permno]
# crsp_m[!is.na(lowPrice)]
# View(crsp_m[permno==28820])

## calcular PTH, PTL
crsp_m[!is.na(highPrice), PTH := prc / highPrice]
crsp_m[!is.na(lowPrice),  PTL := prc / lowPrice]
saveRDS(crsp_m, "2_pipeline/7b_crsp_m.rds")

ccm_a <- ccm_a %>%
  left_join(crsp_m %>% select(permno, monthlink, PTH, PTL),
            by=c("permno", "monthlink")) %>%
  select(gvkey, permno, monthlink, datadate, fyear,
         me, roe, PTH, PTL)
         # me, roe, q, Ret, SG, CFG, CF, cop, PG, EG, dROE, Ie, Id)
# ccm_a %>% na.omit %>% arrange(fyear) %>% as.data.table
# ccm_a %>% na.omit %>% group_by(fyear) %>% count %>% data.frame

db_a <- db_a %>%
  left_join(ccm_a, by=c("gvkey", "datadate", "fyear")) %>%
  # select(gvkey, permno, monthlink, datadate, fyear,
  #        # I, K, I_K, at,
  #        IGt1, me,
  #        IG, q:Id) %>%
  arrange(fyear) %>% na.omit

## Remove duplicated
db_a <- db_a %>%
  unique %>%
  # Some firms merge dROE twice, so remove duplicated rows
  # except the dROE column.
  # group_by_at(vars(-dROE)) %>%
  # filter(n() <= 1) %>% ungroup %>%
  as.data.table

db_a <- db_a %>% select(permno, gvkey:fyear, IG, IGt1, me:PTL)
# rm(ccm_a, comp_a, crsp_m)

db_a

## Previsao --------------------------------------------------------------------
sccm_a <- readRDS("2_pipeline/3a_sccm_a.rds")

db_a <- db_a %>% mutate(filing.year = fyear+1)

sccm_a <- sccm_a %>%
  select(filing.year:gvkey) %>%
  left_join(db_a, by = c("filing.year", "permno", "gvkey")) %>%
  select(filing.year:gvkey, d1_ia=IGt1, roe, PTH, PTL, me) %>% na.omit

sccm_a


## Load SCCM_A -----------------------------------------------------------------
# sccm_a <- readRDS("2_pipeline/3a_sccm_a.rds")
# setDT(sccm_a)
# sccm_a[, d1_ia := winsorize(d1_ia),  by=c("filing.year")]
# sccm_a[, me    := winsorize(me  ),   by=c("filing.year")]
# sccm_a[, q     := winsorize(q   ),   by=c("filing.year")]
# sccm_a[, cop   := winsorize(cop ),   by=c("filing.year")]
# sccm_a[, dROE  := winsorize(dROE),   by=c("filing.year")]
# sccm_a %>% as_tibble -> sccm_a

## Functions -------------------------------------------------------------------
getLocalTermsMatrix  <- function(filing_year, data_base) {

  if (length(filing_year)==1) {
    z_wrd <- readRDS(paste0("0_data/DTM/z_wrd", filing_year,".rds"))
    firms <- readRDS(paste0("0_data/DTM/corpus_firms", filing_year,".rds"))
    # Organize
    z_wrd[[3]] <- firms
    z_wrd[[1]]@Dimnames[[1]] <- firms
    z_wrd[[2]]@Dimnames[[1]] <- firms
    z_coll <- readRDS(paste0("0_data/DTM/z_coll_", filing_year,".rds"))
    # Merge z_wrd and z_coll Matrix
    DT1 <- as.data.frame( t(as.matrix( z_wrd[[2]])) )
    DT2 <- as.data.frame( t(as.matrix(z_coll[[2]])) )
    X <- data.table::rbindlist(list(DT1, DT2), fill = T, )
    X[is.na(X)] <- 0
    X <- as.data.frame(t(X))
    colnames(X) <- c(rownames(DT1), rownames(DT2))
    zz <-  as.matrix(X)

    firms <- as.double(unique(c(colnames(DT1), colnames(DT2))))
    firms <- tibble(cik = firms) %>%
      left_join(data_base %>% filter(filing.year==filing_year), by="cik")

    # Retirando NA
    nrow(firms) ; nrow(zz)

    # zz <- zz[!is.na(firms$gvkey),] # TODO: Atualizar essa parte em todas as funcoes
    zz[rownames(zz) %in% filter(firms, !is.na(gvkey))$cik,]

    firms <- firms %>% filter(!is.na(gvkey))
    nrow(firms) ; nrow(zz)
    outputSample <- list(firms=as.data.table(select(firms, filing.year, cik, permno, gvkey)),
                         y=firms$y,
                         # P=select(firms, q, cop, dROE), # HMXZ(2020) Model
                         # P = select(firms, q, Ret, CF),
                         P = select(firms, roe, PTH, PTL),
                         # P=select(firms, IGt0=d0_ia, Ret, cop, dROE, Id), # My model selection
                         X=Matrix::Matrix(zz, sparse = T),
                         me=firms$me)
    return(outputSample)
  } else {
    z_wrd <- readRDS(paste0("0_data/DTM/z_wrd", filing_year[1],".rds"))
    firms <- readRDS(paste0("0_data/DTM/corpus_firms", filing_year[1],".rds"))
    # Organize
    z_wrd[[3]] <- firms
    z_wrd[[1]]@Dimnames[[1]] <- firms
    z_wrd[[2]]@Dimnames[[1]] <- firms
    z_coll <- readRDS(paste0("0_data/DTM/z_coll_", filing_year[1],".rds"))
    # Merge z_wrd and z_coll Matrix
    DT1 <- as.data.frame( t(as.matrix( z_wrd[[2]])) )
    DT2 <- as.data.frame( t(as.matrix(z_coll[[2]])) )
    X <- data.table::rbindlist(list(DT1, DT2), fill = T, )
    X[is.na(X)] <- 0
    X <- as.data.frame(t(X))
    colnames(X) <- c(rownames(DT1), rownames(DT2))
    zz <-  as.matrix(X)

    firms <- as.double(unique(c(colnames(DT1), colnames(DT2))))
    firms <- tibble(cik = firms) %>%
      left_join(data_base %>% filter(filing.year==filing_year[1]), by="cik")

    # Retirando NA
    nrow(firms) ; nrow(zz)

    # zz <- zz[!is.na(firms$gvkey),] # TODO: Atualizar essa parte em todas as funcoes
    zz[rownames(zz) %in% filter(firms, !is.na(gvkey))$cik,]

    firms <- firms %>% filter(!is.na(gvkey))
    nrow(firms) ; nrow(zz)
    outputSample <- list(firms=as.data.table(select(firms, filing.year, cik, permno, gvkey)),
                         y=firms$y,
                         # P=select(firms, q, cop, dROE), # HMXZ(2020) Model
                         # P = select(firms, q, Ret, CF),
                         P = select(firms, roe, PTH, PTL),
                         # P=select(firms, IGt0=d0_ia, Ret, cop, dROE, Id), # My model selection
                         X=Matrix::Matrix(zz, sparse = T),
                         me=firms$me)
    for (i in 2:length(filing_year)) {

      z_wrd <- readRDS(paste0("0_data/DTM/z_wrd", filing_year[i],".rds"))
      firms <- readRDS(paste0("0_data/DTM/corpus_firms", filing_year[i],".rds"))
      # Organize
      z_wrd[[3]] <- firms
      z_wrd[[1]]@Dimnames[[1]] <- firms
      z_wrd[[2]]@Dimnames[[1]] <- firms
      z_coll <- readRDS(paste0("0_data/DTM/z_coll_", filing_year[i],".rds"))
      # Merge z_wrd and z_coll Matrix
      DT1 <- as.data.frame( t(as.matrix( z_wrd[[2]])) )
      DT2 <- as.data.frame( t(as.matrix(z_coll[[2]])) )
      X <- data.table::rbindlist(list(DT1, DT2), fill = T, )
      X[is.na(X)] <- 0
      X <- as.data.frame(t(X))
      colnames(X) <- c(rownames(DT1), rownames(DT2))
      zz <-  as.matrix(X)

      firms <- as.double(unique(c(colnames(DT1), colnames(DT2))))
      firms <- tibble(cik = firms) %>%
        left_join(data_base %>% filter(filing.year==filing_year[i]), by="cik")

      # Retirando NA
      nrow(firms) ; nrow(zz)

      # zz <- zz[!is.na(firms$gvkey),] # TODO: Atualizar essa parte em todas as funcoes
      zz[rownames(zz) %in% filter(firms, !is.na(gvkey))$cik,]

      firms <- firms %>% filter(!is.na(gvkey))
      nrow(firms) ; nrow(zz)
      outputSample2 <- list(firms=as.data.table(select(firms, filing.year, cik, permno, gvkey)),
                            y=firms$y,
                            # P=select(firms, q, cop, dROE), # HMXZ(2020) Model
                            # P = select(firms, q, Ret, CF),
                            P = select(firms, roe, PTH, PTL),
                            # P=select(firms, IGt0=d0_ia, Ret, cop, dROE, Id), # My model selection
                            X=Matrix::Matrix(zz, sparse = T),
                            me=firms$me)
      # https://stackoverflow.com/questions/19584039/rbindlist-data-tables-with-different-number-of-columns
      DT1 <- as.data.frame(as.matrix(outputSample$X))
      DT2 <- as.data.frame(as.matrix(outputSample2$X))
      X <- rbindlist(list(DT1, DT2), fill = T) ; X[is.na(X)] <- 0
      outputSample$X <- Matrix::Matrix(as.matrix(X), sparse = TRUE)

      outputSample$firms <- rbind(outputSample$firms, outputSample2$firms)
      outputSample$y <- c(outputSample$y, outputSample2$y)
      outputSample$P <- rbind(outputSample$P, outputSample2$P)
      outputSample$me <- c(outputSample$me, outputSample2$me)
    }
    return(outputSample)
  }
}

estimateByIndustryTerms <- function(YEAR, data_base) {
  require(foreach)


  gics <- readRDS("0_data/wrds/raw_gics.rds")
  setDT(gics) ;
  gics <- gics %>% na.omit %>% mutate(group = substr(gind, 1,4))
  gics <- gics %>% select(gvkey, gics=gind) %>% na.omit
  data_base %>% left_join(gics, by="gvkey") %>% na.omit -> data_base

  data_base %>% group_by(filing.year) %>% count %>% data.frame
  cat("\n")
  cat(paste0("Loading training sample between years ", (YEAR-5)," and ", (YEAR-1),".\n\n"))
  trainSample <- getLocalTermsMatrix((YEAR-5):(YEAR-1), data_base = data_base)
  summary(lm(trainSample$y ~ as.matrix(trainSample$P)))
  summary(lm(trainSample$y ~ as.matrix(trainSample$P), weights = trainSample$me))
  cat(paste0("Loading test sample for year ", YEAR,".\n\n"))
  testSample  <- getLocalTermsMatrix(YEAR, data_base = data_base)

  ## Alinhas as duas matrizes de palavras
  DT1 <- as.data.frame(as.matrix(trainSample$X))
  DT2 <- as.data.frame(as.matrix(testSample$X))
  Xtrain <- rbindlist(list(DT1, DT2[0,]), fill = T) ; Xtrain[is.na(Xtrain)] <- 0
  Xtest  <- rbindlist(list(DT1[0,], DT2), fill = T) ; Xtest[is.na(Xtest)]   <- 0

  trainSample$X <- Matrix::Matrix(as.matrix(Xtrain), sparse = TRUE)
  testSample$X  <- Matrix::Matrix(as.matrix(Xtest), sparse = TRUE)

  rm(DT1,DT2, Xtrain, Xtest)

  trainSample2     <- trainSample
  testSample2      <- testSample

  output <- list(data.table(filing.year=numeric(),
                            cik=numeric(), permno=numeric(), gvkey=character(),
                            industry=numeric(),
                            d1_ia=numeric(), EIGtext=numeric(),
                            alpha=numeric(), lambda=numeric(), SE=numeric()),
                 list())

  names(output) <- c("prediction", "words")

  industry_list <- unique(gics$gics)#[1:4]
  industry_list <- c(industry_list, 999999) # Others

  cat("Tuning model and predict in each industry... \n")
  pb <- txtProgressBar(min = 0, max = length(industry_list), style = 3)
  for (j in 1:length(industry_list)) {
    trainSample <- trainSample2
    testSample  <- testSample2

    a <- trainSample$firms %>% left_join(gics, by="gvkey") %>% data.table
    a[, industry := gics]
    other_industry <- a %>% group_by(gics) %>% count %>% filter(n<50)
    a[gics %in% other_industry$gics, industry := 999999]

    b <- testSample$firms %>% left_join(gics, by="gvkey") %>% data.table
    b[, industry := gics]
    b[gics %in% a[industry==999999]$gics, industry := 999999]

    # a %>% group_by(gics) %>% count %>% arrange(n) %>% data.table
    # b %>% group_by(gics) %>% count %>% arrange(n) %>% data.table
    # e em a tem que ter no minimo
    # tem que ter em b

    a[, keep := (industry == industry_list[j])]
    b[, keep := (industry == industry_list[j])]
    a[is.na(keep), keep := FALSE]
    b[is.na(keep), keep := FALSE]
    # a[keep==T,]
    # b[keep==T,]

    # j = 8
    if ( sum(a$industry==industry_list[j])!=0 && sum(b$keep == T)>1 ) {

      trainSample$firms <- trainSample$firms[a$keep,]
      trainSample$y     <- trainSample$y[a$keep]
      trainSample$me    <- trainSample$me[a$keep]
      trainSample$X     <- Matrix::Matrix(as.matrix(trainSample$X)[a$keep,])
      trainSample$P     <- trainSample$P[a$keep,]

      testSample$firms <- testSample$firms[b$keep,]
      testSample$y     <- testSample$y[b$keep]
      testSample$me    <- testSample$me[b$keep]
      testSample$X     <- Matrix::Matrix(as.matrix(testSample$X)[b$keep,])
      testSample$P     <- testSample$P[b$keep,]

      trainX <- cbind(as.matrix(trainSample$P), trainSample$X)
      pw=rep(0,ncol(trainSample$P))
      px=rep(1,ncol(trainSample$X))
      pf=c(pw,px)
      testX <- cbind(as.matrix(testSample$P), testSample$X)

      grids <- expand.grid(alpha = seq(from = 0.1, to = 0.9, by = 0.1),
                           lambda =  seq(from = 0.1, to = 0.9, by = 0.1))
      grids$MSE <- NA
      grids <- grids[order(grids$alpha),]
      grids

      foreach(i = 1:nrow(grids), .combine = c) %do% {
        fit <- glmnet(x = trainX, y = trainSample$y, family = "gaussian",
                      penalty.factor = pf,
                      weights = trainSample$me,
                      alpha = grids$alpha[i], lambda = grids$lambda[i])
        grids$MSE[i] <- mean( ( testSample$y - predict(fit, newx = testX, s = grids$lambda[i]) )^2 )
      }

      fit <- glmnet(x = trainX, y = trainSample$y, family = "gaussian",
                    penalty.factor = pf,
                    weights = trainSample$me,
                    alpha = grids[which.min(grids$MSE), "alpha"],
                    lambda = grids[which.min(grids$MSE), "lambda"])
      best_model <- data.table(variables = rownames(coef(fit, s = grids[which.min(grids$MSE), "lambda"])),
                               coefficients = as.vector(coef(fit, s = grids[which.min(grids$MSE), "lambda"])))
      eig_text <- as.vector(predict(fit, newx = testX, s = grids[which.min(grids$MSE),"lambda"]))

      output$prediction <- rbind(output$prediction,
                                 testSample$firms %>%
                                   mutate(industry = industry_list[j]) %>%
                                   mutate(d1_ia = testSample$y) %>%
                                   mutate(EIGtext = eig_text) %>%
                                   mutate(alpha = grids[which.min(grids$MSE),"alpha"]) %>%
                                   mutate(lambda = grids[which.min(grids$MSE),"lambda"]) %>%
                                   mutate(SE = (d1_ia - EIGtext)^2 ) )
      output$words[[j]] <- best_model[coefficients!=0]
    } else {
      output$words[[j]] <- NA
    }
    setTxtProgressBar(pb, j)
  }
  close(pb)
  names(output$words) <- paste0("ind", industry_list)
  return(output)
}

# highPredictiveWords <- function(list_eigText, group, n_wrd=15) {
#
#   bestWords <- lapply(list_eigText, "[[", 2)
#
#   groupWords <- lapply(bestWords, function(x, i) x[[match(group, names(x))]], i=group)
#
#   groupWords[is.na(groupWords)] <- NULL
#
#   groupWords <- bind_rows(groupWords)
#
#   dt1 <- groupWords %>%
#     filter( !(variables %in% c("(Intercept)", "Ret", "q", "CF")) ) %>%
#     group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>%
#     arrange(-coeff) %>% top_n(n_wrd) %>% mutate(coeff = round(coeff,3)) %>%
#     as.data.frame
#
#   dt2 <- groupWords %>%
#     filter( !(variables %in% c("(Intercept)", "Ret", "q", "CF")) ) %>%
#     group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>%
#     arrange(coeff) %>% top_n(-n_wrd) %>% mutate(coeff = round(coeff,3)) %>%
#     as.data.frame
#
#   output <- rbind(dt1, data.frame(words="---", coeff=NA), dt2)
#
#   return(output)
# }

winsorize <- function (x, fraction=0.01) {
  # Source: https://www.r-bloggers.com/winsorization/
  #
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) { stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}

## eigText - Year 1 ------------------------------------------------------------
sccm_a <- sccm_a %>% mutate(y = d1_ia)

eigText <- list()
# undebug(getLocalTermsMatrix)
# undebug(estimateByIndustryTerms)
eigText$y1999 <- estimateByIndustryTerms(1999, data_base=sccm_a)
eigText$y2000 <- estimateByIndustryTerms(2000, data_base=sccm_a)
eigText$y2001 <- estimateByIndustryTerms(2001, data_base=sccm_a)
eigText$y2002 <- estimateByIndustryTerms(2002, data_base=sccm_a)
eigText$y2003 <- estimateByIndustryTerms(2003, data_base=sccm_a)
eigText$y2004 <- estimateByIndustryTerms(2004, data_base=sccm_a)
eigText$y2005 <- estimateByIndustryTerms(2005, data_base=sccm_a)
eigText$y2006 <- estimateByIndustryTerms(2006, data_base=sccm_a)
eigText$y2007 <- estimateByIndustryTerms(2007, data_base=sccm_a)
eigText$y2008 <- estimateByIndustryTerms(2008, data_base=sccm_a)
eigText$y2009 <- estimateByIndustryTerms(2009, data_base=sccm_a)
eigText$y2009 <- estimateByIndustryTerms(2009, data_base=sccm_a)
eigText$y2010 <- estimateByIndustryTerms(2010, data_base=sccm_a)
eigText$y2011 <- estimateByIndustryTerms(2011, data_base=sccm_a)
eigText$y2012 <- estimateByIndustryTerms(2012, data_base=sccm_a)
eigText$y2012 <- estimateByIndustryTerms(2012, data_base=sccm_a)
eigText$y2012 <- estimateByIndustryTerms(2012, data_base=sccm_a)
eigText$y2012 <- estimateByIndustryTerms(2012, data_base=sccm_a)
eigText$y2013 <- estimateByIndustryTerms(2013, data_base=sccm_a)
eigText$y2014 <- estimateByIndustryTerms(2014, data_base=sccm_a)
eigText$y2015 <- estimateByIndustryTerms(2015, data_base=sccm_a)
eigText$y2016 <- estimateByIndustryTerms(2016, data_base=sccm_a)
eigText$y2017 <- estimateByIndustryTerms(2017, data_base=sccm_a)
eigText$y2018 <- estimateByIndustryTerms(2018, data_base=sccm_a)
eigText$y2019 <- estimateByIndustryTerms(2019, data_base=sccm_a)

saveRDS(eigText, "2_pipeline/5b_eigText_terms_Industry.rds")

## eigText - Year 2 ------------------------------------------------------------
sccm_a %>%
  arrange(cik, filing.year) %>% group_by(cik) %>%
  mutate(y = dplyr::lead(d1_ia, n = 1)) %>% ungroup %>%
  filter(complete.cases(y)) -> sccm_a

eigText2 <- list()
# debug(getLocalTermsMatrix)
# debug(estimateByIndustryTerms)
eigText2$y1999 <- estimateByIndustryTerms(1999, data_base=sccm_a)
eigText2$y2000 <- estimateByIndustryTerms(2000, data_base=sccm_a)
eigText2$y2001 <- estimateByIndustryTerms(2001, data_base=sccm_a)
eigText2$y2002 <- estimateByIndustryTerms(2002, data_base=sccm_a)
eigText2$y2003 <- estimateByIndustryTerms(2003, data_base=sccm_a)
eigText2$y2004 <- estimateByIndustryTerms(2004, data_base=sccm_a)
eigText2$y2005 <- estimateByIndustryTerms(2005, data_base=sccm_a)
eigText2$y2006 <- estimateByIndustryTerms(2006, data_base=sccm_a)
eigText2$y2007 <- estimateByIndustryTerms(2007, data_base=sccm_a)
eigText2$y2008 <- estimateByIndustryTerms(2008, data_base=sccm_a)
eigText2$y2009 <- estimateByIndustryTerms(2009, data_base=sccm_a)
eigText2$y2009 <- estimateByIndustryTerms(2009, data_base=sccm_a)
eigText2$y2010 <- estimateByIndustryTerms(2010, data_base=sccm_a)
eigText2$y2011 <- estimateByIndustryTerms(2011, data_base=sccm_a)
eigText2$y2012 <- estimateByIndustryTerms(2012, data_base=sccm_a)
eigText2$y2013 <- estimateByIndustryTerms(2013, data_base=sccm_a)
eigText2$y2014 <- estimateByIndustryTerms(2014, data_base=sccm_a)
eigText2$y2015 <- estimateByIndustryTerms(2015, data_base=sccm_a)
eigText2$y2016 <- estimateByIndustryTerms(2016, data_base=sccm_a)

eigText2$y2017 <- estimateByIndustryTerms(2016, data_base=sccm_a)
# # TODO estimateByIndustryTerms(2018) give-me "Warning message: In getLocalTermsMatrix(YEAR) : NAs introduced by coercion"eigText2$y2017 <- estimateByIndustryTerms(2017, data_base=sccm_a)
# eigText2$y2018 <- estimateByIndustryTerms(2018, data_base=sccm_a)

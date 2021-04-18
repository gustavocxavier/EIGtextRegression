## Load Packages ---------------------------------------------------------------
library(data.table)
library(glmnet)
library(foreach)
library(tidyverse)
select <- dplyr::select

## Functions -------------------------------------------------------------------

getLocalTermsMatrix <- function(filing_year, data_base) {
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
    zz <- zz[!is.na(firms$gvkey),]
    firms <- firms %>% filter(!is.na(gvkey))
    nrow(firms) ; nrow(zz)
    outputSample <- list(firms=as.data.table(select(firms, filing.year, cik, permno, gvkey)),
                         y=firms$y,
                         P=select(firms, q:dROE), # HMXZ(2020) Model
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
    zz <- zz[!is.na(firms$gvkey),]
    firms <- firms %>% filter(!is.na(gvkey))
    nrow(firms) ; nrow(zz)
    outputSample <- list(firms=as.data.table(select(firms, filing.year, cik, permno, gvkey)),
                         y=firms$y,
                         P=select(firms, q:dROE), # HMXZ(2020) Model
                         # P=select(firms, IGt0=d0_ia, Ret, cop, dROE, Id), # My model selection
                         X=Matrix::Matrix(zz, sparse = T),
                         me=firms$me)
    for (i in 2:length(filing_year)) {

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
      zz <- zz[!is.na(firms$gvkey),]
      firms <- firms %>% filter(!is.na(gvkey))
      nrow(firms) ; nrow(zz)
      outputSample2 <- list(firms=as.data.table(select(firms, filing.year, cik, permno, gvkey)),
                            y=firms$y,
                            P=select(firms, q:dROE), # HMXZ(2020) Model
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

estimateByLifeCycleTerms <- function(YEAR, data_base) {
  require(foreach)

  # gics <- readRDS("0_data/wrds/raw_gics.rds")
  lcycle <- readRDS("2_pipeline/2a_life_cycle_faff.rds")
  setDT(lcycle)
  lcycle <- lcycle %>%
    mutate(filing.year = fyear + 1) %>%
    select(gvkey, filing.year, LC=DCS) %>% filter(LC!=0)

  data_base %>% left_join(lcycle, by=c("gvkey", "filing.year")) %>% na.omit -> data_base

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
                            life_cycle=numeric(),
                            d1_ia=numeric(), EIGtext=numeric(),
                            alpha=numeric(), lambda=numeric(), SE=numeric()),
                 list())

  names(output) <- c("prediction", "words")

  life_cycle_list <- unique(lcycle$LC)#[1:4]
  # life_cycle_list <- c(life_cycle_list, 999999) # Others

  cat("Tuning model and predict in each life_cycle... \n")
  pb <- txtProgressBar(min = 0, max = length(life_cycle_list), style = 3)
  for (j in 1:length(life_cycle_list)) {
    trainSample <- trainSample2
    testSample  <- testSample2

    a <- trainSample$firms %>% left_join(lcycle, by=c("gvkey", "filing.year")) %>% data.table
    a[, life_cycle := LC]

    b <- testSample$firms %>% left_join(lcycle, by=c("gvkey", "filing.year")) %>% data.table
    b[, life_cycle := LC]

    a[, keep := (life_cycle == life_cycle_list[j])]
    b[, keep := (life_cycle == life_cycle_list[j])]
    a[is.na(keep), keep := FALSE]
    b[is.na(keep), keep := FALSE]


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
                                 mutate(life_cycle = life_cycle_list[j]) %>%
                                 mutate(d1_ia = testSample$y) %>%
                                 mutate(EIGtext = eig_text) %>%
                                 mutate(alpha = grids[which.min(grids$MSE),"alpha"]) %>%
                                 mutate(lambda = grids[which.min(grids$MSE),"lambda"]) %>%
                                 mutate(SE = (d1_ia - EIGtext)^2 ) )
    output$words[[j]] <- best_model[coefficients!=0]

    setTxtProgressBar(pb, j)
  }
  close(pb)
  names(output$words) <- paste0("LCycle", life_cycle_list)
  return(output)
}

## eigText - Year 1 ------------------------------------------------------------
eigText <- list()
# debug(estimateByLifeCycleTerms)
sccm_a <- readRDS("2_pipeline/3a_sccm_a.rds") %>% mutate(y = d1_ia)

eigText$y1999 <- estimateByLifeCycleTerms(1999, data_base = sccm_a)
eigText$y2000 <- estimateByLifeCycleTerms(2000, data_base = sccm_a)
eigText$y2001 <- estimateByLifeCycleTerms(2001, data_base = sccm_a)
eigText$y2002 <- estimateByLifeCycleTerms(2002, data_base = sccm_a)
eigText$y2003 <- estimateByLifeCycleTerms(2003, data_base = sccm_a)
eigText$y2004 <- estimateByLifeCycleTerms(2004, data_base = sccm_a)
eigText$y2005 <- estimateByLifeCycleTerms(2005, data_base = sccm_a)
eigText$y2006 <- estimateByLifeCycleTerms(2006, data_base = sccm_a)
eigText$y2007 <- estimateByLifeCycleTerms(2007, data_base = sccm_a)
eigText$y2008 <- estimateByLifeCycleTerms(2008, data_base = sccm_a)
eigText$y2009 <- estimateByLifeCycleTerms(2009, data_base = sccm_a)
eigText$y2009 <- estimateByLifeCycleTerms(2009, data_base = sccm_a)
eigText$y2010 <- estimateByLifeCycleTerms(2010, data_base = sccm_a)
eigText$y2011 <- estimateByLifeCycleTerms(2011, data_base = sccm_a)
eigText$y2012 <- estimateByLifeCycleTerms(2012, data_base = sccm_a)
eigText$y2012 <- estimateByLifeCycleTerms(2012, data_base = sccm_a)
eigText$y2012 <- estimateByLifeCycleTerms(2012, data_base = sccm_a)
eigText$y2012 <- estimateByLifeCycleTerms(2012, data_base = sccm_a)
eigText$y2013 <- estimateByLifeCycleTerms(2013, data_base = sccm_a)
eigText$y2014 <- estimateByLifeCycleTerms(2014, data_base = sccm_a)
eigText$y2015 <- estimateByLifeCycleTerms(2015, data_base = sccm_a)
eigText$y2016 <- estimateByLifeCycleTerms(2016, data_base = sccm_a)
eigText$y2017 <- estimateByLifeCycleTerms(2017, data_base = sccm_a)
eigText$y2018 <- estimateByLifeCycleTerms(2018, data_base = sccm_a)
eigText$y2019 <- estimateByLifeCycleTerms(2019, data_base = sccm_a)

saveRDS(eigText, "2_pipeline/5c_eigText_terms_LifeCycle.rds")

## eigText - Year 2 ------------------------------------------------------------

sccm_a %>% arrange(cik, filing.year) %>% group_by(cik) %>%
  mutate(y = dplyr::lead(d1_ia, n = 1)) -> sccm_a

eigTextLC2 <- list()
eigTextLC2$y1999 <- estimateByLifeCycleTerms(1999, data_base = sccm_a)
eigTextLC2$y2000 <- estimateByLifeCycleTerms(2000, data_base = sccm_a)
eigTextLC2$y2001 <- estimateByLifeCycleTerms(2001, data_base = sccm_a)
eigTextLC2$y2002 <- estimateByLifeCycleTerms(2002, data_base = sccm_a)
eigTextLC2$y2003 <- estimateByLifeCycleTerms(2003, data_base = sccm_a)
eigTextLC2$y2004 <- estimateByLifeCycleTerms(2004, data_base = sccm_a)
eigTextLC2$y2005 <- estimateByLifeCycleTerms(2005, data_base = sccm_a)
eigTextLC2$y2006 <- estimateByLifeCycleTerms(2006, data_base = sccm_a)
eigTextLC2$y2007 <- estimateByLifeCycleTerms(2007, data_base = sccm_a)
eigTextLC2$y2008 <- estimateByLifeCycleTerms(2008, data_base = sccm_a)
eigTextLC2$y2009 <- estimateByLifeCycleTerms(2009, data_base = sccm_a)
eigTextLC2$y2009 <- estimateByLifeCycleTerms(2009, data_base = sccm_a)
eigTextLC2$y2010 <- estimateByLifeCycleTerms(2010, data_base = sccm_a)
eigTextLC2$y2011 <- estimateByLifeCycleTerms(2011, data_base = sccm_a)
eigTextLC2$y2012 <- estimateByLifeCycleTerms(2012, data_base = sccm_a)
eigTextLC2$y2012 <- estimateByLifeCycleTerms(2012, data_base = sccm_a)
eigTextLC2$y2012 <- estimateByLifeCycleTerms(2012, data_base = sccm_a)
eigTextLC2$y2012 <- estimateByLifeCycleTerms(2012, data_base = sccm_a)
eigTextLC2$y2013 <- estimateByLifeCycleTerms(2013, data_base = sccm_a)
eigTextLC2$y2014 <- estimateByLifeCycleTerms(2014, data_base = sccm_a)
eigTextLC2$y2015 <- estimateByLifeCycleTerms(2015, data_base = sccm_a)
eigTextLC2$y2016 <- estimateByLifeCycleTerms(2016, data_base = sccm_a)
eigTextLC2$y2017 <- estimateByLifeCycleTerms(2017, data_base = sccm_a)
eigTextLC2$y2018 <- estimateByLifeCycleTerms(2018, data_base = sccm_a)
eigTextLC2$y2019 <- estimateByLifeCycleTerms(2019, data_base = sccm_a)

saveRDS(eigTextLC2, "2_pipeline/5c_eigText_terms_LifeCycle2.rds")

## eigText - Year 3 ------------------------------------------------------------
eigTextLC3 <- list()

sccm_a %>% arrange(cik, filing.year) %>% group_by(cik) %>%
  mutate(y = dplyr::lead(d1_ia, n = 2))

eigTextLC3$y1999 <- estimateByLifeCycleTerms(1999, data_base = sccm_a)
eigTextLC3$y2000 <- estimateByLifeCycleTerms(2000, data_base = sccm_a)
eigTextLC3$y2001 <- estimateByLifeCycleTerms(2001, data_base = sccm_a)
eigTextLC3$y2002 <- estimateByLifeCycleTerms(2002, data_base = sccm_a)
eigTextLC3$y2003 <- estimateByLifeCycleTerms(2003, data_base = sccm_a)
eigTextLC3$y2004 <- estimateByLifeCycleTerms(2004, data_base = sccm_a)
eigTextLC3$y2005 <- estimateByLifeCycleTerms(2005, data_base = sccm_a)
eigTextLC3$y2006 <- estimateByLifeCycleTerms(2006, data_base = sccm_a)
eigTextLC3$y2007 <- estimateByLifeCycleTerms(2007, data_base = sccm_a)
eigTextLC3$y2008 <- estimateByLifeCycleTerms(2008, data_base = sccm_a)
eigTextLC3$y2009 <- estimateByLifeCycleTerms(2009, data_base = sccm_a)
eigTextLC3$y2009 <- estimateByLifeCycleTerms(2009, data_base = sccm_a)
eigTextLC3$y2010 <- estimateByLifeCycleTerms(2010, data_base = sccm_a)
eigTextLC3$y2011 <- estimateByLifeCycleTerms(2011, data_base = sccm_a)
eigTextLC3$y2012 <- estimateByLifeCycleTerms(2012, data_base = sccm_a)
eigTextLC3$y2012 <- estimateByLifeCycleTerms(2012, data_base = sccm_a)
eigTextLC3$y2012 <- estimateByLifeCycleTerms(2012, data_base = sccm_a)
eigTextLC3$y2012 <- estimateByLifeCycleTerms(2012, data_base = sccm_a)
eigTextLC3$y2013 <- estimateByLifeCycleTerms(2013, data_base = sccm_a)
eigTextLC3$y2014 <- estimateByLifeCycleTerms(2014, data_base = sccm_a)
eigTextLC3$y2015 <- estimateByLifeCycleTerms(2015, data_base = sccm_a)
eigTextLC3$y2016 <- estimateByLifeCycleTerms(2016, data_base = sccm_a)
eigTextLC3$y2017 <- estimateByLifeCycleTerms(2017, data_base = sccm_a)
eigTextLC3$y2018 <- estimateByLifeCycleTerms(2018, data_base = sccm_a)
eigTextLC3$y2019 <- estimateByLifeCycleTerms(2019, data_base = sccm_a)

saveRDS(eigTextLC3, "2_pipeline/5c_eigText_terms_LifeCycle3.rds")

eigTextLC1 <- readRDS("2_pipeline/5c_eigText_terms_LifeCycle.rds")
eigTextLC2 <- readRDS("2_pipeline/5c_eigText_terms_LifeCycle2.rds")
eigTextLC3 <- readRDS("2_pipeline/5c_eigText_terms_LifeCycle3.rds")

### eigText - Year 1 - Evaluate ------------------------------------------------

eigText <- readRDS("2_pipeline/5c_eigText_terms_LifeCycle.rds")
eigTextAll <- bind_rows(lapply(eigText, "[[", 1))

# Set the same length by taking sub sample.
set.seed(1)
HMXZ <- readRDS("2_pipeline/4a_hmxz.rds")
HMXZ <- HMXZ[month(date)==6,]
HMmodel <- HMXZ[sample(nrow(HMXZ), size = nrow(eigTextAll)*0.5),]
M1model <- eigTextAll[sample(nrow(eigTextAll), size = nrow(eigTextAll)*0.5),]

# RMSE
SEtext <- (M1model$d1_ia - M1model$EIGtext)^2
SEhmxz  <- (HMmodel$d1_ia - HMmodel$EIG)^2
sqrt(sum( SEtext )) / sqrt(sum( SEhmxz ))
t.test( SEtext, SEhmxz)

# # RMSE
# sqrt(sum( (text$d1_ia.x - text$EIGtext)^2 )) / sqrt(sum( (text$d1_ia.y - text$EIGhmxz)^2 ))
# t.test( (text$d1_ia.x - text$EIGtext)^2, (text$d1_ia.y - text$EIGhmxz)^2)

hmxz <- HMXZ[month(date)==6,] %>%
  mutate(filing.year = year(fiscaldate)+1) %>%
  select(permno, gvkey, filing.year, d1_ia = d1_ia, EIGhmxz=EIG)

text <- eigTextAll %>% #[filing.year>=2000] %>%
  inner_join(hmxz, by = c("permno", "gvkey", "filing.year", "d1_ia"))

mean((text$d1_ia - text$EIGtext)^2)
mean((text$d1_ia - text$EIGhmxz)^2)

# RMSE
RMSE1 <- sqrt(sum( (text$d1_ia - text$EIGtext)^2 )) / sqrt(sum( (text$d1_ia - text$EIGhmxz)^2 ))
test1 <- t.test( (text$d1_ia - text$EIGtext)^2, (text$d1_ia - text$EIGhmxz)^2)
RMSE1;test1

### eigText - Year 2 - Evaluate ------------------------------------------------

# eigText2 <- readRDS("2_pipeline/5c_eigText_terms_LifeCycle2.rds")


# HMXZ  <- readRDS("~/Data/eig/hmxz2.rds")
HMXZ <- readRDS("2_pipeline/4a_hmxz.rds")
HMXZ  <- HMXZ[month(date)==6,]
eigText <- readRDS("2_pipeline/5c_eigText_terms_LifeCycle2.rds")
eigTextAll <- bind_rows(lapply(eigText, "[[", 1))

# Set the same length by taking sub sample.
set.seed(1)
HMmodel <- HMXZ[sample(nrow(HMXZ), size = nrow(eigTextAll)*0.5),]
M1model <- eigTextAll[sample(nrow(eigTextAll), size = nrow(eigTextAll)*0.5),]

# RMSE
SEtext <- (M1model$d1_ia - M1model$EIGtext)^2
SEhmxz  <- (HMmodel$d1_ia - HMmodel$EIG)^2
sqrt(sum( SEtext )) / sqrt(sum( SEhmxz ))
t.test( SEtext, SEhmxz)

hmxz <- HMXZ[month(date)==6,] %>%
  mutate(filing.year = year(fiscaldate)+1) %>%
  select(permno, gvkey, filing.year, d1_ia = d1_ia, EIGhmxz=EIG)

text <- eigTextAll %>% #[filing.year>=2000] %>%
  inner_join(hmxz, by = c("permno", "gvkey", "filing.year"))

mean((text$d1_ia.x - text$EIGtext)^2)
mean((text$d1_ia.y - text$EIGhmxz)^2)

# RMSE
RMSE2 <- sqrt(sum( (text$d1_ia.x - text$EIGtext)^2 )) / sqrt(sum( (text$d1_ia.y - text$EIGhmxz)^2 ))
test2 <- t.test( (text$d1_ia.x - text$EIGtext)^2, (text$d1_ia.y - text$EIGhmxz)^2)
RMSE2;test2

### eigText - Year 3 - Evaluate ------------------------------------------------

eigText3 <- readRDS("2_pipeline/5c_eigText_terms_LifeCycle3.rds")

# HMXZ  <- readRDS("~/Data/eig/hmxz3.rds")
HMXZ <- readRDS("2_pipeline/4a_hmxz.rds")
HMXZ  <- HMXZ[month(date)==6,]
eigText <- readRDS("2_pipeline/5b_eigText_terms_Industry3.rds")
eigTextAll <- bind_rows(lapply(eigText, "[[", 1))

# Set the same length by taking sub sample.
set.seed(1)
HMmodel <- HMXZ[sample(nrow(HMXZ), size = nrow(eigTextAll)*0.5),]
M1model <- eigTextAll[sample(nrow(eigTextAll), size = nrow(eigTextAll)*0.5),]

# RMSE
SEtext <- (M1model$d1_ia - M1model$EIGtext)^2
SEhmxz  <- (HMmodel$d1_ia - HMmodel$EIG)^2
sqrt(sum( SEtext )) / sqrt(sum( SEhmxz ))
t.test( SEtext, SEhmxz)


hmxz <- HMXZ[month(date)==6,] %>%
  mutate(filing.year = year(fiscaldate)+1) %>%
  select(permno, gvkey, filing.year, d1_ia = d1_ia, EIGhmxz=EIG)

text <- eigTextAll %>% #[filing.year>=2000] %>%
  inner_join(hmxz, by = c("permno", "gvkey", "filing.year"))

mean((text$d1_ia.x - text$EIGtext)^2)
mean((text$d1_ia.y - text$EIGhmxz)^2)

# RMSE
RMSE3 <- sqrt(sum( (text$d1_ia.x - text$EIGtext)^2 )) / sqrt(sum( (text$d1_ia.y - text$EIGhmxz)^2 ))
test3 <- t.test( (text$d1_ia.x - text$EIGtext)^2, (text$d1_ia.y - text$EIGhmxz)^2)
RMSE3;test3

## Output Evaluation -----------------------------------------------------------
evaluation <- list(RMSE = c(RMSE1, RMSE2, RMSE3),
                   "t-stat" = list(test1, test2, test3))
saveRDS(evaluation, "2_pipeline/5c_eigText_evaluation_LifeCycle.rds")

## MSE Analysis: Confront with HMXZ model --------------------------------------

eigTextAll <- eigTextLC1
eigTextAll <- bind_rows(lapply(eigText, "[[", 1))

# Set the same length by taking sub sample.
set.seed(1)
HMXZ <- HMXZ[month(date)==6,]
HMmodel <- HMXZ[sample(nrow(HMXZ), size = nrow(eigTextAll)*0.5),]
M1model <- eigTextAll[sample(nrow(eigTextAll), size = nrow(eigTextAll)*0.5),]

# RMSE
SEtext <- (M1model$d1_ia - M1model$EIGtext)^2
SEhmxz  <- (HMmodel$d1_ia - HMmodel$EIG)^2
sqrt(sum( SEtext )) / sqrt(sum( SEhmxz ))
t.test( SEtext, SEhmxz)

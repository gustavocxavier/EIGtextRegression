# TODO organize z_wrd
# TODO update getWords code to load rownames in advance

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
                         P=select(firms, q, cop, dROE), # HMXZ(2020) Model
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
                         P=select(firms, q, cop, dROE), # HMXZ(2020) Model
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
      zz <- zz[!is.na(firms$gvkey),]
      firms <- firms %>% filter(!is.na(gvkey))
      nrow(firms) ; nrow(zz)
      outputSample2 <- list(firms=as.data.table(select(firms, filing.year, cik, permno, gvkey)),
                            y=firms$y,
                            P=select(firms, q, cop, dROE), # HMXZ(2020) Model
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

highPredictiveWords <- function(list_eigText, group, n_wrd=15) {
  
  bestWords <- lapply(list_eigText, "[[", 2)
  
  groupWords <- lapply(bestWords, function(x, i) x[[match(group, names(x))]], i=group)
  
  groupWords[is.na(groupWords)] <- NULL
  
  groupWords <- bind_rows(groupWords)
  
  dt1 <- groupWords %>%
    filter( !(variables %in% c("(Intercept)", "IGt0","Ret", "q", "cop", "dROE", "Id")) ) %>% 
    group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>% 
    arrange(-coeff) %>% top_n(n_wrd) %>% mutate(coeff = round(coeff,3)) %>%
    as.data.frame
  
  dt2 <- groupWords %>%
    filter( !(variables %in% c("(Intercept)", "IGt0","Ret", "q", "cop", "dROE", "Id")) ) %>% 
    group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>% 
    arrange(coeff) %>% top_n(-n_wrd) %>% mutate(coeff = round(coeff,3)) %>%
    as.data.frame
  
  output <- rbind(dt1, data.frame(words="---", coeff=NA), dt2)
  
  return(output)
}

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

## Load SCCM_A -----------------------------------------------------------------
sccm_a <- readRDS("2_pipeline/2_out/3a_sccm_a.rds")
setDT(sccm_a)
sccm_a[, d1_ia := winsorize(d1_ia),  by=c("filing.year")]
sccm_a[, me    := winsorize(me  ),   by=c("filing.year")]
sccm_a[, q     := winsorize(q   ),   by=c("filing.year")]
sccm_a[, cop   := winsorize(cop ),   by=c("filing.year")]
sccm_a[, dROE  := winsorize(dROE),   by=c("filing.year")]
sccm_a %>% as_tibble -> sccm_a

## eigText - Year 1 ------------------------------------------------------------
sccm_a <- sccm_a %>% mutate(y = d1_ia)

eigText <- list()
# debug(getLocalTermsMatrix)
# debug(estimateByIndustryTerms)
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

saveRDS(eigText, "2_pipeline/2_out/5b_eigText_terms_Industry.rds")
eigText <- readRDS("2_pipeline/2_out/5b_eigText_terms_Industry.rds")

### eigText - Year 1 - Evaluate ------------------------------------------------

eigTextAll <- bind_rows(lapply(eigText, "[[", 1))

# Set the same length by taking sub sample.
set.seed(1)
HMXZ <- readRDS("2_pipeline/2_out/4a_hmxz.rds")
HMXZ <- HMXZ[month(date)==6,]
HMmodel <- HMXZ[sample(nrow(HMXZ), size = nrow(eigTextAll)*0.5),]
M1model <- eigTextAll[sample(nrow(eigTextAll), size = nrow(eigTextAll)*0.5),]

# RMSE
SEtext <- (M1model$d1_ia - M1model$EIGtext)^2
SEhmxz  <- (HMmodel$d1_ia - HMmodel$EIG)^2
sqrt(sum( SEtext )) / sqrt(sum( SEhmxz ))
t.test( SEtext, SEhmxz)

# hmxz <- HMXZ[month(date)==6,] %>%
#   mutate(filing.year = year(fiscaldate)+1) %>%
#   select(permno, gvkey, filing.year, d1_ia = d1_ia, EIGhmxz=EIG)
# 
# text <- eigTextAll %>% #[filing.year>=2000] %>%
#   inner_join(hmxz, by = c("permno", "gvkey", "filing.year", "d1_ia"))
# 
# mean((text$d1_ia - text$EIGtext)^2)
# mean((text$d1_ia - text$EIGhmxz)^2)
# 
# # RMSE
# sqrt(sum( (text$d1_ia - text$EIGtext)^2 )) / sqrt(sum( (text$d1_ia - text$EIGhmxz)^2 ))
# t.test( (text$d1_ia - text$EIGtext)^2, (text$d1_ia - text$EIGhmxz)^2)

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

saveRDS(eigText2, "2_pipeline/2_out/5b_eigText_terms_Industry2.rds")
eigText2 <- readRDS("2_pipeline/2_out/5b_eigText_terms_Industry2.rds")

### eigText - Year 2 - Evaluate ------------------------------------------------
# HMXZ  <- readRDS("~/Data/eig/hmxz2.rds")
HMXZ <- readRDS("2_pipeline/2_out/4a_hmxz.rds")
HMXZ  <- HMXZ[month(date)==6,]
eigText <- readRDS("2_pipeline/2_out/5b_eigText_terms_Industry2.rds")
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
sqrt(sum( (text$d1_ia.x - text$EIGtext)^2 )) / sqrt(sum( (text$d1_ia.y - text$EIGhmxz)^2 ))
t.test( (text$d1_ia.x - text$EIGtext)^2, (text$d1_ia.y - text$EIGhmxz)^2)

## eigText - Year 3 ------------------------------------------------------------

sccm_a %>%
  arrange(cik, filing.year) %>% group_by(cik) %>% 
  mutate(y = dplyr::lead(d1_ia, n = 2)) %>% ungroup %>%
  filter(complete.cases(y)) -> sccm_a

eigText3 <- list()
eigText3$y1999 <- estimateByIndustryTerms(1999, data_base = sccm_a)
eigText3$y2000 <- estimateByIndustryTerms(2000, data_base = sccm_a)
eigText3$y2001 <- estimateByIndustryTerms(2001, data_base = sccm_a)
eigText3$y2002 <- estimateByIndustryTerms(2002, data_base = sccm_a)
eigText3$y2003 <- estimateByIndustryTerms(2003, data_base = sccm_a)
eigText3$y2004 <- estimateByIndustryTerms(2004, data_base = sccm_a)
eigText3$y2005 <- estimateByIndustryTerms(2005, data_base = sccm_a)
eigText3$y2006 <- estimateByIndustryTerms(2006, data_base = sccm_a)
eigText3$y2007 <- estimateByIndustryTerms(2007, data_base = sccm_a)
eigText3$y2008 <- estimateByIndustryTerms(2008, data_base = sccm_a)
eigText3$y2009 <- estimateByIndustryTerms(2009, data_base = sccm_a)
eigText3$y2009 <- estimateByIndustryTerms(2009, data_base = sccm_a)
eigText3$y2010 <- estimateByIndustryTerms(2010, data_base = sccm_a)
eigText3$y2011 <- estimateByIndustryTerms(2011, data_base = sccm_a)
eigText3$y2012 <- estimateByIndustryTerms(2012, data_base = sccm_a)
eigText3$y2013 <- estimateByIndustryTerms(2013, data_base = sccm_a)
eigText3$y2014 <- estimateByIndustryTerms(2014, data_base = sccm_a)
eigText3$y2015 <- estimateByIndustryTerms(2015, data_base = sccm_a)
eigText3$y2016 <- estimateByIndustryTerms(2016, data_base = sccm_a)

saveRDS(eigText3, "2_pipeline/2_out/5b_eigText_terms_Industry3.rds")
eigText3 <- readRDS("2_pipeline/2_out/5b_eigText_terms_Industry3.rds")

### eigText - Year 3 - Evaluate ------------------------------------------------

# HMXZ  <- readRDS("~/Data/eig/hmxz3.rds")
HMXZ <- readRDS("2_pipeline/2_out/4a_hmxz.rds")
HMXZ  <- HMXZ[month(date)==6,]
eigText <- readRDS("2_pipeline/2_out/5b_eigText_terms_Industry3.rds")
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
sqrt(sum( (text$d1_ia.x - text$EIGtext)^2 )) / sqrt(sum( (text$d1_ia.y - text$EIGhmxz)^2 ))
t.test( (text$d1_ia.x - text$EIGtext)^2, (text$d1_ia.y - text$EIGhmxz)^2)


## High Predictive Words -------------------------------------------------------

N = 10
highPredictiveWords(eigText, 'ind253010', n_wrd = N) # Hotels, Restaurants & Leisure # <--- collocations!
highPredictiveWords(eigText, 'ind451010', n_wrd = N) # Internet Software & Services (discontinued effective close of September 30, 2018)
highPredictiveWords(eigText, 'ind352020', n_wrd = N) # Pharmaceuticals ## <---

highPredictiveWords(eigText, 'ind101010', n_wrd = N) # Energy Equipment & Services

highPredictiveWords(eigText, 'ind101020', n_wrd = N) # Oil, Gas & Consumable Fuels
highPredictiveWords(eigText, 'ind151010', n_wrd = N) # Chemicals
highPredictiveWords(eigText, 'ind151020', n_wrd = N) # Construction Materials
highPredictiveWords(eigText, 'ind151030', n_wrd = N) # Containers & Packaging
highPredictiveWords(eigText, 'ind151040', n_wrd = N) # Metals & Mining
highPredictiveWords(eigText, 'ind151050', n_wrd = N) # Paper & Forest Products
highPredictiveWords(eigText, 'ind201010', n_wrd = N) # Aerospace & Defense
highPredictiveWords(eigText, 'ind201020', n_wrd = N) # Building Products
highPredictiveWords(eigText, 'ind201030', n_wrd = N) # Construction & Engineering
highPredictiveWords(eigText, 'ind201040', n_wrd = N) # Electrical Equipment
highPredictiveWords(eigText, 'ind201050', n_wrd = N) # Industrial Conglomerates
highPredictiveWords(eigText, 'ind201060', n_wrd = N) # Machinery
highPredictiveWords(eigText, 'ind201070', n_wrd = N) # Trading Companies & Distributors
highPredictiveWords(eigText, 'ind202010', n_wrd = N) # Commercial Services & Supplies
highPredictiveWords(eigText, 'ind202020', n_wrd = N) # Professional Services
highPredictiveWords(eigText, 'ind203010', n_wrd = N) # Air Freight & Logistics
highPredictiveWords(eigText, 'ind203020', n_wrd = N) # Airlines
highPredictiveWords(eigText, 'ind203030', n_wrd = N) # Marine
highPredictiveWords(eigText, 'ind203040', n_wrd = N) # Road & Rail
highPredictiveWords(eigText, 'ind203050', n_wrd = N) # Transportation Infrastructure
highPredictiveWords(eigText, 'ind251010', n_wrd = N) # Auto Components
highPredictiveWords(eigText, 'ind251020', n_wrd = N) # Automobiles
highPredictiveWords(eigText, 'ind252010', n_wrd = N) # Household Durables ## < ---
highPredictiveWords(eigText, 'ind252020', n_wrd = N) # Leisure Products
highPredictiveWords(eigText, 'ind252030', n_wrd = N) # Textiles, Apparel & Luxury Goods
highPredictiveWords(eigText, 'ind253020', n_wrd = N) # Diversified Consumer Services
highPredictiveWords(eigText, 'ind254010', n_wrd = N) # Media (discontinued effective close of September 30, 2018)
highPredictiveWords(eigText, 'ind255010', n_wrd = N) # Distributors
highPredictiveWords(eigText, 'ind255020', n_wrd = N) # Internet & Direct Marketing Retail
highPredictiveWords(eigText, 'ind255030', n_wrd = N) # Multiline Retail
highPredictiveWords(eigText, 'ind255040', n_wrd = N) # Specialty Retail
highPredictiveWords(eigText, 'ind301010', n_wrd = N) # Food & Staples Retailing
highPredictiveWords(eigText, 'ind302010', n_wrd = N) # Beverages
highPredictiveWords(eigText, 'ind302020', n_wrd = N) # Food Products ## <---
highPredictiveWords(eigText, 'ind302030', n_wrd = N) # Tobacco
highPredictiveWords(eigText, 'ind303010', n_wrd = N) # Household Products
highPredictiveWords(eigText, 'ind303020', n_wrd = N) # Personal Products
highPredictiveWords(eigText, 'ind351020', n_wrd = N) # Health Care Providers & Services
highPredictiveWords(eigText, 'ind351030', n_wrd = N) # Health Care Technology
highPredictiveWords(eigText, 'ind352010', n_wrd = N) # Biotechnology ## <---
highPredictiveWords(eigText, 'ind352020', n_wrd = N) # Pharmaceuticals ## <---
highPredictiveWords(eigText, 'ind352030', n_wrd = N) # Life Sciences Tools & Services
highPredictiveWords(eigText, 'ind401010', n_wrd = N) # Banks
highPredictiveWords(eigText, 'ind401020', n_wrd = N) # Thrifts & Mortgage Finance
highPredictiveWords(eigText, 'ind402010', n_wrd = N) # Diversified Financial Services
highPredictiveWords(eigText, 'ind402020', n_wrd = N) # Consumer Finance
highPredictiveWords(eigText, 'ind402030', n_wrd = N) # Capital Markets
highPredictiveWords(eigText, 'ind402040', n_wrd = N) # Mortgage Real Estate Investment Trusts (REITs)
highPredictiveWords(eigText, 'ind403010', n_wrd = N) # Insurance

highPredictiveWords(eigText, 'ind451020', n_wrd = N) # IT Services
highPredictiveWords(eigText, 'ind451030', n_wrd = N) # Software ### <---
highPredictiveWords(eigText, 'ind452010', n_wrd = N) # Communications Equipment
highPredictiveWords(eigText, 'ind452020', n_wrd = N) # Technology Hardware, Storage & Peripherals
highPredictiveWords(eigText, 'ind452030', n_wrd = N) # Electronic Equipment, Instruments & Components
highPredictiveWords(eigText, 'ind453010', n_wrd = N) # Semiconductors & Semiconductor Equipment
highPredictiveWords(eigText, 'ind501010', n_wrd = N) # Diversified Telecommunication Services
highPredictiveWords(eigText, 'ind501020', n_wrd = N) # Wireless Telecommunication Services
highPredictiveWords(eigText, 'ind502010', n_wrd = N) # Media
highPredictiveWords(eigText, 'ind502020', n_wrd = N) # Entertainment
highPredictiveWords(eigText, 'ind502030', n_wrd = N) # Interactive Media & Services
highPredictiveWords(eigText, 'ind551010', n_wrd = N) # Electric Utilities
highPredictiveWords(eigText, 'ind551020', n_wrd = N) # Gas Utilities
highPredictiveWords(eigText, 'ind551030', n_wrd = N) # Multi-Utilities
highPredictiveWords(eigText, 'ind551040', n_wrd = N) # Water Utilities
highPredictiveWords(eigText, 'ind551050', n_wrd = N) # Independent Power and Renewable Electricity Producers
highPredictiveWords(eigText, 'ind601010', n_wrd = N) # Equity Real Estate Investment Trusts (REITs)"
highPredictiveWords(eigText, 'ind601020', n_wrd = N) # Real Estate Management & Development
highPredictiveWords(eigText, 'ind302020', n_wrd = N) # Food Products ## <---

bestWords <- lapply(eigText, "[[", 2)

d1 <- highPredictiveWords(eigText, 'ind351010', n_wrd = N) # Health Care Equipment & Supplies
d2 <- highPredictiveWords(eigText, 'ind252010', n_wrd = N) # Household Durables ## <---
d3 <- highPredictiveWords(eigText, 'ind151030', n_wrd = N) # Containers & Packaging
d4 <- highPredictiveWords(eigText, 'ind151040', n_wrd = N) # Metals & Mining

cbind(d1, d2, d3, d4)

xtable(cbind(d1, d2, d3, d4))

# 452010 - Communications Equipment
industryWords <- lapply(bestWords, function(x, i) x[[match("ind452010", names(x))]], i="ind452010")
# 452020 - Computers & Peripherals
industryWords <- lapply(bestWords, function(x, i) x[[match("ind452020", names(x))]], i="ind452020")
# 151040 - Metals and Mining
industryWords <- lapply(bestWords, function(x, i) x[[match("ind151040", names(x))]], i="ind151040")
# 352010 - Biotechnology
industryWords <- lapply(bestWords, function(x, i) x[[match("ind352010", names(x))]], i="ind352010")

industryWords <- lapply(bestWords, function(x, i) x[[match("ind999999", names(x))]], i="ind999999")

industryWords[is.na(industryWords)] <- NULL
industryWords <- bind_rows(industryWords)
industryWords %>%
  filter( !(variables %in% c("(Intercept)", "q", "cop", "dROE", "Id")) ) %>% 
  group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>% 
  arrange(-coeff) %>% top_n(10) %>% mutate(coeff = round(coeff,3)) %>%
  as.data.frame

industryWords %>%
  filter( !(variables %in% c("(Intercept)", "q", "cop", "dROE", "Id")) ) %>% 
  group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>% 
  arrange(coeff) %>% top_n(-10) %>% mutate(coeff = round(coeff,3)) %>%
  as.data.frame


# TODO: Build a simple shiny app to analyze each industry

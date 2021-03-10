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
                         # P=select(firms, q=0),
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
                         # P=select(firms, q=0),
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
                            # P=select(firms, q=0),
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

estimateByYear <- function(YEAR, data_base) {
  require(foreach)

  cat("\n")
  # cat(paste0("Loading training sample between years ", (YEAR-2)," and ", (YEAR-1),".\n\n"))
  # trainSample <- getLocalTermsMatrix((YEAR-2):(YEAR-1), data_base = data_base)
  cat(paste0("Loading training sample: year ", (YEAR-1),"\n\n"))
  trainSample <- getLocalTermsMatrix(YEAR-1, data_base = data_base)
  
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
  
  output <- list(data.table(filing.year=numeric(),
                            cik=numeric(), permno=numeric(), gvkey=character(),
                            dT_ia=numeric(), EIGtext=numeric(),
                            alpha=numeric(), lambda=numeric(), SE=numeric()),
                 list())
  
  names(output) <- c("prediction", "words")
  
  cat("Tuning model and predict... \n")

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
                               mutate(dT_ia = testSample$y) %>% 
                               mutate(EIGtext = eig_text) %>% 
                               mutate(alpha = grids[which.min(grids$MSE),"alpha"]) %>% 
                               mutate(lambda = grids[which.min(grids$MSE),"lambda"]) %>%
                               mutate(SE = (dT_ia - EIGtext)^2 ) )
  output$words <- best_model[coefficients!=0]
  
  return(output)
}

highPredictiveWords <- function(list_eigText, group=NULL) {
  
  bestWords <- lapply(list_eigText, "[[", 2)
  
  if (is.null(group)) {
    bestWords <- bind_rows(bestWords)
    
    dt1 <- bestWords %>%
      filter( !(variables %in% c("(Intercept)", "IGt0","Ret", "q", "cop", "dROE", "Id")) ) %>% 
      group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>% 
      arrange(-coeff) %>% top_n(15) %>% mutate(coeff = round(coeff,3)) %>%
      as.data.frame
    
    dt2 <- bestWords %>%
      filter( !(variables %in% c("(Intercept)", "IGt0","Ret", "q", "cop", "dROE", "Id")) ) %>% 
      group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>% 
      arrange(coeff) %>% top_n(-15) %>% mutate(coeff = round(coeff,3)) %>%
      as.data.frame 
  } else {
    groupWords <- lapply(bestWords, function(x, i) x[[match(group, names(x))]], i=group)
    
    groupWords[is.na(groupWords)] <- NULL
    
    groupWords <- bind_rows(groupWords)
    
    dt1 <- groupWords %>%
      filter( !(variables %in% c("(Intercept)", "IGt0","Ret", "q", "cop", "dROE", "Id")) ) %>% 
      group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>% 
      arrange(-coeff) %>% top_n(15) %>% mutate(coeff = round(coeff,3)) %>% filter(coeff>0) %>%
      as.data.frame
    
    dt2 <- groupWords %>%
      filter( !(variables %in% c("(Intercept)", "IGt0","Ret", "q", "cop", "dROE", "Id")) ) %>% 
      group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>% 
      arrange(coeff) %>% top_n(-15) %>% mutate(coeff = round(coeff,3)) %>% filter(coeff<0) %>%
      as.data.frame
  }
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
  # group_by(gvkey) %>% mutate(y = dplyr::lead(cop)) %>% ungroup %>% 
  #   filter(complete.cases(y))

eigText1 <- list()
eigText1$y1995 <- estimateByYear(1995, data_base = sccm_a)
eigText1$y1996 <- estimateByYear(1996, data_base = sccm_a)
eigText1$y1997 <- estimateByYear(1997, data_base = sccm_a)
eigText1$y1998 <- estimateByYear(1998, data_base = sccm_a)
eigText1$y1999 <- estimateByYear(1999, data_base = sccm_a)
eigText1$y2000 <- estimateByYear(2000, data_base = sccm_a)
eigText1$y2001 <- estimateByYear(2001, data_base = sccm_a)
eigText1$y2002 <- estimateByYear(2002, data_base = sccm_a)
eigText1$y2003 <- estimateByYear(2003, data_base = sccm_a)
eigText1$y2004 <- estimateByYear(2004, data_base = sccm_a)
eigText1$y2005 <- estimateByYear(2005, data_base = sccm_a)
eigText1$y2006 <- estimateByYear(2006, data_base = sccm_a)

eigText2 <- list()
eigText2$y2007 <- estimateByYear(2007, data_base = sccm_a)
eigText2$y2008 <- estimateByYear(2008, data_base = sccm_a)
eigText2$y2009 <- estimateByYear(2009, data_base = sccm_a)
eigText2$y2009 <- estimateByYear(2009, data_base = sccm_a)
eigText2$y2010 <- estimateByYear(2010, data_base = sccm_a)
eigText2$y2011 <- estimateByYear(2011, data_base = sccm_a)
eigText2$y2012 <- estimateByYear(2012, data_base = sccm_a)
eigText2$y2012 <- estimateByYear(2012, data_base = sccm_a)
eigText2$y2012 <- estimateByYear(2012, data_base = sccm_a)
eigText2$y2012 <- estimateByYear(2012, data_base = sccm_a)
eigText2$y2013 <- estimateByYear(2013, data_base = sccm_a)
eigText2$y2014 <- estimateByYear(2014, data_base = sccm_a)
eigText2$y2015 <- estimateByYear(2015, data_base = sccm_a)
eigText2$y2016 <- estimateByYear(2016, data_base = sccm_a)
eigText2$y2017 <- estimateByYear(2017, data_base = sccm_a)
eigText2$y2018 <- estimateByYear(2018, data_base = sccm_a)
eigText2$y2019 <- estimateByYear(2019, data_base = sccm_a)

eigText <- c(eigText1,eigText2)
cbind(highPredictiveWords(eigText1),
      highPredictiveWords(eigText2),
      highPredictiveWords(eigText))

# ## Report in LaTeX
# library(xtable)
# xtable(cbind(highPredictiveWords(eigText1),
#              highPredictiveWords(eigText2),
#              highPredictiveWords(eigText)),
#        caption = "High predictive words in different periods",
#        label = "Catp1TabHighWordsCS")

saveRDS(eigText, "2_pipeline/2_out/5a_eigText_terms_CS.rds")
eigText <-  readRDS("2_pipeline/2_out/5a_eigText_terms_CS.rds")

eigTextAll <- bind_rows(lapply(eigText, "[[", 1))

# Set the same length by taking sub sample.
set.seed(1)
HMXZ <- readRDS("2_pipeline/2_out/4a_hmxz.rds")
HMXZ <- HMXZ[month(date)==6,]
HMmodel <- HMXZ[sample(nrow(HMXZ), size = nrow(eigTextAll)*0.5),]
M1model <- eigTextAll[sample(nrow(eigTextAll), size = nrow(eigTextAll)*0.5),]

# RMSE
SEtext <- (M1model$dT_ia - M1model$EIGtext)^2
SEhmxz  <- (HMmodel$d1_ia - HMmodel$EIG)^2
sqrt(sum( SEtext )) / sqrt(sum( SEhmxz ))
t.test( SEtext, SEhmxz)

hmxz <- HMXZ[month(date)==6,] %>%
  mutate(filing.year = year(fiscaldate)+1) %>%
  select(permno, gvkey, filing.year, dT_ia = d1_ia, EIGhmxz=EIG)

text <- eigTextAll %>% #[filing.year>=2000] %>%
  inner_join(hmxz, by = c("permno", "gvkey", "filing.year", "dT_ia"))

mean((text$dT_ia - text$EIGtext)^2)
mean((text$dT_ia - text$EIGhmxz)^2)

# RMSE
sqrt(sum( (text$dT_ia - text$EIGtext)^2 )) / sqrt(sum( (text$dT_ia - text$EIGhmxz)^2 ))
t.test( (text$dT_ia - text$EIGtext)^2, (text$dT_ia - text$EIGhmxz)^2)


## eigText - Year 2 ------------------------------------------------------------
sccm_a %>%
  arrange(cik, filing.year) %>% group_by(cik) %>% 
  mutate(y = dplyr::lead(d1_ia, n = 1)) %>% ungroup %>%
  filter(complete.cases(y)) -> sccm_a

eigTextCS2 <- list()
eigTextCS2$y1999 <- estimateByYear(1999, data_base = sccm_a)
eigTextCS2$y2000 <- estimateByYear(2000, data_base = sccm_a)
eigTextCS2$y2001 <- estimateByYear(2001, data_base = sccm_a)
eigTextCS2$y2002 <- estimateByYear(2002, data_base = sccm_a)
eigTextCS2$y2003 <- estimateByYear(2003, data_base = sccm_a)
eigTextCS2$y2004 <- estimateByYear(2004, data_base = sccm_a)
eigTextCS2$y2005 <- estimateByYear(2005, data_base = sccm_a)
eigTextCS2$y2006 <- estimateByYear(2006, data_base = sccm_a)
eigTextCS2$y2007 <- estimateByYear(2007, data_base = sccm_a)
eigTextCS2$y2008 <- estimateByYear(2008, data_base = sccm_a)
eigTextCS2$y2009 <- estimateByYear(2009, data_base = sccm_a)
eigTextCS2$y2009 <- estimateByYear(2009, data_base = sccm_a)
eigTextCS2$y2010 <- estimateByYear(2010, data_base = sccm_a)
eigTextCS2$y2011 <- estimateByYear(2011, data_base = sccm_a)
eigTextCS2$y2012 <- estimateByYear(2012, data_base = sccm_a)
eigTextCS2$y2012 <- estimateByYear(2012, data_base = sccm_a)
eigTextCS2$y2012 <- estimateByYear(2012, data_base = sccm_a)
eigTextCS2$y2012 <- estimateByYear(2012, data_base = sccm_a)
eigTextCS2$y2013 <- estimateByYear(2013, data_base = sccm_a)
eigTextCS2$y2014 <- estimateByYear(2014, data_base = sccm_a)
eigTextCS2$y2015 <- estimateByYear(2015, data_base = sccm_a)
eigTextCS2$y2016 <- estimateByYear(2016, data_base = sccm_a)
eigTextCS2$y2017 <- estimateByYear(2017, data_base = sccm_a)
eigTextCS2$y2018 <- estimateByYear(2018, data_base = sccm_a)
eigTextCS2$y2019 <- estimateByYear(2019, data_base = sccm_a)

saveRDS(eigTextCS2, "2_pipeline/2_out/5a_eigText_terms_CS2.rds")
HMXZ <- readRDS("2_pipeline/2_out/4a_hmxz.rds")
# HMXZ  <- readRDS("~/Data/eig/hmxz2.rds")
HMXZ  <- HMXZ[month(date)==6,]
eigText <- readRDS("2_pipeline/2_out/5a_eigText_terms_CS2.rds")
eigTextAll <- bind_rows(lapply(eigText, "[[", 1))

# Set the same length by taking sub sample.
set.seed(1)
HMmodel <- HMXZ[sample(nrow(HMXZ), size = nrow(eigTextAll)*0.5),]
M1model <- eigTextAll[sample(nrow(eigTextAll), size = nrow(eigTextAll)*0.5),]

# RMSE
SEtext <- (M1model$dT_ia - M1model$EIGtext)^2
SEhmxz  <- (HMmodel$d1_ia - HMmodel$EIG)^2
sqrt(sum( SEtext )) / sqrt(sum( SEhmxz ))
t.test( SEtext, SEhmxz)

hmxz <- HMXZ[month(date)==6,] %>%
  mutate(filing.year = year(fiscaldate)+1) %>%
  select(permno, gvkey, filing.year, dT_ia = d1_ia, EIGhmxz=EIG)

text <- eigTextAll %>% #[filing.year>=2000] %>%
  inner_join(hmxz, by = c("permno", "gvkey", "filing.year"))

mean((text$dT_ia.x - text$EIGtext)^2)
mean((text$dT_ia.y - text$EIGhmxz)^2)

# RMSE
sqrt(sum( (text$dT_ia.x - text$EIGtext)^2 )) / sqrt(sum( (text$dT_ia.y - text$EIGhmxz)^2 ))
t.test( (text$dT_ia.x - text$EIGtext)^2, (text$dT_ia.y - text$EIGhmxz)^2)

## eigText - Year 3 ------------------------------------------------------------
sccm_a %>%
  arrange(cik, filing.year) %>% group_by(cik) %>% 
  mutate(y = dplyr::lead(d1_ia, n = 2)) %>% ungroup %>%
  filter(complete.cases(y)) -> sccm_a

eigTextCS3 <- list()
eigTextCS3$y1999 <- estimateByYear(1999, data_base = sccm_a)
eigTextCS3$y2000 <- estimateByYear(2000, data_base = sccm_a)
eigTextCS3$y2001 <- estimateByYear(2001, data_base = sccm_a)
eigTextCS3$y2002 <- estimateByYear(2002, data_base = sccm_a)
eigTextCS3$y2003 <- estimateByYear(2003, data_base = sccm_a)
eigTextCS3$y2004 <- estimateByYear(2004, data_base = sccm_a)
eigTextCS3$y2005 <- estimateByYear(2005, data_base = sccm_a)
eigTextCS3$y2006 <- estimateByYear(2006, data_base = sccm_a)
eigTextCS3$y2007 <- estimateByYear(2007, data_base = sccm_a)
eigTextCS3$y2008 <- estimateByYear(2008, data_base = sccm_a)
eigTextCS3$y2009 <- estimateByYear(2009, data_base = sccm_a)
eigTextCS3$y2009 <- estimateByYear(2009, data_base = sccm_a)
eigTextCS3$y2010 <- estimateByYear(2010, data_base = sccm_a)
eigTextCS3$y2011 <- estimateByYear(2011, data_base = sccm_a)
eigTextCS3$y2012 <- estimateByYear(2012, data_base = sccm_a)
eigTextCS3$y2012 <- estimateByYear(2012, data_base = sccm_a)
eigTextCS3$y2012 <- estimateByYear(2012, data_base = sccm_a)
eigTextCS3$y2012 <- estimateByYear(2012, data_base = sccm_a)
eigTextCS3$y2013 <- estimateByYear(2013, data_base = sccm_a)
eigTextCS3$y2014 <- estimateByYear(2014, data_base = sccm_a)
eigTextCS3$y2015 <- estimateByYear(2015, data_base = sccm_a)
eigTextCS3$y2016 <- estimateByYear(2016, data_base = sccm_a)

saveRDS(eigTextCS3, "2_pipeline/2_out/5a_eigText_terms_CS3.rds")
HMXZ <- readRDS("2_pipeline/2_out/4a_hmxz.rds")
# HMXZ  <- readRDS("~/Data/eig/hmxz3.rds")
HMXZ  <- HMXZ[month(date)==6,]
eigText <- readRDS("2_pipeline/2_out/5a_eigText_terms_CS3.rds")
eigTextAll <- bind_rows(lapply(eigText, "[[", 1))

# Set the same length by taking sub sample.
set.seed(1)
HMmodel <- HMXZ[sample(nrow(HMXZ), size = nrow(eigTextAll)*0.5),]
M1model <- eigTextAll[sample(nrow(eigTextAll), size = nrow(eigTextAll)*0.5),]

# RMSE
SEtext <- (M1model$dT_ia - M1model$EIGtext)^2
SEhmxz  <- (HMmodel$d1_ia - HMmodel$EIG)^2
sqrt(sum( SEtext )) / sqrt(sum( SEhmxz ))
t.test( SEtext, SEhmxz)


hmxz <- HMXZ[month(date)==6,] %>%
  mutate(filing.year = year(fiscaldate)+1) %>%
  select(permno, gvkey, filing.year, dT_ia = d1_ia, EIGhmxz=EIG)

text <- eigTextAll %>% #[filing.year>=2000] %>%
  inner_join(hmxz, by = c("permno", "gvkey", "filing.year"))

mean((text$dT_ia.x - text$EIGtext)^2)
mean((text$dT_ia.y - text$EIGhmxz)^2)

# RMSE
sqrt(sum( (text$dT_ia.x - text$EIGtext)^2 )) / sqrt(sum( (text$dT_ia.y - text$EIGhmxz)^2 ))
t.test( (text$dT_ia.x - text$EIGtext)^2, (text$dT_ia.y - text$EIGhmxz)^2)

# TODO: Put that results on 2_pipeline/2_out
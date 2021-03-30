## Generate Output Table

eval_CS  <- readRDS("2_pipeline/2_out/5a_eigText_evaluation_CS.rds")
eval_Ind <- readRDS("2_pipeline/2_out/5b_eigText_evaluation_Industry.rds")
eval_LC  <- readRDS("2_pipeline/2_out/5c_eigText_evaluation_LifeCycle.rds")

eval_ALL <- list(eval_CS, eval_Ind, eval_LC)

DF <- data.frame(row_names = c("CS", "", "Ind", "", "LC", ""),
                 "Year 1" = rep("",6),
                 "Year 2" = rep("",6),
                 "Year 3" = rep("",6))

for (i in 1:3) {
  for (j in 1:3) {
    RMSE <- round(eval_ALL[[i]]$RMSE[j],3)
    p_value <- round(eval_ALL[[i]]$`t-stat`[[j]]$p.value,3)
    t_stat  <- paste0("(",round(eval_ALL[[i]]$`t-stat`[[j]]$statistic,3),")")

    if (p_value <= 0.01)         { RMSE <- paste(RMSE, "***")
    } else if (p_value <= 0.05 ) { RMSE <- paste(RMSE, "**")
    } else if (p_value <= 0.10 ) { RMSE <- paste(RMSE, "*")
    } else { RMSE <- as.character(RMSE) }

    if (i == 1) { df_line <- 1:2
    } else if (i == 2) { df_line <- 3:4
    } else { df_line <- 5:6 }

    DF[df_line,j+1] <- c(RMSE, t_stat)
  }
}
DF

# library(data.table)
# library(tidyverse)
#
# ## Set functions ---------------------------------------------------------------
#
# highPredictiveWords <- function(list_eigText, group) {
#
#   bestWords <- lapply(eigText, "[[", 2)
#
#   groupWords <- lapply(bestWords, function(x, i) x[[match(group, names(x))]], i=group)
#
#   groupWords[is.na(groupWords)] <- NULL
#
#   groupWords <- bind_rows(groupWords)
#
#   dt1 <- groupWords %>%
#     filter( !(variables %in% c("(Intercept)", "IGt0","Ret", "q", "cop", "dROE", "Id")) ) %>%
#     group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>%
#     arrange(-coeff) %>% top_n(15) %>% mutate(coeff = round(coeff,3)) %>%
#     as.data.frame
#
#   dt2 <- groupWords %>%
#     filter( !(variables %in% c("(Intercept)", "IGt0","Ret", "q", "cop", "dROE", "Id")) ) %>%
#     group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>%
#     arrange(coeff) %>% top_n(-15) %>% mutate(coeff = round(coeff,3)) %>%
#     as.data.frame
#
#   output <- rbind(dt1, data.frame(words="---", coeff=NA), dt2)
#
#   return(output)
# }
#
#
# ## HMXZ as Benchmark -----------------------------------------------------------
# HMXZ   <- readRDS("~/Data/eig/hmxz.rds")
# # HMXZ94 <- HMXZ[date>"1994-01-01"]
# # HMXZ_a <- HMXZ94[month(date)==month(fiscaldate)]
# # HMXZ_jun <- HMXZ[month(date)==6]
# # mean((HMXZ$d1_ia   - HMXZ$EIG  )^2)     # MSE 0,165
# # mean((HMXZ94$d1_ia - HMXZ94$EIG)^2)     # MSE 0,203
# # mean((HMXZ_a$d1_ia - HMXZ_a$EIG)^2)     # MSE 0,198
# # mean((HMXZ_jun$d1_ia - HMXZ_jun$EIG)^2) # MSE 0,167
# HMXZ[,year := year(date)]
# HMXZ <- HMXZ[order(permno, year)]
# HMXZ[, SE := (d1_ia - EIG)^2]
#
# ## Text Measure ----------------------------------------------------------------
# # eigText <- readRDS("Data/Models/eigText_Indst_words.rds")
# # eigText <- readRDS("Data/Models/eigText_Indst_words_hmxz.rds")
# # eigText <- readRDS("Data/Models/eigText_terms.rds")
# # eigText <- readRDS("Data/Models/eigText_terms2.rds") # Include 2017 to 2019 # NOPZ
# eigText <- readRDS("Data/Models/eigText_terms_LifeCycle.rds")
# eigTextAll <- bind_rows(lapply(eigText, "[[", 1))
#
#
# ## MSE of each one -------------------------------------------------------------
# # eigTextAll %>% group_by(year = filing.year) %>% summarise(mean = mean(SE)) %>% data.frame
# # eigTextAll %>% group_by(year = filing.year) %>% summarise(median = median(SE)) %>% data.frame
# # eigTextAll %>% group_by(year = filing.year) %>% summarise(mean = mean(SE)) %>%
# #   mutate(MSEclassic_relative = mean < 0.168) %>% data.frame
# # eigTextAll %>% summarise(mean = mean(SE)) %>% data.frame
# mean(eigTextAll$SE)
# mean(HMXZ$SE)
# # HMXZ %>% group_by(year(date)) %>% summarise(MSE = mean(SE)) %>% data.frame
# mean(HMXZ[month(date)==6,]$SE)
# # mean(eigTextAll$SE) / mean(bmLASSO$SElasso)
#
#
# # ## Confront with MSE LASSO -----------------------------------------------------
# # bmLASSO <- bestModelLASSO %>%
# #   mutate(year = year+1) %>%
# #   select(permno, gvkey, filing.year = year, d1_ia = IGt1, EIG, SElasso = SE)
# #
# # set.seed(1)
# # BMmodel <- bmLASSO[sample(nrow(bmLASSO), size = nrow(eigTextAll)*0.5),]
# # M1model <- eigTextAll[sample(nrow(eigTextAll), size = nrow(eigTextAll)*0.5),]
# #
# # # RMSE
# # SEtext <- (M1model$d1_ia - M1model$EIGtext)^2
# # SElasso <- (BMmodel$d1_ia - BMmodel$EIG)^2
# # sqrt(sum( SEtext )) / sqrt(sum( SElasso ))
# # t.test( SEtext, SElasso)
#
# ## Confront with HMXZ model ----------------------------------------------------
#
# # Set the same length by taking sub sample.
# set.seed(1)
# HMXZ <- HMXZ[month(date)==6,]
# HMmodel <- HMXZ[sample(nrow(HMXZ), size = nrow(eigTextAll)*0.5),]
# M1model <- eigTextAll[sample(nrow(eigTextAll), size = nrow(eigTextAll)*0.5),]
#
# # RMSE
# SEtext <- (M1model$dT_ia - M1model$EIGtext)^2
# SEhmxz  <- (HMmodel$d1_ia - HMmodel$EIG)^2
# sqrt(sum( SEtext )) / sqrt(sum( SEhmxz ))
# t.test( SEtext, SEhmxz)
#
#
# # TODO Try the use of inner join to confront MSE
# #
# # hmxz <- HMXZ[month(date)==6,] %>%
# #   mutate(filing.year = year(fiscaldate)+1) %>%
# #   select(permno, gvkey, filing.year, d1_ia, EIGhmxz=EIG)
# #
# # text <- eigTextAll[filing.year>=2000] %>%
# #   inner_join(hmxz, by = c("permno", "gvkey", "filing.year", "d1_ia"))
# #
# # mean((text$d1_ia - text$EIGtext)^2)
# # mean((text$d1_ia - text$EIGhmxz)^2)
# #
# # # RMSE
# # sqrt(sum( (text$d1_ia - text$EIGtext)^2 )) / sqrt(sum( (text$d1_ia - text$EIGhmxz)^2 ))
# # t.test( (text$d1_ia - text$EIGtext)^2, (text$d1_ia - text$EIGhmxz)^2)
#
# ###
# #
# # text <- eigTextAll %>%
# #   inner_join(bmLASSO, by = c("permno", "gvkey", "filing.year", "d1_ia"))
# # # text <- text %>% group_by(permno) %>% mutate(EIGtext = dplyr::lead(EIGtext)) %>% na.omit
# #
# # # RMSE
# # sqrt(sum( (text$d1_ia - text$EIGtext)^2 )) / sqrt(sum( (text$d1_ia - text$EIGlasso)^2 ))
# # t.test( (text$d1_ia - text$EIGtext)^2, (text$d1_ia - text$EIGlasso)^2)
# #
# # sqrt(sum( text$SE )) / sqrt(sum( text$SElasso ))
#
#
# #
# # hmxz <- HMXZ[month(date)==6,] %>%
# #   mutate(filing.year = year(fiscaldate)+1) %>%
# #   select(permno, gvkey, filing.year, d1_ia, EIGhmxz=EIG)
# #
# # text <- eigTextAll[filing.year>=2004] %>%
# #   inner_join(hmxz, by = c("permno", "gvkey", "filing.year", "d1_ia"))
# #
# # text <- eigTextAll %>%
# #   left_join(hmxz, by = c("permno", "gvkey", "filing.year", "d1_ia"))
# # text <- text %>% filter(!is.na(EIGhmxz))
# # # text <- text %>% group_by(permno) %>% mutate(EIGtext = dplyr::lag(EIGtext)) %>% na.omit
# #
# # mean( (text$d1_ia - text$EIGhmxz)^2 )
# # mean( (text$d1_ia - text$EIGtext)^2 )
# #
# # # RMSE
# # sqrt(sum( (text$d1_ia - text$EIGtext)^2 )) / sqrt(sum( (text$d1_ia - text$EIGhmxz)^2 ))
# # t.test( (text$d1_ia - text$EIGtext)^2, (text$d1_ia - text$EIGhmxz)^2)
# #
#
#
# # ## Conpare MSE Text and Pure Elastic Net fromo eig09a_LC_ElasticNet
# # EIG_ENet_serie <- readRDS("~/Data/EIG/EIG_ENet_serie.rds")
# # mean(EIG_ENet_serie$SE)
#
#
# # sum(average_mse < 0.91) / length(average_mse)
# # mean(unlist(lapply(analise, function(x) x[3])), na.rm=T) # Media de preditores selec
# #
# #
# # ## --------------------------------------------------------------------
# # optimal_lambda <- mse[which.min(mse), lambda]
# #
# # # eig <- predict(fit, newx = testSample$X, s = as.numeric(mse[which.min(mse), 1]))
# #
# # ## Use the coef() function to access the
# # coef(fit) %>% dim
# # coef(fit)[, mse[which.min(mse), lambda]][which(as.matrix(coef(fit))!=0)]
# # # ## Higher the lambda, smaller will be the coeffients:
# # # # Very high
# # # fit$lambda[1]
# # # coef(fit)[, 1]
# # # # Very low
# # # fit$lambda[length(MSE)]
# # # coef(fit)[, length(MSE)]
# #
# # ## We can use predict() function for a number of purposes.
# # ## For instance, we can obtain the ridge coefficients for a new value of lambda
# # ## the arguement using type = "coefficients"
# # predict(fit, s = fit$lambda[1], type = "coefficients")
# # predict(fit, s = fit$lambda[99], type = "coefficients")
# # ## the argument "s = ..." specify the value of lambda
# #
# #
# #
# #
# #
# #
# # ## 2- optimal_alphas(escolha de alpha e lambda) -------------------------------
# #
# # # grid_lambdas=seq(by=0.1,to=0.9,from=0.1)
# # # grid_alphas=seq(by=0.05,to=0.95,from=0.05)
# # grid_alphas=seq(by=0.2, from=0.2, to=0.8)
# # grid_alphas
# #
# # set.seed(123)
# # Sys.setenv(LANG = "en")
# # t0 <- Sys.time() ## Registrando inicio da execucao
# # optimal_alphas_output=optimal_alphas(x = trainSample$X, y=trainSample$y,
# #                                      w = trainSample$P,
# #                                      grid_alphas=grid_alphas,
# #                                      cont_folds=FALSE, family="gaussian")
# # Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao
# # optimal_alphas_output
# #
# # optimal_alphas_output
# #
# # # > optimal_alphas_output
# # # [1] 0.050000 0.648892
# #
# # # 3 -Tv_dictionary ------------------------------------------------------------
# # # EStima elastic net, nessa um dos  outputs ser? os coeficientes
# #
# # set.seed(123)
# # t0 <- Sys.time() ## Registrando inicio da execucao
# # x_star=tv_dictionary(x=trainSample$X, y=trainSample$y, w=trainSample$P,
# #                      alpha=optimal_alphas_output[1],
# #                      lambda=optimal_alphas_output[2],
# #                      newx=testX,
# #                      family="gaussian")
# # Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao
# #
# #
# # dim ( as.matrix(trainSample$X) ) # Matriz X
# # dim ( as.matrix(x_star[[1]])   ) # Matriz trainSample$X reduzida
# # dim ( as.matrix(x_star[[2]])   ) # Coeficientes
# #
# # str(x_star[[2]])
# #
# # head(x_star[[1]]@Dimnames[[2]],50) # Palavras mais preditivas (maximo 50)
# # x_star[[2]]@x # Coeficientes (ja incluindo o  intercepto)
# # head(x_star[[2]],30) # Coeficientes + palavras (30 primeiros preditores)
# # Mx_star <- as.matrix(x_star[[2]])
# # round(Mx_star[which(Mx_star[,1]!=0),],3)
# #
# # dim(Mx_star)
# # dim(testSample$X)
# # X <- as.matrix(testSample$X)
# # testSample$X[1:12,1:2]
# # a <- apply(X, 1, function (x) Mx_star[1,] + Mx_star[2:1281,] * X  )
# #
# # x <- as.matrix(x_star[[2]])
# # coefs <- round(x[,1][round(x[,1],4)!=0],4)
# # data.frame(coefs)
# # length(coefs)
# #
# # coefs[2:length(coefs)]
# # z <- as.matrix(trainSample$X)
# # z <- z[,match(names(coefs[2:length(coefs)]), colnames(z))]
# # dim(z)
# # eigText <- coefs[1] + apply(z*coefs[2:length(coefs)], 1, sum)
# # cor(eigText, trainSample$y)
# # trainSample$firms
# #
# #
# #
#
# ## High Predictive Words -------------------------------------------------------
#
# result_high_words_LC <- cbind(highPredictiveWords(eigText, 'LCycle1'),
#                               highPredictiveWords(eigText, 'LCycle2'),
#                               highPredictiveWords(eigText, 'LCycle3'),
#                               highPredictiveWords(eigText, 'LCycle4'))
# xlsx::write.xlsx(result_high_words_LC, file = "~/TempFolder_EIG/result_high_words_LC.xlsx")
#
# ## Lab ------------------------------------------------------------------------
#
# z_wrd  <- readRDS(paste0("Data/DTM/z_wrd", 2002,".rds")) ; nrow(z_wrd[[2]])
# firms <- readRDS(paste0("Data/DTM/corpus_firms", 2002,".rds")) ; length(firms)
# # Organize
# z_wrd[[3]] <- firms
# z_wrd[[1]]@Dimnames[[1]] <- firms
# z_wrd[[2]]@Dimnames[[1]] <- firms
#
# z_coll <- readRDS(paste0("Data/DTM/z_coll_", 2002,".rds")) ; nrow(z_coll[[2]])
# # Check
# head(z_coll[[1]]@Dimnames[[1]])
# head(z_coll[[1]]@Dimnames[[2]])
# head(z_coll[[2]]@Dimnames[[1]])
# head(z_coll[[2]]@Dimnames[[2]])
# head(z_coll[[3]])
#
# # Merge z_wrd and z_coll Matrix
# DT1 <- as.data.frame( t(as.matrix( z_wrd[[2]])) )
# DT2 <- as.data.frame( t(as.matrix(z_coll[[2]])) )
# X <- data.table::rbindlist(list(DT1, DT2), fill = T, )
# X[is.na(X)] <- 0
# X <- as.data.frame(t(X))
# colnames(X) <- c(rownames(DT1), rownames(DT2))
# X <-  Matrix::Matrix(as.matrix(X), sparse = T)
#
# # Test if is correct
# nrow(DT1) + nrow(DT2) # Total rows pooled
# length(unique(c(colnames(DT1), colnames(DT2)))) # Total merged columns
# X@Dim
#
# library(data.table)
# # fill missing columns, and match by col names
# DT1 = data.table(A=1:3,B=letters[1:3])
# DT2 = data.table(B=letters[4:5],C=factor(1:2))
# l = list(DT1,DT2)
# rbindlist(l, use.names=TRUE, fill=TRUE)
# rbindlist(l, use.names=FALSE, fill=FALSE)
#


## Set parameters --------------------------------------------------------------
# setwd("D:/Data_SEC")
setwd("/home/gcx/Dropbox/[projetos]/docCode")

library(TextForecast)
library(udpipe)
library(tidyverse)
library(data.table)

dir.create("~/Data/udpipe")
# ud_model <- udpipe_download_model(language = "english",
#                                   model_dir="~/Data/udpipe")
# ud_model <- udpipe_load_model(ud_model$file_model)
ud_model <- udpipe_load_model(file = "~/Data/udpipe/english-ewt-ud-2.4-190531.udpipe")

identical (ud_model, ud_model2)

ano <- 1997

path_name_year <- paste0("~/Data/MD&A section text/y", ano)

# Lista de cik das empresas
empresas <- dir(path_name_year) ; empresas[1:4]
sccm_a <- readRDS("2_pipeline/2_out/3a_sccm_a.rds")
sccm_a %>%
  mutate(year.filed = fyear+1) %>% 
  mutate(cik = as.numeric(cik)) -> sccm_a
empresas <- empresas[empresas %in% (sccm_a %>% filter(year.filed==ano))$cik]


# Toy data
empresas <- empresas[1:10]

## Get words from MD&A ---------------------------------------------------------

# ## Get Words
# z_wrd = get_words2(corpus_dates = empresas, path_name = path_name_year,
#                    ntrms = 1000, st = 0)
# zz = z_wrd[[2]] # ; zz = as.matrix(z_wrd[[2]])


# ## Get Collocations
# z_coll=get_collocations2(corpus_dates=empresas, path_name=path_name_year,
#                         ntrms=10,ngrams_number=3,min_freq=10)
# zz_coll=z_coll[[2]]

## Get Terms
t0 <- Sys.time() ## Registrando inicio da execucao
z_terms=get_terms2(corpus_dates=empresas, path.name=path_name_year,ntrms_words=1000,
                   ngrams_number=2,st=0,ntrms_collocation=1000, min_freq=10)
Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao

# zz=z_terms[[2]] ## <-- Perguntar a Lucas
zz=z_terms[[2]][[2]]

## Teste
# str(z_terms[[1]]) # Words
# str(z_terms[[2]]) # Collocations
# z_terms[[1]][[1]]@Dimnames # Words
# z_terms[[2]][[1]]@Dimnames # Collocations
# z_terms[[1]][[1]]@Dim # Words
# z_terms[[2]][[1]]@Dim # Collocations (s? tem 9, entao uma nao pegou)
# m <- z_terms[[2]][[1]]
# m[1:10,1:4]
# rm(m)

saveRDS(z_terms, paste0("Forecast/terms", ano,".rds"))

z_terms <- readRDS(paste0("Forecast/terms", ano,".rds"))
zz=z_terms[[2]][[2]]

## Criar um vetor de variavel dependente --------------------------------------
df <- sccm_a %>%
  # Filtrar apenas empresas do ano analisado
  filter(year.filed==ano) %>%
  # Ficar apenas com o primeiro relatorio do ano por empresa(cik)
  group_by(cik, year.filed) %>%
  filter(row_number() == 1) %>% 
  # Selecionar apenas as colunas de interesse
  select(cik, d1_ia, fyear, year.filed)

rownames(df) <- df$cik
df[empresas,]

## 2- optimal_alphas(escolha de alpha e lambda) -------------------------------
grid_alphas=seq(by=0.05,to=0.95,from=0.05)
# grid_lambdas=seq(by=0.1,to=0.9,from=0.1)


set.seed(123)
optimal_alphas_output=optimal_alphas(x=zz, y=df[empresas,]$d1_ia,
                                     #grid_lambdas=grid_lambdas,
                                     grid_alphas=grid_alphas,
                                     cont_folds=FALSE, family="gaussian")
optimal_alphas_output

# 3 -Tv_dictionary ------------------------------------------------------------
# EStima elastic net, nessa um dos  outputs ser? os coeficientes

set.seed(123)
t0 <- Sys.time() ## Registrando inicio da execucao
x_star=tv_dictionary(x=zz, y=df[empresas,]$d1_ia,
                     alpha=optimal_alphas_output[1],
                     lambda=optimal_alphas_output[2],
                     newx=zz, family="gaussian")
Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao

str(x_star[[2]])

head(x_star[[1]]@Dimnames[[2]],50) # 50 palavras mais preditivas
x_star[[2]]@x # Coeficientes (ja incluindo o  intercepto)
head(x_star[[2]],30) # Coeficientes + palavras (30 primeiros preditores)

# summary(x_star)
# str(x_star[[2]])
# class(x_star)
# as.data.table(as(x_star[[2]], "matrix"))[order(s0)]
# class(x_star[[1]])





# ## Model Selection as WMY Machine Learning Cross-Section to Model Selection ----
# ## Estimate parameters by year and take the avarege coefficients ---------------
# 
# mgmt_disc
# d1994
# 
# setwd("D:\\Data_SEC")
# 
# mgmt_disc %>% filter(extract.status==1) %>% as_tibble
# 
# mgmt_disc[mgmt_disc$cik==1750,]
# 
# file_path <- paste0("MD&A section text/",
#                     mgmt_disc[145,]$cik,"_10-K_",
#                     mgmt_disc[145,]$date.filed,"_",
#                     mgmt_disc[145,]$accession.number,".txt")
# 
# filing.text <- readLines(file_path)
# cleanedDoc <- gsub("<.*?>", "", filing.text)
# cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
# cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
# cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
# cleanedDoc <- gsub("\\d+", "", cleanedDoc)
# cleanedDoc <- gsub("&#;", "", cleanedDoc)
# cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
# cleanedDoc <- cleanedDoc[cleanedDoc != ""]
# cleanedDoc <- cleanedDoc[cleanedDoc != " "]
# cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
# cleanedDoc <- cleanedDoc[cleanedDoc != ","]
# filing.text
# cleanedDoc
# 
# # load("Master Indexes/1994master.Rda")
# # db1994 <- as_tibble(year.master)
# # # db1994 <- filter(year.master, form.type == "10-K")
# # load("Master Indexes/1995master.Rda")
# # db1995 <- filter(year.master, form.type == "10-K")
# 
# comp_a %>% select(gvkey, year) %>% group_by(year) %>% count %>%
#   filter(year==1994)
# comp_a %>% filter(year==1995) %>% select(gvkey, datadate, at)
# scm
# paste0()
# 
# # t0 <- Sys.time() ## Registrando inicio da execucao
# # Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao
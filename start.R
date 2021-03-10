# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#                                                                              #
# Chapter 1 - Measuring Firms Investment Plans: A text-based analysis          #
#                                                                              #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#


## Working directory -----------------------------------------------------------

## Check if the current working directory is the project folder
getwd()

## If not, set the right working directory, which your project folder is located
# setwd("C:/") ## <--- Change here, make sure to avoid using single backslashes (i.e. \)

## Language --------------------------------------------------------------------
# Set language in English to resolve issues based on error messages
Sys.setenv(LANG = "en")

## Set  up pipeline folder if missing ------------------------------------------
## The code below will automatically create a pipeline folder for this code file
## if it does not exist.

if (!(dir.exists(file.path(getwd(), "2_pipeline")))) {
  dir.create(file.path(getwd(), "2_pipeline"))
}

pipeline <- file.path(getwd(), "2_pipeline")

for (folder in c('0_tmp', '1_store', '2_out')){
  if (!(dir.exists(file.path(pipeline, folder)))) {
    dir.create(file.path(pipeline, folder))
  }
}
rm(pipeline, folder)

## Imports ---------------------------------------------------------------------

library(RPostgres)
library(reticulate)
library(lubridate)
library(data.table)
library(tidyverse)

# ## Install packages by using groundhog to address reproducible research
# ## Se more in http://datacolada.org/95 or https://groundhogr.com/
# 
# ## All the library imports go here
# pkgs = c('lubridate','data.table', 'dplyr')
# 
# library(groundhog)
# groundhog_day = "2020-12-31" # Last date when script runs as expected
# groundhog.library(pkgs, groundhog_day)

# Set sample date range --------------------------------------------------------
begdate = '01/01/1966'
enddate = '12/31/2019'

## Functions -------------------------------------------------------------------
source('1_code/functions.R')

## Source next steps -----------------------------------------------------------
## If you prefer, you can source each script by this one


# ## Load Data From server (Run once)
# source("Data/getWRDSdata.R", echo = T)  

source("1_code/1a_organizeCRSPComp.R",          echo = T)
source("1_code/1b_organizeCCM.R",               echo = T)
source("1_code/2a_lifeCycleProxies.R",          echo = T)
source("1_code/3a_linkDataSEC_CCM.R",           echo = T)
source("1_code/4a_eigBenchMark.R",              echo = T)
source("1_code/5a_eigTextBased_terms_CS.R",     echo = T)
source("1_code/5b_eigTextBased_terms_Indst.R",  echo = T)
source("1_code/5c_eigTextBased_terms_LCycle.R", echo = T)
source("1_code/6a_eigTextBased_bestWords.R",    echo = T)
source("1_code/functions.R",                    echo = T)
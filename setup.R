# Super quick load (no install)
library(devtools) ; document() ; load_all()

# Quick install
#library(devtools) ; remove.packages(hyfer) ; document() ; install() ; library(hyfer)

# Full load and check
#library(devtools) ; document() ; load_all() ; check() ; install() ; library(hyfer)

# Create package environment

#library(devtools)
#setwd('../')
#getwd()
#create_package('/Users/erickeen/repos/hyfer')

# Import packages
#use_package('DBI')
#use_package('RPostgres')
#use_package('yaml')
#use_package('magrittr')
#use_package('dplyr')
#use_package('readr')
#use_package('stringr')
#use_package('lubridate')
#use_package('usethis')
#use_package('devtools')
#use_package('yaml')
#use_package('stringi')
#use_package('shiny')


# Create R Files
#use_r('get_cohort_data')

# Data
library(hyferdrive)
library(magrittr)
library(dplyr)
uids <- c('9D7SChvklVa7zya0LdU6YVOi9QV2', # navarra+12@hyfeapp.com # smoker
          '5Ue2PKP6KMUUbQcVIIjWu8rglIU2') # navarra+73@hyfeapp.com # covid
hyfe_data <- get_user_data(id=uids, id_type='uid', verbose=TRUE)
usethis::use_data(hyfe_data)


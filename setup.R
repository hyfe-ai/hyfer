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

library(hyfer)
library(hyferdrive)
library(magrittr)
library(dplyr)

# Example of processed data
device_details <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1QUXa5Uhh6oLrA6CyIc2rV2CVmGNrUL1Qczl9tbWvNLk/edit#gid=0')
names(device_details) <- gsub(" ","_",names(device_details))
device_details
consumers <- device_details$uid ; consumers
(researchers <- device_details$HyfeID[is.na(consumers)])
consumers <- consumers[!is.na(consumers)]
consumers

hd_researchers <- get_user_data(id=researchers,id_type='alias', verbose=TRUE)
researchers <- hd_researchers$id_key$uid %>% unique ; researchers
uids <- c(consumers, researchers)

hyfe_data <- get_user_data(id=uids,id_type='uid', verbose=TRUE)
hyfe_data %>% names
hyfe_data$id_key$uid %>% unique

# ho aggregate
ho <- process_hyfe_data(hyfe_data, verbose=TRUE)
names(ho)
usethis::use_data(ho, overwrite=TRUE)

# ho by user
ho_by_user <- process_hyfe_data(hyfe_data, by_user=TRUE, verbose=TRUE)
names(ho_by_user)
ho_by_user$user_summaries[[1]]$hours

usethis::use_data(ho_by_user, overwrite=TRUE)





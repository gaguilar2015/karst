##--------------------------------------------------------------------------------------------------
##'@name:   loader.R
##'@author: Gian Aguilar
##'
##'@description:  This script downloads the latest data from Survey Solutions and loads it into the 
##'               census database. This script should run every 5 minutes or so to keep the data on
##'               the Census Dashboard up to date. 
##'               
##'               Eventually this script will be expanded to do all
##'               data wrangling and tabulations and save them as database tables. So that the 
##'               Census Dashboard does minimal processing work and focuses on the interface.
##--------------------------------------------------------------------------------------------------

#Load all packages
library(loggit)         #' For general logging purposes
library(DBI)            #' DBI interface for DB drivers and connections
library(tidyverse)      #' Data manipulation library
library(httr)           #' http request library
library(request)        #' http request library
library(haven)          #' For reading external files of different formats, such as SPSS files
library(RMySQL)         #' MySQL driver
library(vroom)          #' For reading large files
library(janitor)        #' For data cleaning and tabulations
library(forcats)        #' For working with factor data
library(susoapi)
library(jsonlite)

#' Set location of log file
#' TODO: fix on server
#' TODO: ENCRYPT FILES
#' TODO: Flat files to DB tables
#set_logfile("/srv/R/logs/loggit.log")

###' FILE PATHS AND FILE NAMES ---------------------------
###' 

CURRENT_IP <- 'http://surveys.belizeanalytics.com'


CURRENT_ROOT <- './'
#CURRENT_ROOT <- '/srv/R/'

#' Survey Solutions zip file names
BRC_ZIP <- 'BRC.zip'

#' Survey Solutions folder names
BRC_FOLDER <- 'BRC_FILES'
#BRC_FOLDER <- 'BRCfiles2'

#' Base file names
BRC_BASE <- 'cash_assessment'

#' Child file names
BRC_UNITS <- 'listing'


connectDB <- function(){
  d2 <- dbConnect(
    RMySQL::MySQL(),
    dbname = "sibadmin_listing",
    username = "sibadmin_gian",
    password = "password",
    host = BRC_IP)
  
  d2
}

fetchWholeTable <- function(table){
  d2 <- connectDB()
  
  data <- dbReadTable(d2, table)
  
  dbDisconnect(d2)
  
  data
}

censusLogger <- function(user, action){
  tstamp <- lubridate::with_tz(Sys.time(), "America/Belize")
  username <- user
  actn <- action
  
  df <- data.frame(timestamp = tstamp, user = username, action = actn)
  
  d2 <- connectDB()
  
  dbWrite <- dbWriteTable(conn = d2, name = "BRC_logs", value = df, overwrite = T,
                          append = F,row.names = FALSE)
  dbDisconnect(d2)
  
}


#' --------------------------------
#' ------ Download and Load Data
#' --------------------------------

#' -------- Downloading and Loading functions --------

#' downloadData() fetches census data from Survey Solutions. It uses the
#'          Survey Solutions API to export and download the dataset in SPSS format. 
#'          Data from multiple versions of the questionnaire may be downloaded and extracted
#'          to different local directories.

#' readBaseFile() and readSavFile() are helper functions for importing the downloaded Census files.
#'           The Basefile is the main survey questionnaire file. Other auxiliary files are imported
#'           with readSavFile.


readBaseFile <- function(folder, filename){
  mainFilePath <- paste0(CURRENT_ROOT, folder, '/', filename, '.sav')
  diagnosticsFilePath <- paste0(CURRENT_ROOT, folder, '/interview__diagnostics.sav')
  actionsFilePath <- paste0(CURRENT_ROOT, folder, '/interview__actions.sav')
  
  diagnosticsFile <- read_sav(diagnosticsFilePath) %>% 
    mutate(
      duration_hourstoseconds = as.integer(stringr::str_sub(interview__duration, 4, 5)) * 3600,
      duration_minutestoseconds = as.integer(stringr::str_sub(interview__duration, 7, 8)) * 60,
      duration_secondstoseconds = as.integer(stringr::str_sub(interview__duration, 10, 11)),
      duration_seconds = duration_hourstoseconds + duration_minutestoseconds + duration_secondstoseconds
    ) %>% select(interview__key, responsible, interview__duration, duration_seconds)
  
  read_sav(mainFilePath) %>% 
    as_factor() %>% 
    select(-starts_with('unitList')) %>% 
    left_join(
      diagnosticsFile, by = c('interview__key')
    ) %>% 
    left_join(
      read_sav(actionsFilePath) %>% 
        as_factor() %>% 
        filter(action == "Completed") %>% 
        distinct(interview__key, .keep_all = TRUE) %>% 
        select(interview__key, date, originator, role),
      by = c('interview__key')
    )
}

readSavFile <- function(folder, filename){
  read_sav(paste0(CURRENT_ROOT, folder, '/', filename, '.sav')) %>% 
    as_factor()
}


#' getCensusData() reads the Census files into memory with the help of the above function. Data is 
#' read, merged, and pre-processed.
getCensusBRCData <- function(){
  
  #' BRC DATA -------------------------------------------------------------------
  #' 
  #' 
  
  #' Actions file contains information about when each interview was first completed,
  #' among other things
  households <- readBaseFile(BRC_FOLDER, BRC_BASE) %>% mutate(coder = '')
  
  
  #------ CORRECTION FOR ERRORS IN EDS 
  
  building_cover_vars <- households %>% 
    select(interview__key, LOCALITY, DISTRICT, HOUSEHOLD_NUM, interview__status) 
  
  
  #' Main units table
  individuals <- building_cover_vars %>% 
    full_join(readSavFile(BRC_FOLDER, BRC_UNITS),
              by = 'interview__key') 
  
  list(
    hhs = households,
    inds = individuals
  )
}


#' -------------- Download and Load --------------

ldata <- getCensusBRCData()


writeMainTables <- function(){
  print("Writing Main Tables...")
  #' Read the downloaded Census data into R objects
  # 
  #' -------- Upload MAIN files to database --------
  d2 <- connectDB()
  
  
  dbWrite <- dbWriteTable(conn = d2, name = "BRC_households", value = ldata$hhs, overwrite = T,
                          append = F, row.names = FALSE)
  
  dbWrite <- dbWriteTable(conn = d2, name = "BRC_inds", value = ldata$inds, overwrite = T,
                          append = F, row.names = FALSE)
  
  dbDisconnect(d2)
  print("Main Tables Written")
}


writeMainTables()

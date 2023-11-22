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
library(DBI)            #' DBI interface for DB drivers and connections
library(httr)           #' http request library
library(request)        #' http request library
library(RMySQL)


###' FILE PATHS AND FILE NAMES ---------------------------
###' 

#CURRENT_IP <- '190.197.22.215'
CURRENT_IP <- '192.168.0.3'

#CURRENT_SS <- 'http://census.sib.org.bz/'
CURRENT_SS <- 'http://surveys.belizeanalytics.com/'

#CURRENT_ROOT <- './'
CURRENT_ROOT <- '/srv/R/'

#' Survey Solutions zip file names
BRC_ZIP <- 'BRC.zip'

#' Survey Solutions folder names
BRC_FOLDER <- 'BRC_FILES'

#' Survey Solutions API constants
#' 
SERVER_URL <- CURRENT_SS
BRC_URL <- paste0(CURRENT_SS, 'redcross/api/v2/export/')

BRC_QID <- 'a87e5069a5c74b5eabc8690d631b350d$4'
FILE_TYPE <- 'SPSS'
API_USER <- 'gapi'
API_PWD <- 'Passw0rd10'
WS = 'redcross'


#--------------------------
ENVIRONMENT <- "SERVER"

#' --------------------------------
#' ------ Download and Load Data
#' --------------------------------

#' -------- Downloading and Loading functions --------

#' downloadData() fetches census data from Survey Solutions. It uses the
#'          Survey Solutions API to export and download the dataset in SPSS format. 
#'          Data from multiple versions of the questionnaire may be downloaded and extracted
#'          to different local directories.
downloadBRC <- function(){
  print("DL start")
  ## BRC1 ------------------
  
  bodyP <- list(
    ExportType = FILE_TYPE, 
    QuestionnaireId = BRC_QID, 
    InterviewStatus = 'All'
  )
  response = list(JobId = 0)
  #2A POST call to start export process and get resulting job ID of started process
  while(response$JobId == 0 | is.null(response)){
    Sys.sleep(5)
    response <- POST(
      BRC_URL,  
      body = bodyP, 
      encode = 'json', 
      authenticate(API_USER, API_PWD))
    response <- content(response)
    
    while (length(response) == 0){
      Sys.sleep(5)
      response <- POST(
        BRC_URL,  
        body = bodyP, 
        encode = 'json', 
        authenticate(API_USER, API_PWD))
      response <- content(response)
    }
    jobId <- response$JobId
  }
  print("DL Middle")
  #2B. Make consecutive GET requests to query the status of the export job, and when job is completed,
  #    download the exported files
  
  #URL to test status of export
  testUrl <- paste0(BRC_URL, jobId)
  #Perform the first query
  res <- GET(testUrl, authenticate(API_USER, API_PWD))
  res <- content(res)
  #Keep querying until export status is completed
  while (res$ExportStatus != "Completed") {
    Sys.sleep(5)
    res <- GET(testUrl, authenticate(API_USER, API_PWD))
    res <- content(res)
    print("Printing RES")
    print(res)
    
    while(length(res) == 0){
      Sys.sleep(5)
      res <- GET(testUrl, authenticate(API_USER, API_PWD))
      res <- content(res)
      print("Printing RES2")
      print(res)
    }
  }
  
  print("DL 3q")
  reqUrl <- paste0(BRC_URL, jobId, "/file")
  response <- GET(reqUrl, authenticate(API_USER, API_PWD))
  files <- content(response, "raw")
  
  #3. Open a file connection to unzip downloaded files and save into local directory
  filecon <- file(file.path(CURRENT_ROOT, BRC_ZIP), "wb")
  writeBin(files, filecon)
  close(filecon)
  
  if (ENVIRONMENT == "LOCAL"){
    unzip(paste0(CURRENT_ROOT, BRC_ZIP),
          exdir = paste0(CURRENT_ROOT, BRC_FOLDER))
  } else {
    # command <- paste0('7za x -o', CURRENT_ROOT, BRC_FOLDER,' ',
    #                   CURRENT_ROOT,  BRC_ZIP,' -p4aERzk%vIW%5 -y')
    # system(command)
    unzip(paste0(CURRENT_ROOT, BRC_ZIP),
          exdir = paste0(CURRENT_ROOT, BRC_FOLDER))
  }
  
  print("DL Finish")
  
}

downloadBRC() 


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(tidyr)
library(haven)
library(leaflet)
library(ggplot2)
library(DT)
library(reactable)
library(DBI)
library(RMySQL)
library(stringi)
library(stringr)
library(xlsx)
library(janitor)
library(DBI)            #' DBI interface for DB drivers and connections
library(httr)           #' http request library
library(request)        #' http request library
library(RMySQL)

# configure polished auth when the app initially starts up.



# LFS_IP <- '108.179.234.146'
# 
# connectDB <- function(){
#   d2 <- dbConnect(
#     RMySQL::MySQL(),
#     dbname = "sibadmin_listing",
#     username = "sibadmin_gian",
#     password = "password",
#     host = LFS_IP)
#   
#   d2
# }
# 
# fetchWholeTable <- function(table){
#   d2 <- connectDB()
#   
#   data <- dbReadTable(d2, table)
#   
#   dbDisconnect(d2)
#   
#   data
# }
# 
# base <- fetchWholeTable('BRC_households')
# individuals <- fetchWholeTable('BRC_inds') 
# 
# individuals$CBIRTH <- as.numeric(as.character(individuals$CBIRTH))
# base$COORD__Latitude <- as.numeric(as.character(base$COORD__Latitude))
# base$COORD__Longitude <- as.numeric(as.character(base$COORD__Longitude))
# individuals$AGE <- as.numeric(as.character(individuals$AGE))


###' FILE PATHS AND FILE NAMES ---------------------------
###' 

CURRENT_IP <- 'http://surveys.belizeanalytics.com'


CURRENT_ROOT <- './'
#CURRENT_ROOT <- '/srv/R/'

#' Survey Solutions zip file names
ZIPFILE <- 'KARST.zip'

#' Survey Solutions folder names
FOLDER <- 'KARST_FILES'
#BRC_FOLDER <- 'BRCfiles2'

#' Base file names
KRAST_BASE <- 'cash_assessment'

#' Child file names
KARST_INDS <- 'listing'

#' Survey Solutions API constants
#' 
SERVER_URL <- 'http://surveys.belizeanalytics.com/'
EXPORT_URL <- paste0(SERVER_URL, 'bkhc/api/v2/export/')

QID <- '370637e863294a40bff58721962d2da6$11'
FILE_TYPE <- 'SPSS'
API_USER <- 'gapi'
API_PWD <- 'Passw0rd10'
WS = 'bkhc'

ENVIRONMENT = 'LOCAL'


downloadSS <- function(){
  print("DL start")
  ## BRC1 ------------------
  
  bodyP <- list(
    ExportType = FILE_TYPE, 
    QuestionnaireId = QID, 
    InterviewStatus = 'All'
  )
  response = list(JobId = 0)
  #2A POST call to start export process and get resulting job ID of started process
  while(response$JobId == 0 | is.null(response)){
    response <- POST(
      EXPORT_URL,  
      body = bodyP, 
      encode = 'json', 
      authenticate(API_USER, API_PWD))
    response <- content(response)
    
    while (length(response) == 0){
      response <- POST(
        EXPORT_URL,  
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
  testUrl <- paste0(EXPORT_URL, jobId)
  #Perform the first query
  res <- GET(testUrl, authenticate(API_USER, API_PWD))
  res <- content(res)
  #Keep querying until export status is completed
  while (res$ExportStatus != "Completed") {

    res <- GET(testUrl, authenticate(API_USER, API_PWD))
    res <- content(res)
    print("Printing RES")
    print(res)
    
    while(length(res) == 0){

      res <- GET(testUrl, authenticate(API_USER, API_PWD))
      res <- content(res)
      print("Printing RES2")
      print(res)
    }
  }
  
  print("DL 3q")
  reqUrl <- paste0(EXPORT_URL, jobId, "/file")
  response <- GET(reqUrl, authenticate(API_USER, API_PWD))
  files <- content(response, "raw")
  
  #3. Open a file connection to unzip downloaded files and save into local directory
  filecon <- file(file.path(CURRENT_ROOT, ZIPFILE), "wb")
  writeBin(files, filecon)
  close(filecon)
  
  if (ENVIRONMENT == "LOCAL"){
    unzip(paste0(CURRENT_ROOT, ZIPFILE),
          exdir = paste0(CURRENT_ROOT, FOLDER))
  } else {
    # command <- paste0('7za x -o', CURRENT_ROOT, BRC_FOLDER,' ',
    #                   CURRENT_ROOT,  BRC_ZIP,' -p4aERzk%vIW%5 -y')
    # system(command)
    unzip(paste0(CURRENT_ROOT, ZIPFILE),
          exdir = paste0(CURRENT_ROOT, FOLDER))
  }
  
  print("DL Finish")
  
}

#downloadSS() 

base <- read_sav('./KARST_FILES/tmnr.sav') |> as_factor()
diag <- read_sav('./KARST_FILES/interview__diagnostics.sav') |> as_factor()
base <- base |> 
  mutate(
    building_number = as.numeric(as.character(building_number)),
    dwelling_number = as.numeric(as.character(dwelling_number)),
    household_number = as.numeric(as.character(household_number))
  ) |> 
  left_join(
    diag |> select(interview__key, responsible),
    by = 'interview__key'
  ) |> 
  mutate(
    Interviewer = case_when(
      responsible == 'interviewer1' ~ 'Aracelly Teck',
      responsible == 'interviewer2' ~ 'Celine Ortega',
      responsible == 'interviewer3' ~ 'Juan Bol',
      responsible == 'interviewer4' ~ 'Leah Pandy',
      responsible == 'interviewer5' ~ 'Angelina Flores'
    )
  )

total_interviews <- base |> nrow()
completed_households <- base |> filter(result == 'Completed') |> nrow()
pcompleted_households <- base |> filter(result == 'Partilaly Completed') |> nrow()
response_rate <- ((completed_households + pcompleted_households)/total_interviews) |> round(2)

communities_started <- base |> dplyr::distinct(ctv) |> nrow()

ksample <- readxl::read_excel('./ksample2.xlsx') |> 
  select(interview_key, 
         ed, 
         head_name,
         ctv_official, 
         building_number, 
         dwelling_number, 
         household_number,
         GPS_Building_Navig__Latitude,
         GPS_Building_Navig__Longitude
         ) |> 
  mutate(
    GPS_Building_Navig__Latitude = as.double(GPS_Building_Navig__Latitude),
    GPS_Building_Navig__Longitude = as.double(GPS_Building_Navig__Longitude)
  )

ksample_join <- ksample |> 
  left_join(
    base,
    by = c('ed', 
           'building_number', 
           'dwelling_number', 
           'household_number')
  )

communities_started_names <- ksample_join |> 
  filter(!is.na(interview__key)) |> 
  dplyr::distinct(ctv_official) |> 
  pull(ctv_official)

communities_names <- ksample_join |> 
  dplyr::distinct(ctv_official) |> 
  pull(ctv_official)

pending_by_community <- ksample_join |> 
  group_by(ctv_official) |>
  summarise(
    interviews_pending = sum(is.na(ctv))
  )
  
communities_completed <- pending_by_community |> 
  filter(interviews_pending == 0) |> 
  nrow()



gg_text_charts <- theme(
  text = element_text(size = 15)
)

gg_blank_x <- theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank()
  )

gg_blank_y <- theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank()
  )

# base
# reactable(base)

bmap_choices <- c('Esri.WorldImagery', 'OpenStreetMap.Mapnik')


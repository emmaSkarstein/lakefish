#-------------------------------------------------------------------------------
# Load packages
#-------------------------------------------------------------------------------

library(rgbif)        # for interacting with gbif
library(dplyr)        # for data-wrangling
library(rstudioapi)   # for writing in user info
library(rio)          # a swiss army knife for data I/O
library(here)         # for knowing where we are


#' Download GBIF API
#' 
#' Download occurrence data through GBIF API.
#'
#' @param download_key 
#' @param destfile_name 
#' @param n_try 
#' @param Sys.sleep_duration 
#'
#' @return Nothing, but saves data to temporary directory.
#'
#' @examples
download_GBIF_API <- function(download_key,destfile_name,n_try,Sys.sleep_duration){
  # Coffebreak version (see https://github.com/GBIF-Europe/nordic_oikos_2018_r/blob/master/s3_gbif_demo/Download_gbif.md)
  
  cat("Attempting to download data from GBIF with", n_try,"attempts at", Sys.sleep_duration, "second intervals.\n")
  start_time <- Sys.time()
  n_try_count <- 1
  
  download_url <- paste("http://api.gbif.org/v1/occurrence/download/request/",
                        download_key[1],sep="")
  
  try_download <- try(download.file(url=download_url,destfile=destfile_name,
                                    quiet=TRUE, mode="wb"),silent = TRUE)
  
  while (inherits(try_download, "try-error") & n_try_count < n_try) {   
    Sys.sleep(Sys.sleep_duration)
    n_try_count <- n_try_count+1
    try_download <- try(download.file(url=download_url,destfile=destfile_name,
                                      quiet=TRUE, mode="wb"),silent = TRUE)
    cat("trying... Download link not ready. Time elapsed (min):",
        round(as.numeric(paste(difftime(Sys.time(),start_time, 
                                        units = "mins"))),2), "\n")
  }
}

#' Download lakefish
#'
#' @param latin_name a character string, lating name of wanted species .
#' @param n_try numeric, number of download tries, default 10.
#'
#' @return Nothing, but saves occurrence data and key to "data" folder in current working directory.
#' @export
#'
#' @examples
download_lakefish <- function(latin_name, n_try = 10){
  #-------------------------------------------------------------------------------
  # Register credentials 
  #-------------------------------------------------------------------------------
  cat("Register credentials\n")
  
  #options(gbif_user = rstudioapi::askForPassword("my gbif username"))
  #options(gbif_email = rstudioapi::askForPassword("my registred gbif e-mail"))
  #options(gbif_pwd = rstudioapi::askForPassword("my gbif password"))
  
  
  #-------------------------------------------------------------------------------
  # Set search parameters and get download KEY
  #-------------------------------------------------------------------------------
  cat("Setting search parameters and getting download key...\n")
  
  # Salmo trutta - Orret
  # Esox lucius - Gjedde
  # Salmo salar - Laks
  # Salvelinus alpinus - Roye
  # Rutilus rutilus - Mort
  # Perca fluviatilis - Abbor
  # Cyprinus carpio - Karpe
  # Actinopterygii - Ray-finned fish
  
  # Find a taxonkey - get list of gbif keys to filter download
  key <- name_suggest(q = toString(latin_name), rank='species')$key[1] 
  
  
  
  #-------------------------------------------------------------------------
  # Send download request
  #-------------------------------------------------------------------------
  cat("Sending download request...\n")
  
  # Spawn download request - careful, there is a max limit on simultanious downloads 
  # per user allowed by the GBIF API. 
  download_key <- occ_download(
    paste('taxonKey = ', key),
    'hasCoordinate = TRUE',
    'country = NO',
    #geom_param,
    type = "and"
  ) %>% 
    occ_download_meta
  
  # For overview of your downloads, see https://www.gbif.org/user/download . 
  # The preparation will of the data may take some time.
  temp <- tempdir()
  
  download_GBIF_API(download_key = download_key, 
                    destfile_name = paste0(temp,"/tmp.zip"),
                    n_try = n_try, Sys.sleep_duration = 30)
  cat("------IGNORE THE WARNING MESSAGES------")
  
  
  #--------------------------------------------------------------------------
  # Read in the occurrence data
  #--------------------------------------------------------------------------
  cat("Reading in the occurrence data...\n")
  
  occ <- rio::import(unzip(paste0(temp,"/tmp.zip"), files = "occurrence.txt"))
  
  # Create "data"-folder if this doesn't exist
  if (!dir.exists(here::here("data"))){ 
    dir.create(here::here("data"))
  }
  
  # Save the occurrence data to a file
  saveRDS(occ, here::here("data", paste0("GBIF_download_", key, ".rds")))
  
  unlink(temp)
  unlink(here::here("GBIF_download", "occurrence.txt"))
  
  
  # Write download key and citation to .rds files for reference (also available on https://www.gbif.org/user/download)
  saveRDS(download_key,here::here("data", paste0("GBIF_download_key_", key, ".rds")))
  
  # CITE YOUR DATA!!! 
  citation <- paste0("GBIF Occurrence Download https://doi.org/", 
                     download_key[2], " accessed via GBIF.org on ", 
                     Sys.Date())
  return(occ)
}
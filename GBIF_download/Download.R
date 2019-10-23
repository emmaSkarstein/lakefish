#################################################################################
#
# Download GBIF data by asyncronous download.
# See https://github.com/GBIF-Europe/nordic_oikos_2018_r/blob/master/s3_gbif_demo/Download_gbif.md
#
##################################################################################

#-------------------------------------------------------------------------------
# Load packages
#-------------------------------------------------------------------------------

library(rgbif)        # for interacting with gbif
library(dplyr)        # for data-wrangling
library(rstudioapi)   # for writing in user info
library(rio)          # a swiss army knife for data I/O
library(here)         # for knowing where we are



#-------------------------------------------------------------------------------
# Register credentials 
#-------------------------------------------------------------------------------

options(gbif_user=rstudioapi::askForPassword("my gbif username"))
options(gbif_email=rstudioapi::askForPassword("my registred gbif e-mail"))
options(gbif_pwd=rstudioapi::askForPassword("my gbif password"))


#-------------------------------------------------------------------------------
# Set search parameters and get download KEY
#-------------------------------------------------------------------------------

# Salmo trutta - Orret
# Esox lucius - Gjedde
# Salmo salar - Laks
# Salvelinus alpinus - Roye
# Rutilus rutilus - Mort
# Perca fluviatilis - Abbor
# Cyprinus carpio - Karpe

# Find a taxonkey - get list of gbif keys to filter download
key <- name_suggest(q='Salmo salar', rank='species')$key[1] 



#-------------------------------------------------------------------------
# Send download request
#-------------------------------------------------------------------------

# Spawn download request - carefull, there is a max limit on simultanious downloads 
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

# Coffebreak version (see https://github.com/GBIF-Europe/nordic_oikos_2018_r/blob/master/s3_gbif_demo/Download_gbif.md)
download_GBIF_API <- function(download_key,destfile_name,n_try,Sys.sleep_duration){
  print(paste("Attempting to download data from GBIF with", n_try,"attempts at", Sys.sleep_duration, "second intervals."))
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
    print(paste("trying... Download link not ready. Time elapsed (min):",
                round(as.numeric(paste(difftime(Sys.time(),start_time, units = "mins"))),2)))
  }
}
download_GBIF_API(download_key=download_key,destfile_name=paste0(temp,"/tmp.zip"),n_try=5,Sys.sleep_duration=30)
print(paste("------IGNORE THE WARNING MESSAGES------"))


#--------------------------------------------------------------------------
# Read inn the occurrence data
#--------------------------------------------------------------------------

occ <- rio::import(unzip(paste0(temp,"/tmp.zip"),files="occurrence.txt"))
if (!dir.exists(here::here("data"))){ # Create "data"-folder if this doesn't exist
  dir.create(here::here("data"))
}

# Save the occurrence data to a file
saveRDS(occ, here::here("data", paste0("GBIF_download_", key, ".rds")))

unlink(temp)
unlink(here::here("GBIF_download", "occurrence.txt"))


# Write download key and citation to .rds files for reference (also available on https://www.gbif.org/user/download)
saveRDS(download_key,here::here("data", paste0("GBIF_download_key_", key, ".rds")))
        
# CITE YOUR DATA!!! 
citation <- paste0("GBIF Occurrence Download https://doi.org/", download_key[2], " accessed via GBIF.org on ", Sys.Date())

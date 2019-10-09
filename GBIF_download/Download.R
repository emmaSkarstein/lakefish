#################################################################################
#
# download GBIF data by asyncronous download.
# See https://github.com/GBIF-Europe/nordic_oikos_2018_r/blob/master/s3_gbif_demo/Download_gbif.md
#
##################################################################################

#-------------------------------------------------------------------------------
# load packages
#-------------------------------------------------------------------------------


library(rgbif)
library(dplyr) # for data-wrangling
library(rstudioapi)
library(rio)
library(here)



#-------------------------------------------------------------------------------
# register credentials 
#-------------------------------------------------------------------------------

options(gbif_user=rstudioapi::askForPassword("my gbif username"))
options(gbif_email=rstudioapi::askForPassword("my registred gbif e-mail"))
options(gbif_pwd=rstudioapi::askForPassword("my gbif password"))

options(gbif_user = "lyderiversen")
options(gbif_email = "lyder.iversen95@hotmail.com")
options(gbif_pwd="Retsam144")

#-------------------------------------------------------------------------------
# Set search parameters and get download KEY
#-------------------------------------------------------------------------------

# Find a taxonkey - get list of gbif keys to filter download
key <- name_suggest(q='Esox lucius', rank='species')$key[1] 

# Crate spatial filter 

# manually create polygon using mapedit
# polygon <- mapedit::drawFeatures(map=NULL)
# polygon <- sf::st_geometry(polygon)
# my_wkt <- sf::st_as_text(polygon)
# #wicket::validate_wkt(my_wkt)
# geom_param <- paste("geometry", "within", my_wkt)
# # Send download request

# spawn download request - carefull, there is a max limit on simultanious downloads 
# per user allowed by the GBIF API. 
download_key <- occ_download(
  'taxonKey = 2346633',
  'hasCoordinate = TRUE',
  'country = NO',
  #geom_param,
  type = "and"
) %>% 
  occ_download_meta


# Download data when ready (see https://www.gbif.org/user/download). 
# The preparation will of the data may take some time.
temp <- tempdir()
download.file(url=paste("http://api.gbif.org/v1/occurrence/download/request/",
                        download_key[1],sep=""),
              destfile=paste0(temp,"/tmp.zip"),
              quiet=TRUE, mode="wb")

# or....
# coffebreak version (see https://github.com/GBIF-Europe/nordic_oikos_2018_r/blob/master/s3_gbif_demo/Download_gbif.md)
download_GBIF_API <- function(download_key,destfile_name,n_try,Sys.sleep_duration){
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

# read inn the occurrence data
occ <- rio::import(unzip(paste0(temp,"/tmp.zip"),files="occurrence.txt"))
if (!dir.exists(here::here("data"))){
  dir.create(here::here("data"))
}
saveRDS(occ,here::here("data", "GBIF_download.rds"))
unlink(temp)

# CITE YOUR DATA!!! 
citation <- paste0("GBIF Occurrence Download https://doi.org/", download_key[2], " accessed via GBIF.org on ", Sys.Date())

# Write download key and citation to .rds files for refference (also awailable on https://www.gbif.org/user/download)
saveRDS(download_key,here::here("data","GBIF_download_key.rds"))
        
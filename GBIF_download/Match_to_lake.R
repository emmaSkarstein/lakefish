################################################################################
#
#  Match GBIF occurrence to lakes
#
################################################################################
library(ggplot2)
library(sf)
library(dplyr)
library(mapview)
library(here)

#------------------------------------------------------------------------------
# load lake layer
# Example dataset used fennoscandian a lake dataset from the NOFA database (https://github.com/ninaNor/nofa/wiki)
# sharing links below directs to a simple feture object (SF) stored in .rdf format
#------------------------------------------------------------------------------

# Run Download.R to download the data
#source(here::here("GBIF_download","Download.R"))

# Fennoscandian lake data convinience download -----------------------------------

Finnish_data_url <- "https://api.loke.aws.unit.no/dlr-gui-backend-resources-content/v2/contents/links/565cfeef-59bb-44de-b616-4f7a02cbef408559ef0b-7ffd-4d03-b5b6-15a30a58b41f04742bb2-3f6f-4aef-9f09-a3e65861bdbf"
Norwegian_data_url <- "https://api.loke.aws.unit.no/dlr-gui-backend-resources-content/v2/contents/links/aa79c15a-eb88-489b-8b20-4cb6275766cd4f48325a-d84b-4395-93ea-ccd3283c8d04197a8a81-9d30-4f94-a3c3-b4c2532fc2a5"
Swedish_data_url <- "https://api.loke.aws.unit.no/dlr-gui-backend-resources-content/v2/contents/links/b440d734-03bc-4226-853f-e2b8810b65193ff6b1ab-74e4-40f7-9fbd-5f70d80cf19b215d0dbd-b311-42f3-ab57-6c954c515320"
Fennoscandia_data_url <- "https://api.loke.aws.unit.no/dlr-gui-backend-resources-content/v2/contents/links/571d50fd-6db0-45e7-b955-144407ecd33cb6afba4b-0762-44e7-9f8d-a3af4241f93953100f03-6f92-49af-97d9-aaac791a3c13"


# temp <- tempdir()
# download.file(url = Norwegian_data_url,destfile = paste0(temp,"/lakes.rds"),mode = "wb")
# lakes <- readRDS(paste0(temp,"/lakes.rds"))
# unlink(temp)
# if (!dir.exists(here::here("data"))){
#   dir.create(here::here("data"))
# }
# saveRDS(lakes,here::here("data","lake_polygons.rds"))
lakes <- readRDS(here::here("data","lake_polygons.rds"))

#-------------------------------------------------------------------------------------------------
# load GBIF occurrence data and convert to EPSG:32633 (or whatever the same as the lake dataset)
#-------------------------------------------------------------------------------------------------

occ <- readRDS(here::here("data","GBIF_download.rds")) %>% 
  dplyr::select_if(~!all(is.na(.)))
# Remove variables that contain na-values (is this what it does?)


occ_sf <- occ %>% 
  # Convert to sf object for easier handling. crs = Coordinate Reference System
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  
  # Transform coordinate system, using same system as in "lakes"
  st_transform(st_crs(lakes)$epsg) %>%
  
  # Select a few of the variables
  dplyr::select(gbifID, occurrenceID, catalogNumber, geometry, species, 
                taxonKey, datasetKey, locality, municipality, county, 
                countryCode, locationID, eventDate, year, month, day, 
                samplingProtocol, eventID, fieldNumber, recordedBy, 
                dynamicProperties, collectionCode, datasetName, license, 
                institutionCode)


# Have a look at the data: 
#mapview(occ_sf)

#-------------------------------------------------------------------------------------------------
# find closest lake, distance to closest lake, and join
#-------------------------------------------------------------------------------------------------

# # find closest lake - and join
# occ_sf1 <- occ_sf
# start_time <- Sys.time()
# garg <- st_join(occ_sf1, lakes, join = st_nearest_feature)
# end_time <- Sys.time()
# end_time - start_time

# find distance to closest lake

# selection for test purposes
#occ_sf1 <- occ_sf[1:100,]

# find closest lake
start_time <- Sys.time()
occ_with_lakes <- st_join(occ_sf1, lakes, join = st_nearest_feature)
end_time <- Sys.time()
end_time - start_time

# find distance to closest lake
start_time <- Sys.time()
index <- st_nearest_feature(x = occ_sf1, y = lakes) # index of closest lake
closest_lakes <- lakes %>% slice(index) # slice based on the index
dist_to_lake <- st_distance(x = occ_sf1, y= closest_lakes, by_element = TRUE) # get distance
occ_with_lakes$dist_to_lake <- as.numeric(dist_to_lake) # add the distance calculations to match data
end_time <- Sys.time()
end_time - start_time

occ_farfaraway <- occ_with_lakes %>% filter(dist_to_lake > 1000)
# mapview(occ_farfaraway)


#-------------------------------------------------------------------------------------------------
# Filter out occurrence records not matching lakes (given certain criteria)
#-------------------------------------------------------------------------------------------------

occ_matched <- occ_with_lakes %>% filter(dist_to_lake <= 10) # example, 10 m

#mapview(occ_matched)

#-------------------------------------------------------------------------------------------------
# Looking closer at occurrence records not matching lakes (given certain criteria)
#-------------------------------------------------------------------------------------------------

occ_far_from_lake <- occ_with_lakes %>% filter(dist_to_lake > 10)

mapview(occ_far_from_lake)

# Number of observations outside limit:
cat("Number of observations further than 10m from a lake: ", nrow(occ_far_from_lake))


# Now: do we wish to actually move the observations so they are in the lake?



# # trying st_nn function of the nngeo package - seems to move slow
# #install.packages("nngeo")
# library(nngeo)
# occ_sf1 <- occ_sf[1:5,]
# start_time <- Sys.time()
# garg <- st_nn(occ_sf1, lakes, sparse = TRUE, k = 2, maxdist = 1500,
#       returnDist = TRUE, progress = TRUE)
# end_time <- Sys.time()
# end_time - start_time

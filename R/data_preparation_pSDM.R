library(plyr)
library(dplyr)
library(here)

#source(here::here("R/download_lakefish.R"))
source(here::here("R/match_to_lake.R"))

#-------------------------------------------------------------------------
# Step 0: Merge event and occurence files
#-------------------------------------------------------------------------
merge_occ_event <- function(file_path, file_type, name, sep = "\t", quote = "\"'"){
  occ_path <- paste0(file_path, "/occurrence.", file_type)
  event_path <- paste0(file_path, "/event.", file_type)
  
  occ <- read.table(occ_path, header = TRUE, sep = sep, quote = quote, 
                    fill = FALSE)
  event <- read.table(event_path, header = TRUE,sep = sep, quote = quote, 
                      fill = FALSE)
  
  data <- merge(occ, event, by = "eventID")
  
  saveRDS(data, paste0(file_path, "/", name,"_merged.rds"))
}

merge_occ_event(file_path = here::here("data","TranscribedGillnet"), 
                file_type = "txt", name = "TranscribedGillnet")

occ_lakefish <- readRDS(here::here("data","TranscribedGillnet","TranscribedGillnet_merged.rds"))
lakes <- readRDS(here::here("data","lake_polygons.rds"))

#-------------------------------------------------------------------------
# Step 1: Filter out species not in list
#-------------------------------------------------------------------------
#fish_names <- read.table("fish_names.txt", sep = "\n", stringsAsFactors = FALSE)[,1]
#occ_lakefish <- occ %>% filter(species %in% fish_names)

#-------------------------------------------------------------------------
# Step 2: Match to closest lake
#-------------------------------------------------------------------------
occ_list <- match_to_lake(occ_lakefish, lakes)
occ_matched <- occ_list[[1]]
#occ_w_lakes <- occ_list[[2]]
#saveRDS(occ_matched, here::here("data", paste0("GBIF_download_", name, "_matched.rds")))
#saveRDS(occ_w_lakes, here::here("data", paste0("GBIF_download_", name, "_wlakes.rds")))

#-------------------------------------------------------------------------
# Step 3: Remove all observations with no time variable
#-------------------------------------------------------------------------
occ_matched <- occ_matched[complete.cases(occ_matched$year,occ_matched$month,occ_matched$day),]
st_geometry(occ_matched) <- NULL

#-------------------------------------------------------------------------
#Step 4: Select 12 most prevalent species
#-------------------------------------------------------------------------
occ_matched <- occ_matched %>% rename("species" = scientificName)
abundant_species <- occ_matched %>% filter(occurrenceStatus == "present") %>% count(vars = species) %>% top_n(n = 12, wt = n) %>% arrange(desc(n))
occ_matched <- occ_matched %>% filter(species %in% abundant_species$vars)

#-------------------------------------------------------------------------
# For presence-absence: Make occurence-status logical
#-------------------------------------------------------------------------
occ_matched$occurrenceStatus <- as.logical(mapvalues(occ_matched$occurrenceStatus, c("absent", "present"), c(FALSE,TRUE)))

#-------------------------------------------------------------------------
# Save this in the data-folder
#-------------------------------------------------------------------------
saveRDS(occ_matched, here::here("data","TranscribedGillnet","TranscribedGillnet_filtered.rds"))            
               
occ_matched <- occ_matched[complete.cases(occ_matched$ebint),]
testtable <- occ_matched %>% count(vars = occ_matched$species) %>% arrange(desc(n))   
testtable <- occ_matched %>% filter(occ_matched$ebint == "5291810") 


library(beepr)
library(dplyr)
library(here)

source("R/download_lakefish.R")
source("R/match_to_lake.R")

name <- "Actinopterygii"
key <- name_suggest(q = name, rank = 'class')$key[1]

# Download all fish-data
download_lakefish(name, n_try = 100, file_marker = name, key_num = key)
beep(0)

occ <- readRDS(here::here("data", paste0("GBIF_download_", name, ".rds")))
lakes <- readRDS(here::here("data","lake_polygons.rds"))

#-------------------------------------------------------------------------
# Step 1: Filter out species not in list
#-------------------------------------------------------------------------
fish_names <- read.table("fish_names.txt", sep = "\n", stringsAsFactors = FALSE)[,1]
occ_lakefish <- occ %>% filter(species %in% fish_names)

#-------------------------------------------------------------------------
# Step 2: Match to closest lake
#-------------------------------------------------------------------------
occ_list <- match_to_lake(occ_lakefish, lakes)
beep(0)
occ_matched <- occ_list[[1]]
occ_w_lakes <- occ_list[[2]]
saveRDS(occ_matched, here::here("data", paste0("GBIF_download_", name, "_matched.rds")))
saveRDS(occ_w_lakes, here::here("data", paste0("GBIF_download_", name, "_wlakes.rds")))

#-------------------------------------------------------------------------
# Step 3: Remove all observations with no time variable
#-------------------------------------------------------------------------
occ_time <- occ_matched[complete.cases(occ_matched$year,occ_matched$month,occ_matched$day),]


#-------------------------------------------------------------------------
#Step 4: Select 12 most prevalent species
#-------------------------------------------------------------------------
abundant_species <- count(occ_time, vars = species) %>% top_n(n = 12, wt = n) %>% arrange(desc(n))
large_occ <- occ_time %>% filter(occ_time$species %in% abundant_species$vars)


#-------------------------------------------------------------------------
# Save this in the report-folder
#-------------------------------------------------------------------------
saveRDS(large_occ, here::here("reports", "filtered_occurences.rds"))            
               
               


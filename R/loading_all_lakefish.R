#---------------------------------------------------------------------------
# Testing all lakefish
#---------------------------------------------------------------------------
source("R/download_lakefish.R")
source("R/match_to_lake.R")

fish_names <- read.table("fish_names.txt", sep = "\n", stringsAsFactors = FALSE)[,1]

download_lakefish(fish_names, file_marker = "all_fish", n_try = 50)
beep(0)

occ <- readRDS(here::here("data", "GBIF_download_all_fish.rds"))



lakes <- readRDS(here::here("data","lake_polygons.rds"))
occ <- readRDS(here::here("data", paste0("GBIF_download_all_fish.rds")))
occ_list <- match_to_lake(occ, lakes)
beep(0)
occ_matched <- occ_list[[1]]
occ_w_lakes <- occ_list[[2]]
saveRDS(occ_matched, here::here("data", paste0("GBIF_download_all_fish_matched.rds")))
saveRDS(occ_w_lakes, here::here("data", paste0("GBIF_download_all_fish_wlakes.rds")))





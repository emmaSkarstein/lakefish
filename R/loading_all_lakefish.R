#---------------------------------------------------------------------------
# Testing all lakefish
#---------------------------------------------------------------------------
source("R/download_lakefish.R")
source("R/match_to_lake.R")

fish_names <- read.table("fish_names.txt", sep = "\n", stringsAsFactors = FALSE)[,1]
names1 <- fish_names[1:20]
names2 <- fish_names[21:25]
names3 <- fish_names[26:44]

#download_lakefish(names1, file_marker = "all_fish1", n_try = 30)
#download_lakefish(names2, file_marker = "all_fish2", n_try = 30)
download_lakefish(names3, file_marker = "all_fish3", n_try = 30)
beep(0)

occ1 <- readRDS(here::here("data", "GBIF_download_all_fish1.rds"))
occ2 <- readRDS(here::here("data", "GBIF_download_all_fish2.rds"))
occ3 <- readRDS(here::here("data", "GBIF_download_all_fish3.rds"))
beep(0)

occ <- rbind(occ1, occ2, occ3)
saveRDS(occ, here::here("data", paste0("GBIF_download_all_fish.rds")))



# Now run from here
lakes <- readRDS(here::here("data","lake_polygons.rds"))
occ <- readRDS(here::here("data", paste0("GBIF_download_all_fish.rds")))
occ_list <- match_to_lake(occ, lakes)
beep(0)
occ_matched <- occ_list[[1]]
occ_w_lakes <- occ_list[[2]]
saveRDS(occ_matched, here::here("data", paste0("GBIF_download_all_fish_matched.rds")))
saveRDS(occ_w_lakes, here::here("data", paste0("GBIF_download_all_fish_wlakes.rds")))





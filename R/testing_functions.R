# Testing functions
# Some code for testing functions and doing basic plotting

library(beepr)
library(gganimate)
library(gifski)

source("R/download_lakefish.R")
source("R/match_to_lake.R")

name <- "Carassius auratus"
download_lakefish(name)
beep(sound = 4)

lakes <- readRDS(here::here("data","lake_polygons.rds"))
occ <- readRDS(here::here("data", "GBIF_download_4286942.rds"))

occ_list <- match_to_lake(occ, lakes)
beep(sound = 4)

occ_matched <- occ_list[[1]]
occ_w_lakes <- occ_list[[2]]

#data.coords <- cbind(occ_matched$decimalLongitude, occ_matched$decimalLatitude)

norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)") 

# Setting theme for all plots
theme_set(theme_light() + theme(panel.background = element_rect(fill = "aliceblue")))

# Plotting matched locations
ggplot(occ_matched) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), 
           color="#2b2b2b", fill="white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude), 
             show.legend = FALSE, alpha = 0.7, size = 1, color = "red3") +
  ggtitle(paste0(name))


# Plotting all obs
ggplot(occ_w_lakes) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), 
           color="#2b2b2b", fill="white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude), 
             show.legend = FALSE, alpha = 0.7, size = 1, color = "red3") +
  ggtitle(paste0(name)) 

# Plotting the deleted observations
occ_cut <- occ_w_lakes %>% filter(dist_to_lake > 10)

ggplot(occ_cut) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id = region), 
           color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = dist_to_lake), 
                 show.legend = TRUE, alpha = 0.7, size = 1) +
  scale_colour_gradient(low = "red3", high = "deepskyblue4") + 
  ggtitle(paste0(name)) 


ggplot(occ_w_lakes, aes(x=dist_to_lake)) + 
  geom_histogram(color="black", fill="white")

ggplot(occ_matched, aes(x=dist_to_lake)) + 
  geom_histogram(color="black", fill="white") +
  ggtitle("Histogram of distance to closest lake for matched data")

non_zero_dist <- filter(occ_matched, dist_to_lake > 0)

ggplot(non_zero_dist, aes(x=dist_to_lake)) + 
  geom_histogram(color="black", fill="white") +
  ggtitle("Histogram of distance to closest lake for matched data (except for 0-dist)")

ggplot(occ_cut, aes(x=dist_to_lake)) + 
  geom_histogram(color="black", fill="white") + 
  ggtitle("Histogram of log of distance to closest lake for removed data") + 
  scale_x_log10()

#---------------------------------------------------------------------------
# Testing two species
#---------------------------------------------------------------------------
name <- c("Carassius auratus", "Aspius aspius")
download_lakefish(name, file_marker = "two_fish")
beep(sound = 4)

lakes <- readRDS(here::here("data","lake_polygons.rds"))
occ <- readRDS(here::here("data", "GBIF_download_two_fish.rds"))

occ_list <- match_to_lake(occ, lakes)
beep(sound = 4)

occ_matched <- occ_list[[1]]
occ_w_lakes <- occ_list[[2]]


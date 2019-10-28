# Testing functions
library(beepr)
library(gganimate)
library(gifski)

source("R/download_lakefish.R")
source("R/match_to_lake.R")

download_lakefish("Rutilus rutilus")
beep(sound = 4)

lakes <- readRDS(here::here("data","lake_polygons.rds"))
occ <- readRDS(here::here("data", "GBIF_download_2359706.rds"))

occ_list <- match_to_lake(occ, lakes)
beep(sound = 4)

occ_matched <- occ_list[[1]]
occ_w_lakes <- occ_list[[2]]

#data.coords <- cbind(occ_matched$decimalLongitude, occ_matched$decimalLatitude)

norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)") 

# Plotting matched locations
ggplot(occ_matched) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), 
           color="#2b2b2b", fill=NA) + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = dist_to_lake), 
             size=0.001) + 
  scale_colour_gradient(low = "blue", high = "yellow")


# Plotting all obs
ggplot(occ_w_lakes) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id = region), 
           color="#2b2b2b", fill=NA) + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = dist_to_lake), 
             size=0.001) +
  scale_colour_gradient(low = "blue", high = "yellow")

occ_cut <- occ_w_lakes %>% filter(dist_to_lake > 10)

ggplot(occ_cut) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id = region), 
           color="#2b2b2b", fill=NA) + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = dist_to_lake), 
             size=0.001) +
  scale_colour_gradient(low = "blue", high = "yellow")

ggplot(occ_w_lakes, aes(x=dist_to_lake)) + 
  geom_histogram(color="black", fill="white")

ggplot(occ_matched, aes(x=dist_to_lake)) + 
  geom_histogram(color="black", fill="white") +
  ggtitle("Histogram of distance to closest lake for matched data")

non_zero_dist <- filter(occ_matched, dist_to_lake>0)
(nrow(occ_matched) - nrow(non_zero_dist))/nrow(occ_matched)

ggplot(non_zero_dist, aes(x=dist_to_lake)) + 
  geom_histogram(color="black", fill="white") +
  ggtitle("Histogram of distance to closest lake for matched data (except for 0-dist)")

ggplot(occ_cut, aes(x=dist_to_lake)) + 
  geom_histogram(color="black", fill="white") + 
  ggtitle("Histogram of log of distance to closest lake for removed data") + 
  scale_x_log10()




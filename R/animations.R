#-----------------------------------------------------------------------------------
# Animations
#-----------------------------------------------------------------------------------
name <- "Rutilus rutilus"

download_lakefish(name)
lakes <- readRDS(here::here("data","lake_polygons.rds"))
occ <- readRDS(here::here("data", "GBIF_download_2359706.rds"))
occ_list <- match_to_lake(occ, lakes)
occ_matched <- occ_list[[1]]

norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)") 
occ_time <- occ_matched[complete.cases(occ_matched$year,occ_matched$month,occ_matched$day),]

# Plotting matched observations with animation
ggplot(occ_time) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), 
           color="#2b2b2b", fill=NA) + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude), 
             show.legend = FALSE, alpha = 0.7, size = 0.1, color = "deepskyblue4") +
  labs(title = 'Year: {frame_time}') + 
  transition_time(year) + 
  ggtitle()

anim_save("fish_animation.gif")

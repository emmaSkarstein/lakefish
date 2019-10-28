#-----------------------------------------------------------------------------------
# Animations
#-----------------------------------------------------------------------------------
name <- "Salmo trutta"

key = download_lakefish(name)$key
lakes <- readRDS(here::here("data", "lake_polygons.rds"))
occ <- readRDS(here::here("data", paste0("GBIF_download_", key, ".rds")))
occ_list <- match_to_lake(occ, lakes)
occ_matched <- occ_list[[1]]

norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)") 
occ_time <- occ_matched[complete.cases(occ_matched$year,occ_matched$month,occ_matched$day),]

# Setting theme for all plots
theme_set(theme_light() + theme(aspect.ratio = .70, panel.background = element_rect(fill = "aliceblue")))

# Plotting matched observations with animation
ggplot(occ_time) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), 
           color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude), 
             show.legend = FALSE, alpha = 0.6, size = 1.5, color = "red3")  +
  labs(title = paste0(name, ', year: {frame_time}')) + 
  transition_time(year)

anim_save("fish_animation.gif")

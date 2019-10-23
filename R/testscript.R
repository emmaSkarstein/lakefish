library(ggplot2)
library(sf) # for sf objects
library(dplyr) # for smoother dataframe-manipulation
library(here) # for cleaner filepath-handling
library(mapview) # for nice maps
library(ggmap) # also for nice maps
library(maps)
library(maptools)
library(mapdata)
library(cowplot)

occ_matched <- readRDS(here::here("data","occ_matched_allfish.rds"))
lakes <- readRDS(here::here("data","lake_polygons.rds"))

occ_nona <- occ_matched[complete.cases(occ_matched$year,occ_matched$month,occ_matched$day),]

data.coords <- cbind(occ_matched$decimalLongitude, occ_matched$decimalLatitude)

norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)")
ggplot() +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), color="#2b2b2b", fill=NA) +
  geom_point(aes(x = lakes$decimalLongitude, y = lakes$decimalLatitude), size=0.001, color = "grey") +
  geom_point(aes(x = occ_matched$decimalLongitude, y = occ_matched$decimalLatitude), size=0.001, color = "red")

pick <- function(condition){
  function(d) d %>% filter(!!enquo(condition))
}

norway1980 <- ggplot(data = occ_nona, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), color="#2b2b2b", fill="white") +
  geom_point(data = pick(year<1980 & year>1970), size=0.001, color = "red") +
  ggtitle("Fish data between 1970 and 1980")

norway1990 <- ggplot(data = occ_nona, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), color="#2b2b2b", fill="white") +
  geom_point(data = pick(year<1990 & year>1980), size=0.001, color = "red") +
  ggtitle("Fish data between 1980 and 1990")

norway2000 <- ggplot(data = occ_nona, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), color="#2b2b2b", fill="white") +
  geom_point(data = pick(year<2000 & year>1990), size=0.001, color = "red") +
  ggtitle("Fish data between 1990 and 2000")

norway2010 <- ggplot(data = occ_nona, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), color="#2b2b2b", fill="white") +
  geom_point(data = pick(year<2010 & year>2000), size=0.001, color = "red") +
  ggtitle("Fish data between 2000 and 2010")

plot_grid(norway1980, norway1990, norway2000, norway2010, labels = "AUTO", align = "h")

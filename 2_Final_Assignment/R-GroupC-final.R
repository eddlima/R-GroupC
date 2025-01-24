library(tidyverse)
library(ggrepel)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(forcats)
library(sf)
library(tmap)

for(policy_name in c("base","policy")) {
  
  base_url <- str_c("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/tutorial/datascience2024/matsim_outputs/output-1pct/", policy_name, "/berlin-v6.3.output_")
  
  for(dataset_name in c("persons", "activities", "trips", "legs")){
    # for(dataset_name in c("trips")){
    destfile <- str_c(dataset_name, ".csv.gz")
    url <- stringr::str_c(base_url, destfile)
    
    download.file(url, destfile)
    
    assign(str_c(policy_name, "_", dataset_name), value = read_delim(gzfile(destfile)))
  }
}

bb_kreise_shp <- st_read("C://Users//ibrah//OneDrive//Documents//GitHub//R-GroupC//2_Final_Assignment//103837211850837114_data//pos_1//GRENZE_152811-5683624_kreise.shp")

st_crs(bb_kreise_shp)

tmap_mode("view")
tm_shape(bb_kreise_shp) + 
  tm_polygons()

base_trips_sf <- base_trips %>% filter(str_starts(person, "bb_")) %>% 
  st_as_sf(coords = c("start_x","start_y"), crs = 25832) %>% st_transform(25833)

st_crs(base_trips_sf)

base_trips_sf %>% 
  tm_shape() +
  tm_dots(col = "main_mode") +
  tm_shape(bb_kreise_shp) + 
  tm_borders() 

# Summing travel time for each person
base_person_bb_travel_time <- base_trips %>%
  filter(grepl("^bb", person, ignore.case = TRUE)) %>% 
  group_by(person) %>%
  summarise(total_trav_time = sum(trav_time, na.rm = TRUE))

policy_person_bb_travel_time <- policy_trips %>%
  filter(grepl("^bb", person, ignore.case = TRUE)) %>% 
  group_by(person) %>%
  summarise(total_trav_time = sum(trav_time, na.rm = TRUE))

persons_bb <- base_persons %>% 
  filter(grepl("^bb", person, ignore.case = TRUE)) %>% 
  filter(!is.na(home_x) & !is.na(home_y))

bb_persons_shp <- full_join(persons_bb, base_person_bb_travel_time) %>% 
  filter(!is.na(total_trav_time)) %>% 
  st_as_sf(coords = c("home_x","home_y"), crs = 25832) %>% st_transform(25833)
plot(bb_persons_shp)

kreise_persons_joined_inner <- bb_kreise_shp %>% st_join(bb_persons_shp, left = FALSE)
#aggregation
kreise_persons_joined_agg <- kreise_persons_joined_inner %>% 
  group_by(krs_name) %>% 
  summarise(population = n(), trav_time = mean(total_trav_time))

tm_shape(kreise_persons_joined_agg) +
  tm_polygons(col = "population")

tm_shape(kreise_persons_joined_agg) +
  tm_polygons(col = "trav_time")






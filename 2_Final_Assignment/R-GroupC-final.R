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

# Base case
base_person_bb_travel_time <- base_trips %>%
  filter(grepl("^bb", person, ignore.case = TRUE)) %>% 
  group_by(person) %>%
  summarise(total_trav_time = sum(trav_time, na.rm = TRUE))

base_persons_bb <- base_persons %>% 
  filter(grepl("^bb", person, ignore.case = TRUE)) %>% 
  filter(!is.na(home_x) & !is.na(home_y))

base_persons_bb_shp <- full_join(base_persons_bb, base_person_bb_travel_time) %>% 
  filter(!is.na(total_trav_time)) %>% 
  st_as_sf(coords = c("home_x","home_y"), crs = 25832) %>% st_transform(25833)

base_kreise_persons_joined_inner <- bb_kreise_shp %>% st_join(base_persons_bb_shp, left = FALSE)

base_kreise_persons_joined_agg <- base_kreise_persons_joined_inner %>%
  group_by(krs_name) %>% 
  summarise(population = n(), trav_time = mean(total_trav_time)) %>% 
  mutate(trav_time = as.numeric(trav_time))

tm_shape(base_kreise_persons_joined_agg) +
  tm_polygons(col = "population")

tm_shape(base_kreise_persons_joined_agg) +
  tm_polygons(col = "trav_time",
              title = "Base Travel Time")

#Policy Case
policy_person_bb_travel_time <- policy_trips %>%
  filter(grepl("^bb", person, ignore.case = TRUE)) %>% 
  group_by(person) %>%
  summarise(total_trav_time = sum(trav_time, na.rm = TRUE))

policy_persons_bb <- policy_persons %>% 
  filter(grepl("^bb", person, ignore.case = TRUE)) %>% 
  filter(!is.na(home_x) & !is.na(home_y))

policy_persons_bb_shp <- full_join(policy_persons_bb, policy_person_bb_travel_time) %>% 
  filter(!is.na(total_trav_time)) %>% 
  st_as_sf(coords = c("home_x","home_y"), crs = 25832) %>% st_transform(25833)

policy_kreise_persons_joined_inner <- bb_kreise_shp %>% st_join(policy_persons_bb_shp, left = FALSE)

policy_kreise_persons_joined_agg <- policy_kreise_persons_joined_inner %>%
  group_by(krs_name) %>% 
  summarise(population = n(), trav_time = mean(total_trav_time)) %>% 
  mutate(trav_time = as.numeric(trav_time))

tm_shape(policy_kreise_persons_joined_agg) +
  tm_polygons(col = "trav_time",
              title = "Policy Travel Time")


#Comparing Base and Policy Travel Times
diff_person_bb_travel_time <- inner_join(base_person_bb_travel_time, policy_person_bb_travel_time,
                                         by = "person", suffix = c("_base","_policy")) %>% 
  mutate(diff_travel_time = total_trav_time_policy - total_trav_time_base) %>% 
  select(person, diff_travel_time)

diff_person_bb_travel_time_shp <- full_join(base_persons_bb, diff_person_bb_travel_time) %>%  
  filter(!is.na(diff_travel_time)) %>% 
  st_as_sf(coords = c("home_x","home_y"), crs = 25832) %>% st_transform(25833)

diff_kreise_persons_joined_inner <- bb_kreise_shp %>% st_join(diff_person_bb_travel_time_shp, left = FALSE)

diff_kreise_persons_joined_agg <- diff_kreise_persons_joined_inner %>%
  group_by(krs_name) %>% 
  summarise(population = n(), diff_travel_time = mean(diff_travel_time)) %>% 
  mutate(diff_travel_time = as.numeric(diff_travel_time))


# Plot travel time difference (continuous values)

tm_shape(diff_kreise_persons_joined_agg) +
  tm_polygons(col = "diff_travel_time",
              title = "Difference in Travel Time (Policy - Base)")


RColorBrewer::display.brewer.all()





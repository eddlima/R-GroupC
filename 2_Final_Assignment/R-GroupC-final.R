library(tidyverse)
library(ggrepel)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(forcats)
library(sf)
library(tmap)
library(readxl)

for(policy_name in c("base","policy")) {
  
  base_url <- str_c("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/tutorial/datascience2024/matsim_outputs/output-1pct/", policy_name, "/berlin-v6.3.output_")
  
  for(dataset_name in c("persons", "activities", "trips", "legs")){
    destfile <- str_c(dataset_name, ".csv.gz")
    url <- stringr::str_c(base_url, destfile)
    
    download.file(url, destfile)
    
    assign(str_c(policy_name, "_", dataset_name), value = read_delim(gzfile(destfile)))
  }
}

bb_kreise_shp <- st_read("C://Users//Eduardo Lima//Documents//TUB//Studium//24-25 WiSe//Data Science for agent-based transport simulations//R-GroupC//R-GroupC//2_Final_Assignment//103837211850837114_data//pos_1//GRENZE_152811-5683624_kreise.shp")

st_crs(bb_kreise_shp)

tmap_mode("view")
tm_shape(bb_kreise_shp) + 
  tm_polygons()

base_persons_sf <- base_persons %>% filter(str_starts(person, "bb_")) %>% 
  st_as_sf(coords = c("home_x","home_y"), crs = 25832) %>% 
  st_transform(25833) %>% 
  st_intersection(bb_kreise_shp)

base_main_mode <- base_trips %>%
  filter(grepl("^bb", person, ignore.case = TRUE)) %>% 
  group_by(person) %>%
  summarise(main_mode) %>% 
  distinct(person, .keep_all = TRUE)

base_trips_sf <- left_join(base_persons_sf, base_main_mode) %>% 
  filter(!is.na(main_mode))

base_trips_sf %>% 
  tm_shape() +
  tm_dots(col = "main_mode",
          title = "Agents' Main Mode in Brandenburg - Base Case") +
  tm_shape(bb_kreise_shp) + 
  tm_borders()

policy_main_mode <- policy_trips %>%
  filter(grepl("^bb", person, ignore.case = TRUE)) %>% 
  group_by(person) %>%
  summarise(main_mode) %>% 
  distinct(person, .keep_all = TRUE)

policy_trips_sf <- left_join(base_persons_sf, policy_main_mode) %>% 
  filter(!is.na(main_mode))

policy_trips_sf %>% 
  tm_shape() +
  tm_dots(col = "main_mode") +
  tm_shape(bb_kreise_shp) + 
  tm_borders()

#Base and Policy Case - Travel Time - Plot
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
  summarise(population = n(), trav_time = median(total_trav_time)) %>% 
  mutate(trav_time = as.numeric(trav_time)/60) %>% 
  mutate(scenario = "Base")

#tm_shape(base_kreise_persons_joined_agg) +
#  tm_polygons(col = "trav_time",
#              title = "Base Travel Time")

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
  summarise(population = n(), trav_time = median(total_trav_time)) %>% 
  mutate(trav_time = as.numeric(trav_time)/60) %>% 
  mutate(scenario = "Policy")

#tm_shape(policy_kreise_persons_joined_agg) +
#  tm_polygons(col = "trav_time",
#              title = "Policy Travel Time")

trav_time_bb_combined <- bind_rows(base_kreise_persons_joined_agg, policy_kreise_persons_joined_agg)

ggplot(trav_time_bb_combined, aes(x = krs_name, y = trav_time, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Median Travel Time in Brandenburg: Base vs Policy Scenario",
       x = "County", y = "Travel Time (min)") +
  scale_fill_manual(values = c("Base" = "darkgreen", "Policy" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))

#Comparing Base and Policy - Travel Time - Map, Presenting Agents Population
diff_person_bb_travel_time <- inner_join(base_person_bb_travel_time, policy_person_bb_travel_time,
                                         by = "person", suffix = c("_base","_policy")) %>% 
  mutate(diff_travel_time = total_trav_time_policy - total_trav_time_base) %>% 
  select(person, diff_travel_time)

diff_person_bb_travel_time_shp <- full_join(base_persons_bb, diff_person_bb_travel_time) %>%  
  filter(!is.na(diff_travel_time)) %>% 
  st_as_sf(coords = c("home_x","home_y"), crs = 25832) %>% 
  st_transform(25833)

diff_kreise_persons_joined_inner <- bb_kreise_shp %>% st_join(diff_person_bb_travel_time_shp, left = FALSE)

diff_kreise_persons_joined_agg <- diff_kreise_persons_joined_inner %>%
  group_by(krs_name) %>% 
  summarise(population = n(), diff_travel_time = median(diff_travel_time)) %>% 
  mutate(
    diff_travel_time = as.numeric(diff_travel_time),
    population_percentage = (population / sum(population, na.rm = TRUE)) * 100
  )

tm_shape(diff_kreise_persons_joined_agg) +
  tm_polygons(col = "diff_travel_time",
              title = "Difference in Travel Time (Policy - Base)",
              palette = "-RdYlGn",
              style = "cont") +
  tm_borders() +
  tm_scale_bar() +
  tm_layout(legend.outside = TRUE)

tm_shape(diff_kreise_persons_joined_agg) +
  tm_polygons(col = "population_percentage",
              title = "Agents Population % to the total agents of Brandenburg",
              palette = "Greens",
              style = "cont",
              breaks = seq(0, 20, by = 5))

st_write(diff_kreise_persons_joined_agg, "diff_trav_time_bb.shp")

#Downloading information from Statistics from Brandenburg
population <- read_excel("Bevoelkerungsstand_Regionaldaten_2023_Berlin-Brandenburg.xlsx", skip=4)

population_tidy <- population[2:19,] %>% 
  rename(krss=...1, krs_name=...2, population = Anzahl) %>% 
  select(krss, krs_name, population) %>% 
  mutate(population = as.numeric(population)) %>% 
  mutate(population_percentage = (population / sum(population, na.rm = TRUE)) * 100)

population_tidy$krss <- gsub("000$", "", population_tidy$krss)

population_bb_shp <- population_tidy %>% 
  left_join(bb_kreise_shp) %>% 
  st_as_sf()

tm_shape(population_bb_shp) +
  tm_polygons(col = "population_percentage",
              title = "Population % to the total of Brandenburg",
              palette = "Greens",
              style = "cont",
              breaks = seq(0, 20, by = 5))

#Base and Policy Case - Car Use in Brandenburg
base_krs_car <- base_trips_sf %>% 
  group_by(krs_name, main_mode) %>% 
  summarise( trip_count = n(), groups= "drop") %>% 
  group_by(krs_name) %>% 
  mutate(total_trips = sum(trip_count)) %>% 
  filter(main_mode == "car") %>%
  mutate(car_percentage = (trip_count / total_trips) * 100) %>% 
  mutate(scenario = "Base")

policy_krs_car <- policy_trips_sf %>% 
  group_by(krs_name, main_mode) %>% 
  summarise( trip_count = n(), groups= "drop") %>% 
  group_by(krs_name) %>% 
  mutate(total_trips = sum(trip_count)) %>% 
  filter(main_mode == "car") %>%
  mutate(car_percentage = (trip_count / total_trips) * 100) %>% 
  mutate(scenario = "Policy")

diff_krs_car <- st_join(base_krs_car, policy_krs_car,
                        by = "krs_name",
                        suffix = c("_base","_policy")) %>% 
  mutate(diff_car_percentage = car_percentage_policy - car_percentage_base)

diff_krs_car_joined_inner <- bb_kreise_shp %>% st_join(diff_krs_car, left = FALSE)

diff_krs_car_joined_agg <- diff_krs_car_joined_inner %>%
  group_by(krs_name) %>% 
  summarise(diff_car_percentage)

tm_shape(diff_krs_car_joined_agg) +
  tm_polygons(col = "diff_car_percentage",
              title = "Reduction % car use in Brandenburg",
              palette = "Oranges",
              style = "cont")

st_write(diff_krs_car_joined_agg, "diff_krs_car_bb.shp")

krs_car_combined <- bind_rows(base_krs_car, policy_krs_car)

ggplot(krs_car_combined, aes(x = krs_name, y = car_percentage, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Car Use as Main Mode in Brandenburg: Base vs Policy Scenario",
       x = "County", y = "Car Use as Main Mode (%)") +
  scale_fill_manual(values = c("Base" = "blue", "Policy" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))



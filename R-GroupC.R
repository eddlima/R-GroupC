library(tidyverse)
library(ggrepel)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(forcats)

#BASE
url <- "https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/tutorial/datascience2024/matsim_outputs/output-1pct/base/berlin-v6.3.output_trips.csv.gz"
destfile <- "berlin-v6.3.base.output_trips.csv.gz"

download.file(url, destfile)

original_dir <- getwd()

file_dir <- dirname(destfile)
setwd(file_dir)

base_data <- read_delim(gzfile(basename(destfile)))

sample(base_data)

#POLICY
url <- "https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/tutorial/datascience2024/matsim_outputs/output-1pct/policy/berlin-v6.3.output_trips.csv.gz"
destfile <- "berlin-v6.3.policy.output_trips.csv.gz"

download.file(url, destfile)

original_dir <- getwd()

file_dir <- dirname(destfile)
setwd(file_dir)

policy_data <- read_delim(gzfile(basename(destfile)))

# Comparing Mode Share for Brandenburg Residents: Base vs. Policy Scenario
mode_share_bb_base <- base_data %>% 
  select(trip_id, main_mode) %>% 
  filter(grepl("^bb", trip_id, ignore.case = TRUE)) %>% 
  count(main_mode) %>% 
  mutate(share = n / sum(n)) %>% 
  mutate(scenario = "base") %>% 
  select(scenario, main_mode, share)

mode_share_bb_policy <- policy_data %>% 
  select(trip_id, main_mode) %>% 
  filter(grepl("^bb", trip_id, ignore.case = TRUE)) %>% 
  count(main_mode) %>% 
  mutate(share = n / sum(n)) %>% 
  mutate(scenario = "policy") %>% 
  select(scenario, main_mode, share)

mode_share_bb_combined <- bind_rows(mode_share_bb_base, mode_share_bb_policy) %>%
  mutate(main_mode = fct_recode(main_mode,
                                "Bike" = "bike",
                                "Car" = "car",
                                "Public Transport" = "pt",
                                "Ride" = "ride",
                                "Walk" = "walk"))

ggplot(mode_share_bb_combined, aes(x = main_mode, y = share, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Mode Share for Brandenburg Residents: Base vs Policy Scenario",
       x = "Mode of Transportation", y = "Share of Trips") +
  scale_fill_manual(values = c("base" = "blue", "policy" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))

# Comparing mean travel time by main mode
base_data_bb <- base_data %>% 
  filter(grepl("^bb", trip_id, ignore.case = TRUE)) %>%
  mutate(total_trav_time = trav_time + wait_time)  
policy_data_bb <- policy_data %>% 
  filter(grepl("^bb", trip_id, ignore.case = TRUE)) %>%
  mutate(total_trav_time = trav_time + wait_time)

mode_base_summary <- base_data_bb %>% 
  group_by(main_mode) %>% 
  summarise (avg_travel_time = mean(total_trav_time, na.rm = TRUE), .groups = 'drop')

mode_policy_summary <- policy_data_bb %>% 
  group_by(main_mode) %>% 
  summarise (avg_travel_time = mean(total_trav_time, na.rm = TRUE), .groups = 'drop')

mode_summary_combined <- left_join(mode_base_summary, mode_policy_summary,
                                   by = "main_mode",
                                   suffix = c("_base", "_policy")) %>%
  mutate(avg_travel_time_diff = avg_travel_time_policy - avg_travel_time_base) %>% 
  mutate(color_diff = ifelse(avg_travel_time_diff > 0, "Positive", "Negative")) %>%
  mutate(main_mode = fct_recode(main_mode,
                                "Bike" = "bike",
                                "Car" = "car",
                                "Public Transport" = "pt",
                                "Ride" = "ride",
                                "Walk" = "walk"))

ggplot(mode_summary_combined, aes(x = main_mode, y = avg_travel_time_diff, fill = color_diff)) +
  geom_col(alpha = 0.7) +
  labs(title = "Difference of Average Travel Time for Brandenburg Residents",
       x = "Mode of Transportation", y = "Difference of Average Travel Time (seconds) - Policy vs. Base Scenario") +
  scale_fill_manual(values = c("Positive" = "green", "Negative" = "red")) +
  theme_minimal() +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))

# Comparing median travel time by main mode
base_data_bb <- base_data %>% 
  filter(grepl("^bb", trip_id, ignore.case = TRUE)) %>%
  mutate(total_trav_time = trav_time + wait_time)  
policy_data_bb <- policy_data %>% 
  filter(grepl("^bb", trip_id, ignore.case = TRUE)) %>%
  mutate(total_trav_time = trav_time + wait_time)

mode_base_summary <- base_data_bb %>% 
  group_by(main_mode) %>% 
  summarise (avg_travel_time = median(total_trav_time, na.rm = TRUE), .groups = 'drop')

mode_policy_summary <- policy_data_bb %>% 
  group_by(main_mode) %>% 
  summarise (avg_travel_time = median(total_trav_time, na.rm = TRUE), .groups = 'drop')

mode_summary_combined <- left_join(mode_base_summary, mode_policy_summary,
                                   by = "main_mode",
                                   suffix = c("_base", "_policy")) %>%
  mutate(avg_travel_time_diff = avg_travel_time_policy - avg_travel_time_base) %>% 
  mutate(color_diff = ifelse(avg_travel_time_diff > 0, "Positive", "Negative")) %>%
  mutate(main_mode = fct_recode(main_mode,
                                "Bike" = "bike",
                                "Car" = "car",
                                "Public Transport" = "pt",
                                "Ride" = "ride",
                                "Walk" = "walk"))

ggplot(mode_summary_combined, aes(x = main_mode, y = avg_travel_time_diff, fill = color_diff)) +
  geom_col(alpha = 0.7) +
  labs(title = "Difference of Average Travel Time for Brandenburg Residents",
       x = "Mode of Transportation", y = "Difference of Average Travel Time (seconds) - Policy vs. Base Scenario") +
  scale_fill_manual(values = c("Positive" = "green", "Negative" = "red")) +
  theme_minimal() +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))






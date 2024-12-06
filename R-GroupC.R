library(tidyverse)
library(ggrepel)
library(RColorBrewer)
library(lubridate)

#BASE
# Define the URL of the csv.gz file and the destination file path
url <- "https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/tutorial/datascience2024/matsim_outputs/output-1pct/base/berlin-v6.3.output_trips.csv.gz"
destfile <- "berlin-v6.3.base.output_trips.csv.gz"

# Download the file
download.file(url, destfile)

# Get the current working directory (to return later, if needed)
original_dir <- getwd()

# Move to the directory where the file is located
file_dir <- dirname(destfile)
setwd(file_dir)

# Unzip and read the file
base_data <- read_delim(gzfile(basename(destfile)))

sample(base_data)

#POLICY
# Define the URL of the csv.gz file and the destination file path
url <- "https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/tutorial/datascience2024/matsim_outputs/output-1pct/policy/berlin-v6.3.output_trips.csv.gz"
destfile <- "berlin-v6.3.policy.output_trips.csv.gz"

# Download the file
download.file(url, destfile)

# Get the current working directory (to return later, if needed)
original_dir <- getwd()

# Move to the directory where the file is located
file_dir <- dirname(destfile)
setwd(file_dir)

# Unzip and read the file
policy_data <- read_delim(gzfile(basename(destfile)))



base_person <- base_data %>% select(person)
base_person

base_person_berlin <- base_person %>% filter(!grepl("^bb_", person, ignore.case = TRUE))
base_person_berlin

base_person_bb <- base_person %>% filter(grepl("^bb_", person, ignore.case = TRUE))
base_person_bb

fct_count(base_data$main_mode)

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

mode_share_bb_combined <- bind_rows(mode_share_bb_base, mode_share_bb_policy)

ggplot(mode_share_bb_combined, aes(x = main_mode, y = share, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Mode Share for Brandenburg Residents: Base vs Policy Scenario",
       x = "Mode of Transportation", y = "Share of Trips") +
  scale_fill_manual(values = c("base" = "blue", "policy" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))


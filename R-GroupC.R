library(tidyverse)
library(ggrepel)
library(RColorBrewer)
library(lubridate)

#BASE
# Define the URL of the csv.gz file and the destination file path
url <- "https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/tutorial/datascience2024/matsim_outputs/output-1pct/base/berlin-v6.3.output_persons.csv.gz"
destfile <- "berlin-v6.3.output_persons.csv.gz"

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
url <- "https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/tutorial/datascience2024/matsim_outputs/output-1pct/policy/berlin-v6.3.output_persons.csv.gz"
destfile <- "berlin-v6.3.output_persons.csv.gz"

# Download the file
download.file(url, destfile)

# Get the current working directory (to return later, if needed)
original_dir <- getwd()

# Move to the directory where the file is located
file_dir <- dirname(destfile)
setwd(file_dir)

# Unzip and read the file
policy_data <- read_delim(gzfile(basename(destfile)))

sample(policy_data)

base_person <- base_data %>% select(person)
base_person

base_person_berlin <- base_person %>% filter(!grepl("^bb_", person, ignore.case = TRUE))
base_person_berlin

base_person_bb <- base_person %>% filter(grepl("^bb_", person, ignore.case = TRUE))
base_person_bb
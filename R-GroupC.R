library(tidyverse)
library(ggrepel)
library(RColorBrewer)
library(lubridate)

# Define the URL of the csv.gz file and the destination file path
url <- "https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/tutorial/datascience2024/matsim_outputs/output-1pct/policy/berlin-v6.3.output_trips.csv.gz"
destfile <- "yourfile.csv.gz"

# Download the file
download.file(url, destfile)

# Get the current working directory (to return later, if needed)
original_dir <- getwd()

# Move to the directory where the file is located
file_dir <- dirname(destfile)
setwd(file_dir)

# Unzip and read the file
trips_data <- read_delim(gzfile(basename(destfile)))

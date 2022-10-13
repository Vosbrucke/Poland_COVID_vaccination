library(plyr)
library(dplyr)
library(readr)
library(tidyverse)
library(purrr)
library(downloader)
library(lubridate)

# Get data of the most recently available data on daily infections in Poland by county

# Data source. The url link is taken from 'https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2'
url <- "https://arcgis.com/sharing/rest/content/items/e16df1fa98c2452783ec10b0aea4b341/data"

# Download and uzip the most recent data. Until 2022-05-19 they were updated daily. 
# When epidemic status in Poland was lifted the updates were also stopped
download(url, dest="Raw_data/infections_county/dataset.zip", mode="wb") 
unzip("Raw_data/infections_county/dataset.zip", exdir = "Raw_data/infections_county")

# List downloaded files using pattern to make sure these are data for counties 
files <- list.files(path = "Raw_data/infections_county", pattern = ".csv")

# Set working directory for ldply function
setwd("/Users/mr.fox/Desktop/Github/COVID_wg_powiatow/Raw_data/infections_county")

# Load all the files into one data frame
df <- ldply(
  .data = files, 
  .fun = function(i) {
    # Substract date from file name
    i_name <- paste(substr(i,1,4), substr(i,5,6), substr(i,7,8), sep = "-")
    # Read csv
    read.csv(i, header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE, na.strings = c("", "NA")) %>% 
      # Add date column
      mutate(stan_rekordu_na = as.Date(i_name) - days(1))
    }
  )

# Get back to regular working directory
setwd("/Users/mr.fox/Desktop/Github/COVID_wg_powiatow")

# Load county names. Some of the files had incorrect format of counties names resulting in bugs.
county_names <- read.csv("Raw_data/county_names.csv")[, -1]

# Fix the bugs using teryt column.
df <- county_names %>%
  right_join(df, by =  "teryt") %>%
  # Remove old, bugged county and voivodeship data
  select(-c(powiat_miasto.y, wojewodztwo.y)) %>%
  # Rename selected columns
  dplyr::rename(voivodeship = wojewodztwo.x, 
         county = powiat_miasto.x) %>% 
  # Remove aggregated data for the whole Poland
  filter(teryt != "t00") %>%
  # Change NA to 0
  dplyr::mutate(across(everything(), ~ifelse(is.na(.), 0, .)),
                stan_rekordu_na = as.Date(stan_rekordu_na, origin = as.Date("1970-01-01")))

# Write data
write.csv(df, "Processed_data/daily_infections.csv")

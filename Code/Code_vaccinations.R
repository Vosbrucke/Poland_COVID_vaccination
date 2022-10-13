library(plyr)
library(dplyr)
library(readr)
library(tidyverse)
library(purrr)
library(downloader)
library(data.table)
library(rvest)

# Get data of the most recently available data on daily vaccination in Poland by county

# Data source. The url link is taken from 'https://www.gov.pl/web/szczepimysie/raport-szczepien-przeciwko-covid-19'
url <- "https://arcgis.com/sharing/rest/content/items/b860f2797f7f4da789cb6fccf6bd5bc7/data"

# Download and uzip the most recent data. Until 2022-05-19 they were updated daily. 
# When epidemic status in Poland was lifted the updates were also stopped
download(url, dest="Raw_data/vaccinations_county/dataset.zip", mode="wb") 
unzip("Raw_data/vaccinations_county/dataset.zip", exdir = "Raw_data/vaccinations_county/")

# List downloaded files using pattern to make sure these are data for counties 
files <- list.files(path = "Raw_data/vaccinations_county", pattern = "rcb_pow_szczepienia.csv")

# Set working directory for ldply function
setwd("/Users/mr.fox/Desktop/Github/COVID_wg_powiatow/Raw_data/vaccinations_county")

# Load all the files into one data frame
df <- ldply(
    .data = files, 
    .fun = function(i) {
      # Substract date from file name
      i_name <- paste(substr(i, 1, 4), substr(i, 5, 6), substr(i, 7, 8), sep = "-")
      # Read csv
      read.csv(i, header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE, na.strings = c("", "NA")) %>% 
        # Add date column
        mutate(stan_rekordu_na = as.Date(i_name))
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
  select(-c(4:5)) %>%
  # Remove aggregated data for the whole Poland
  filter(teryt != "t00") %>%
  # Change NA to 0
  dplyr::mutate(across(everything(), ~ifelse(is.na(.), 0, .)),
                stan_rekordu_na = as.Date(stan_rekordu_na))

# Write data
write.csv(df, "Processed_data/daily_vaccination.csv")



# Web scraping data of the most recently available data on a level of vaccination rates in Poland by county

# I chose 390th line as this is where I found the data for counties. 
# It is not perfect solution as any changes will break the whole code chunk. 
# I decided to use it nevertheless as the data seems to be no longer updated after the 20th of March 2022.
page <- readLines("https://www.gov.pl/web/szczepienia-gmin/sprawdz-poziom-wyszczepienia-mieszkancow-gmin")[390]

# In this part I clean the data from the page and take only those strings that I need.
page_1 <- gsub(x = page, '"', "") 
page_1 <- sub(x = page_1, ".*w1_60_69;w1_70plus", "") 
page_1 <- sub(x = page_1, ",fileName:.*", "")
page_1 <- gsub(x  = page_1, "[\\]r[\\]n", ";")
page_1 <- gsub(x  = page_1, ",", ".")

# Do more cleaning and store the result in the tibble 
page_df <- tibble(df = unlist(str_split(page_1, pattern = ";"))) %>% 
  filter(df != ":") %>% 
  filter(!row_number() %in% c(1, last(row_number()))) 

to <- nrow(page_df)

# Function to separate the data
columns <- function (i) {
  column_i <- page_df %>% filter(row_number() %in% seq(i,to, by = 13)) 
}

# Bind columns using columns function and setting seq from 1 to 13
df <- do.call("bind_cols", lapply(1:13, columns))

# Adding the correct names for columns
colnames(df) <- c("wojewodztwo_nazwa", "powiat_nazwa", "gmina_nazwa", "proc_zaszczepieni_pelna_dawka", "przyrost_zaszczepionych_od_2021_08_01", "liczba_ludnosci", "zaszczepieni_pacjenci_jedna_dawka", "zaszczepieni_pelna_dawka", "grupa_wiekowa_12_19", "grupa_wiekowa_20_39", "grupa_wiekowa_40_59", "grupa_wiekowa_60_69", "grupa_wiekowa_70_plus")

# Sorting by name and adding a date. This date was chosen according to data found on source page. 
# According to it the data is no longer updated after the 20th of March 2022.
df %<>% arrange(wojewodztwo_nazwa, powiat_nazwa, gmina_nazwa) %>% mutate(stan_rekordu_na = as.Date("2022-03-20"))

# Transfer numerical data into integer class
df %<>% 
  dplyr::mutate(across(4:13, as.numeric))

# Write data for gminy territorial division (smaller than the county one)
write_csv(df, "Processed_data/vaccination_rates_on_2022_03_20_gminy.csv")


# Replace special letters and add prefix to cities to make a name alike to population_voivodeships names.
df %<>%
  dplyr::mutate(
    wojewodztwo_nazwa = str_replace_all(wojewodztwo_nazwa, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
    powiat_nazwa = str_replace_all(powiat_nazwa, c(Ł = "L", Ś = "S", Ż = "Z", Ć = "C")),
    powiat_nazwa = str_replace_all(powiat_nazwa, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
    powiat_nazwa = ifelse(str_to_title(powiat_nazwa) == powiat_nazwa, paste0("M.", powiat_nazwa), powiat_nazwa)
  )

# Manupulate the data frame
df %<>%
  # Change teryt column format- remove 't' before the numbers
  # mutate(teryt = str_remove(teryt, "[a-z]+")) %>%
  dplyr::group_by(wojewodztwo_nazwa, powiat_nazwa) %>% 
  # Calculate the cumulative sum for new infections
  dplyr::summarise(zaszczepieni_pelna_dawka = sum(zaszczepieni_pelna_dawka),
                   liczba_ludnosci = sum(liczba_ludnosci),
                   proc_zaszczepieni_pelna_dawka = round(zaszczepieni_pelna_dawka / liczba_ludnosci * 100, 2))

# Write data for county division (bigger than the gminy one)
write_csv(df, "Processed_data/vaccination_rates_on_2022_03_20_county.csv")

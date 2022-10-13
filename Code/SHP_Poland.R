library(tidyverse)
library(magrittr)
library(sf)
library(readxl)

# Read shapefile for Poland. I use this shapefile because it is relatively small making it fast to plot 
# while also preserving details needed for county identification. There is a limitation however- some counties names 
# have been cahnged since the publication of the SHP which will result in more difficult adjusting of the counties, cities.
# After cleaning the data there are still 4 counties missing from shp file.
SHP <- st_read("Raw_data/shapefile_raw/POL_adm2.shp") %>% 
  # Change a format of a city and county name to fit with the other data
  mutate(powiat = ifelse(ENGTYPE_2 == "City", paste0("M.", str_remove(NAME_2, " City")), str_to_lower(str_remove(VARNAME_2, "Powiat ")))) %>%
  rename(county = powiat) %>% 
  select(county, geometry, ENGTYPE_2, NAME_1) %>% 
  arrange(county) %>% 
  # Replace special letters and add a starting letter to use it in the future to detect where the two data frames have 
  # differences
  mutate(starting_letter = str_sub(county, 1, 1), 
         county = str_replace_all(county, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z"))) 

# Load administrative teritory data
voivodeships_pl <- read.csv("Raw_data/Voivodeship_names.csv", sep = ";") %>%
  # Get only voivodeships names and corresponding number
  filter(NAZWA_DOD == "województwo") %>% 
  select(WOJ, NAZWA) %>%
  mutate(NAZWA = tolower(NAZWA)) %>% 
  rename(code = WOJ, voivodeship = NAZWA)

# Apply a relevant code to english voivodeship names
voivodeships_eng <- data.frame(voivodeships_eng = SHP$NAME_1 %>% unique(),
                               code = c(4, 20, 28, 10, 24, 6, 14, 32, 18, 12, 2, 16, 26, 22, 30, 8))

# Join the polish and english voivodeship names. Check if the names are correct
voivodeships <- voivodeships_eng %>%
  full_join(voivodeships_pl, by = c("code"))

# Load recent population data downloaded from
# https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/ludnosc-stan-i-struktura-ludnosci-oraz-ruch-naturalny-w-przekroju-terytorialnym-stan-w-dniu-31-12-2020,6,29.html
population <- read_xls("Raw_data/Population_by_county.xls")[,c(1:3)] %>%
  set_colnames(c("county", "teryt", "population")) %>%
  filter(row_number() > 7) %>%
  arrange(county) %>%
  # Replace special letters, pull voivodeship code and add a starting letter to use it in the future to detect where the two data frames have
  # differences
  mutate(code = as.numeric(str_sub(teryt, 1, 2)),
         county = str_replace_all(county, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
         starting_letter = str_sub(county, 1, 1)) %>%
  # Remove voivodeship data
  filter(starting_letter != "W")

# Check if two data frames have equal number of counties. Data for population contains more observations- 380.
# As of 2022 the total number of counties and cities is 380. Shapefile data contains only 376 observations
nrow(SHP) == nrow(population)

# Compare the differences between population and SHP data frames. There seems to be more counties,
# or actually in this case cities, in population data frame as it contains more recent administrative data.
tibble(
  starting_letter = population$starting_letter %>% unique() %>% sort(),
  pupulation = table(population$starting_letter),
  SHP = table(SHP$starting_letter)) %>%
  tail(n = 19)

# Check which counties in SHP are different from population
population %>%
  anti_join(SHP %>% as.data.frame(), by = "county")

# Check which counties in population are different from SHP
SHP %>% as.data.frame() %>%
  anti_join(population, by = "county")

# The counties were not joined by name because of some minor mistakes or county to city transformation.
# County names listed in test_2 are thus changed to the correct one, used in population data frame
SHP %<>%
  mutate(county = case_when(county == "M.Warsaw" ~ "M.Warszawa",
                            county == "M.Zielona" ~ "M.Zielona Gora",
                            county == "M.Ople" ~ "M.Opole",
                            county == "lipski|lipski" ~ "lipski",
                            county == "jeleniogorski" ~ "karkonoski",
                            TRUE ~ county))

# Check for the differences again. There are still a county and cities missing from population
population %>%
  anti_join(SHP %>% as.data.frame(), by = "county")

# There is no county left missing in the SHP  
SHP %>% as.data.frame() %>% 
  anti_join(population, by = "county")

# Join two data frames
SHP_county <- SHP %>% 
  full_join(voivodeships, by = c("NAME_1" = "voivodeships_eng")) %>% 
  select(county, voivodeship, code, geometry) %>%
  left_join(population, by = c("county", "code")) %>%
  select(-starting_letter) %>%
  mutate(county = str_replace_all(county, c(Ł = "L", Ś = "S", Ż = "Z", Ć = "C")),
         voivodeship = str_replace_all(voivodeship, c(Ł = "L", Ś = "S", Ż = "Z", Ć = "C")),
         voivodeship = str_replace_all(voivodeship, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z"))
         )

# Write a county shapefile
st_write(SHP_county, "Processed_data/Shapefiles/SHP_county.shp", driver = "ESRI Shapefile", append = FALSE)


# Make a geometry data for voivodeships in Poland from county geometry
SHP_voivodeship <- SHP_county %>% 
  group_by(voivodeship) %>% 
  split(group_indices(.)) %>% 
  # Unite geometries belonging to voivodeships
  purrr::map_df(st_union) %>% 
  ungroup() %>% 
  # Change dimensions of the resulting data frame 
  pivot_longer(cols = 1:16, values_to = "geometry") %>% 
  # Add voivodeships names back
  mutate(voivodeship = SHP_county$voivodeship %>% unique()) %>%
  select(-name) %>% 
  st_as_sf()

# Write a voivodeship shapefile
st_write(SHP_voivodeship, "Processed_data/Shapefiles/SHP_voivodeship.shp", driver = "ESRI Shapefile", append = FALSE)

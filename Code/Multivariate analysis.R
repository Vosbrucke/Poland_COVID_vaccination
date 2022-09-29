# Multivariate analysis
library(maptools)
library(tidyverse)
library(knitr)
library(broom)
library(kableExtra)


df_vaccination <- read_csv("Processed_data/Percentage_of_people_fully_vaccinated.csv") %>% 
  rename(Kod = teryt) %>% 
  mutate(Kod = paste0(Kod, "000"),
         Kod = as.integer(Kod))

df_elections <- read.csv("Raw_data/wyniki_gl_na_listy_po_powiatach_proc_sejm.csv", sep = ";", dec = ",") %>% 
  rename(Kod = Kod.TERYT) %>% 
  mutate(Kod = paste0(Kod, "0"),
         Kod = as.integer(Kod))

df_education <- read.csv("Raw_data/Employed_persons_by_level_of_education.csv", sep = ";")

df_events <- read.csv("Raw_data/Events_organized_by_the_entity_and_participants.csv", sep = ";")

df_retirees <- read.csv("Raw_data/Share_of_population_by_economic_age_groups_in_%_of_total_population.csv", sep = ";", dec = ",")

df_density <- read.csv("Raw_data/Density_of_population.csv", sep = ";")

df_doctors <- read.csv("Raw_data/Doctors.csv", sep = ";", dec = ",")


population <- readxl::read_xls("/Users/mr.fox/Desktop/Github/COVID_wg_powiatow/Raw_data/Population_by_county.xls")[,c(1:3)] %>%
  set_colnames(c("powiat", "Kod", "ludnosc")) %>%
  filter(row_number() > 7) %>%
  arrange(powiat) %>% 
  # Replace special letters, pull voivodeship code and add a starting letter to use it in the future to detect where the two data frames have 
  # differences
  mutate(WOJ = as.numeric(str_sub(Kod, 1, 2)), 
         # powiat = str_replace_all(powiat, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o")),
         starting_letter = str_sub(powiat, 1, 1),
         Kod = paste0(Kod, "000"),
         Kod = as.integer(Kod)) %>% 
  # Remove voivodeship data
  filter(starting_letter != "W") %>% 
  select(-starting_letter) 

df_all <- population %>% 
  left_join(df_events, by = "Kod") %>% 
  left_join(df_retirees, by = "Kod") %>% 
  left_join(df_doctors, by = "Kod") %>% 
  left_join(df_density, by = "Kod") %>% 
  left_join(df_vaccination, by = "Kod") %>% 
  left_join(df_elections, by = "Kod") %>% 
  select(powiat, Kod, ludnosc, 6, 9:11, 14, 17, 21:24, 29:43)
  



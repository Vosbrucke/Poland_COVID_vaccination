# Code modelling
library(magrittr)
library(tidyverse)

# Read vaccination data
df_vaccination <- read_csv("Processed_data/Percentage_of_people_fully_vaccinated.csv") %>% 
  rename(Kod = teryt) %>% 
  mutate(Kod = paste0(Kod, "000"),
         Kod = as.integer(Kod))

# Read elections data
df_elections <- read.csv("Raw_data/wyniki_gl_na_listy_po_powiatach_proc_sejm.csv", sep = ";", dec = ",") %>% 
  rename(Kod = Kod.TERYT) %>% 
  mutate(Kod = paste0(Kod, "0")) %>% 
  # Change the Warsaw code to fit the other ones. Anti join with population showed that the code for Warsaw City in df_elections
  # dataset was 1465010 and ion population 1465000. 
  mutate(Kod = ifelse(Powiat == "Warszawa", "1465000", Kod),
         Kod = as.integer(Kod))

# Read data on economic groups
df_economic_groups <- read.csv("Raw_data/Share_of_population_by_economic_age_groups_in_%_of_total_population.csv", sep = ";", dec = ",")

# Read data on population density
df_density <- read.csv("Raw_data/Density_of_population.csv", sep = ";")

# Read data on number of doctors and medical staff
df_doctors <- read.csv("Raw_data/Doctors.csv", sep = ";", dec = ",")

# Read feminine index data
df_feminine <- read.csv("Raw_data/Feminine_index.csv", sep = ";", dec = ",")[,1:3]

# Read data on education share. This data is a part of 2011 census. 2021 census for education groups per county
# is still not ready. Once it is available I will change the data source
df_education <- read.csv("Raw_data/Education_2011.csv", sep = ";", dec = ",") %>% 
  pivot_wider(names_from = Poziom.wykształcenia, values_from = Liczba.osób) %>% 
  group_by(Nazwa) %>% 
  # Adjust county names to fit the other data sets. The territorial code columns are formatted in a different fashion to 
  # current way of presenting their code. That is why I decided to standardize using county name instead
  mutate(
    Nazwa = ifelse(str_detect(Nazwa, "M. "), str_replace(Nazwa, "M. ", "m."), Nazwa),
    Suma = sum(across(c(wyższe, średnie, "zasadnicze zawodowe", podstawowe)))) %>% 
  mutate(Proc = across(c(wyższe, średnie, "zasadnicze zawodowe", podstawowe), .fns = function(i) {i / Suma})) %>% 
  select(Województwo, Nazwa, contains("Proc")) %>% 
  ungroup() %>% 
  unnest(Proc) %>% 
  as.data.frame() %>% 
  set_colnames(c("Województwo", "Nazwa", "wyższe", "średnie", "zasadnicze_zawodowe", "podstawowe"))

# Read data on health expenditure
df_health <- read.csv("Raw_data/Expenditure_on_health.csv", sep = ";", dec = ",")[,1:3] %>% 
  set_colnames(c("Kod", "Nazwa", "Health_exp"))

# Read data on population by county
population <- readxl::read_xls("Raw_data/Population_by_county.xls")[,c(1:3)] %>%
  set_colnames(c("county", "Kod", "population")) %>%
  filter(row_number() > 7) %>%
  arrange(county) %>% 
  # Replace special letters, pull voivodeship code and add a starting letter to use it in the future to detect where the two data frames have 
  # differences
  mutate(WOJ = as.numeric(str_sub(Kod, 1, 2)), 
         starting_letter = str_sub(county, 1, 1),
         Kod = paste0(Kod, "000"),
         Kod = as.integer(Kod),
         population = as.integer(population)) %>% 
  # Remove voivodeship data
  filter(starting_letter != "W") %>% 
  select(-starting_letter) 

# Join all the data sets together
df_all <- population %>% 
  left_join(df_feminine, by = "Kod") %>%
  left_join(df_economic_groups, by = "Kod") %>% 
  left_join(df_doctors, by = "Kod") %>% 
  left_join(df_density, by = "Kod") %>% 
  left_join(df_vaccination, by = "Kod") %>% 
  left_join(df_elections, by = "Kod") %>% 
  left_join(df_education, by = c("Nazwa.x" = "Nazwa", "WOJ.x" = "Województwo")) %>% 
  left_join(df_health, by = "Kod") %>% 
  select(county, Kod, population, where(is.numeric)) %>% 
  mutate(Health_exp_per_person = Health_exp / population) %>% 
  # Replace NA's to '0'
  dplyr::mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
  # Rename the column names
  dplyr::rename("Fully_vaccinated_population" = proc_zaszczepieni_pelna_dawka, 
                "Feminization_index" = ogółem.2021..osoba.,
                "Working_age_population_share" = w.wieku.produkcyjnym.2021....,
                "Doctors_and_staff_per_10_thousand_people_in_2020" = lekarze..personel.pracujący.ogółem..na.10.tys..ludności.2020..osoba.,
                "Health_expenditure_per_1_person" = Health_exp_per_person,
                "Population_per_1_square_km_in_2021" = ludność.na.1.km2.2021..osoba.,
                "Higher_education_population_share_in_2011" = wyższe,
                "Votes_on_PiS_commission" = KOMITET.WYBORCZY.PRAWO.I.SPRAWIEDLIWOŚĆ...ZPOW.601.9.19)

# Write joined data frame
write_csv(df_all, "Processed_data/regression_analysis.csv")

library(plm)
library(splm)
library(spdep)
library(maptools)
library(spatialreg)
library(tidyverse)
library(knitr)
library(broom)
library(kableExtra)


df_vaccination <- read_csv("/Users/mr.fox/Desktop/Github/COVID_wg_powiatow/Processed_data/Percentage_of_people_fully_vaccinated.csv")

df_elections <- read.csv("/Users/mr.fox/Desktop/Github/COVID_wg_powiatow/Raw_data/wyniki_gl_na_listy_po_powiatach_proc_sejm.csv", sep = ";", dec = ",")[, c(1:4, 10, 12, 13, 15, 17)] %>% 
  set_colnames(c("Kod.TERYT", "Powiat", "Województwo", "Frekwencja", "KOMITET_OP_KO", "KOMITET_Konfederacja", "KOMITET_OP_PSL", "KOMITET_PiS", "KOMITET_OP_SLD"))

df_elections %<>% 
  mutate(
         Kod.TERYT = as.character(Kod.TERYT),
         Kod.TERYT = ifelse(str_length(Kod.TERYT) == 5, paste0("0", Kod.TERYT), Kod.TERYT),
         Kod.TERYT = str_sub(Kod.TERYT, 1, 4)
         # Kod.TERYT = ifelse(Powiat == "Warszawa", "1465", Kod.TERYT)
         )

df_elections_vaccination <- df_elections %>% 
  inner_join(df_vaccination, by = c("Kod.TERYT" = "teryt"))


rownanie = proc_zaszczepieni_pelna_dawka ~ 
  KOMITET_PiS
# I(KOMITET_OP_SLD +
# KOMITET_OP_PSL +
# KOMITET_OP_KO)
# KOMITET_Konfederacja

reg1 <- lm(rownanie, data = df_elections_vaccination)

df_elections_vaccination %$%
cor(proc_zaszczepieni_pelna_dawka, KOMITET_PiS)

df_elections_vaccination %>% 
  select(proc_zaszczepieni_pelna_dawka, KOMITET_PiS) %>% 
  rename("Fully vaccinated population (%)" = proc_zaszczepieni_pelna_dawka, "Votes on PiS commission (%)" = KOMITET_PiS) %>% 
  cor() %>% 
  kbl(digits = 4, format = "pipe") %>% 
  kable_material(c("striped", "hover"))



df_elections_vaccination %>% 
  ggplot(aes(proc_zaszczepieni_pelna_dawka, KOMITET_PiS)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(limits = c(20, 80)) +
  scale_y_continuous(limits = c(20, 80)) +
  labs(
       x = "Fully vaccinated population (%)", 
       y = "Votes on PiS commission (%)", 
       title = "Correlation between votes cast on PiS party and vaccinated population",
       subtitle = "Based on 2019 Sejm and Senat elections and vaccination rate on 19 May 2022",
       caption = "pkw.gov.pl, gov.pl"
       ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 6),
    plot.background = element_rect(fill = "white", color = "white")
    ) 

ggsave("Plots/Correlation_between_votes_cast_on_PiS_party_and_vaccinated_population.png", dpi = 900, width = 15, height = 15, units = "cm")

df_elections_vaccination %<>% 
  group_by(Kod.TERYT) %>% 
  mutate(opposition = sum(across(contains("KOMITET_OP")))) %>% 
  ungroup()


rownanie = proc_zaszczepieni_pelna_dawka ~ 
  # KOMITET_PiS
I(KOMITET_OP_SLD +
KOMITET_OP_PSL +
KOMITET_OP_KO)
# KOMITET_Konfederacja

reg1 <- lm(rownanie, data = df_elections_vaccination)

df_elections_vaccination %$%
  cor(proc_zaszczepieni_pelna_dawka, opposition)

df_elections_vaccination %>% 
  select(proc_zaszczepieni_pelna_dawka, opposition) %>% 
  rename("Fully vaccinated population (%)" = proc_zaszczepieni_pelna_dawka, "Votes on opposition parties commissions (%)" = opposition) %>% 
  cor() %>% 
  kbl(digits = 4, format = "pipe") %>% 
  kable_material(c("striped", "hover"))



df_elections_vaccination %>% 
  ggplot(aes(proc_zaszczepieni_pelna_dawka, opposition)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(limits = c(20, 80)) +
  scale_y_continuous(limits = c(20, 80)) +
  labs(
    x = "Fully vaccinated population (%)", 
    y = "Votes on opposition parties commissions (%)", 
    title = "Correlation between votes cast on opposition parties and vaccinated population",
    subtitle = "Based on 2019 Sejm and Senat elections and vaccination rate on 19 May 2022",
    caption = "pkw.gov.pl, gov.pl"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 6),
    plot.background = element_rect(fill = "white", color = "white")
  ) 

ggsave("Plots/Correlation_between_votes_cast_on_opposition_parties_and_vaccinated_population.png", dpi = 900, width = 15, height = 15, units = "cm")



population <- readxl::read_xls("/Users/mr.fox/Desktop/Github/COVID_wg_powiatow/Raw_data/Population_by_county.xls")[,c(1:3)] %>%
  set_colnames(c("powiat", "teryt", "ludnosc")) %>%
  filter(row_number() > 7) %>%
  arrange(powiat) %>% 
  # Replace special letters, pull voivodeship code and add a starting letter to use it in the future to detect where the two data frames have 
  # differences
  mutate(WOJ = as.numeric(str_sub(teryt, 1, 2)), 
         powiat = str_replace_all(powiat, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o")),
         starting_letter = str_sub(powiat, 1, 1)) %>% 
  # Remove voivodeship data
  filter(starting_letter != "W") 


# Load data on COVID-19 infections
df_infections <- read_csv("/Users/mr.fox/Desktop/Github/COVID_wg_powiatow/Processed_data/daily_infections.csv")[,-1]

# Manupulate the data frame
df_infections <- df_infections %>%
  # Change teryt column format- remove 't' before the numbers 
  mutate(teryt = str_remove(teryt, "[a-z]+")) %>%
  group_by(teryt) %>% 
  # Calculate the cumulative sum for new infections
  mutate(cumsum = max(cumsum(liczba_nowych_zakazen))) %>% 
  filter(row_number() == 1)

# Join the data frame with population
df_infections %<>%
  left_join(population, by = "teryt") %>%
  filter(voivodeship != "Cały kraj") %>%
  mutate(ludnosc = as.numeric(ludnosc),
         cumsum = cumsum / ludnosc * 100) %>% 
  select(voivodeship, WOJ, county, teryt, cumsum, liczba_nowych_zakazen, ludnosc, zgony, zgony_w_wyniku_covid_bez_chorob_wspolistniejacych, stan_rekordu_na)


# Load data on COVID-19 infections. Select only a few of the columns
df_elections <- read.csv("/Users/mr.fox/Desktop/Github/COVID_wg_powiatow/Raw_data/wyniki_gl_na_listy_po_powiatach_proc_sejm.csv", sep = ";", dec = ",")[, c(1:4, 10, 12, 13, 15, 17)] %>% 
  set_colnames(c("Kod.TERYT", "Powiat", "Województwo", "Frekwencja", "KOMITET_OP_KO", "KOMITET_Konfederacja", "KOMITET_OP_PSL", "KOMITET_PiS", "KOMITET_OP_SLD"))

# Replace special letters and add prefix to cities to make a name alike to population_voivodeships names.
df_elections %<>%
  mutate(
    Powiat = str_replace_all(Powiat, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
    Powiat = ifelse(str_to_title(Powiat) == Powiat, paste0("M.", Powiat), Powiat)
  )

# Join df with population voivodeship data in order to properly plot problematic counties. There are some counties 
# that share the same name. It disrupts plotting showing outliers in places where there is no such case
df_elections %<>%
  right_join(population_voivodeships, by = c("Powiat" = "powiat", "Województwo" = "NAZWA")) %>% 
  drop_na()

# # Calculating who won the elections by county
# df_1 <- df %>% 
#   group_by(Kod.TERYT) %>% 
#   mutate(oposition = sum(across(contains("KOMITET_OP")))) %>% 
#   mutate(max_votes = max(across(contains("KOMITET"))),
#          max_votes = max(across(.cols = c("max_votes", "oposition")))) %>% 
#   mutate(who_won = case_when(
#     KOMITET_OP_KO == max_votes ~ "KO",
#     KOMITET_Konfederacja == max_votes ~ "Konfederacja",
#     KOMITET_OP_PSL == max_votes ~ "PSL",
#     KOMITET_PiS == max_votes ~ "PiS",
#     KOMITET_OP_SLD == max_votes ~ "SLD",
#     oposition == max_votes ~ "Opozycja",
#     TRUE ~ "None"))

df_vaccination <- read_csv("/Users/mr.fox/Desktop/Github/COVID_wg_powiatow/Processed_data/Percentage_of_people_fully_vaccinated.csv")

shp <- st_read("/Users/mr.fox/Desktop/Github/COVID_wg_powiatow/Processed_data/SHP_county.shp")

df <- df_infections %>% 
  left_join(df_elections %>% select(-c(WOJ, starting_letter)), by = c("teryt")) %>%
  drop_na() %>% 
  left_join(df_vaccination, by = "teryt")

df %<>%
  right_join(shp %>% as.data.frame() %>% select(teryt), by = "teryt") %>% 
  ungroup()

library(tseries)
jarque.bera.test(df$cumsum)
# H0: the data is normally distributed
# HA: the data is not normally distributed

# The data is normally distributed. Null hipothesis is not rejected

panel <- pdata.frame(df, c("teryt" , "stan_rekordu_na"))

shp <- st_read("Processed_data/Shapefiles/SHP_county.shp")

coordinates = st_coordinates(shp)

w <- poly2nb(shp, row.names=shp$Kod, queen= F)

wm <- nb2mat(w, style='B')

sasiad_q1 = poly2nb (shp , queen = T)

macierz_q1 = nb2listw (sasiad_q1 , style = "W" , zero.policy = TRUE)

moran(df$proc_zaszczepieni_pelna_dawka, macierz_q1, n=length(macierz_q1$neighbours), S0=Szero(macierz_q1))

moran.test(df$KOMITET_PiS, macierz_q1, randomisation=FALSE)

moran.plot(df$proc_zaszczepieni_pelna_dawka, macierz_q1)

# H0: There is no spacial dependency
# H1: There is a spacial dependency

rownanie = proc_zaszczepieni_pelna_dawka ~ 
  KOMITET_PiS
  # I(KOMITET_OP_SLD +
  # KOMITET_OP_PSL +
  # KOMITET_OP_KO)
  # KOMITET_Konfederacja

reg1 <- lm(rownanie, data = df)
summary(reg1)
kable(
broom::glance(summary(reg1)), "simple"
)

kable(
  broom::tidy(summary(reg1)), "simple"
)

lmMoranTest <- lm.morantest(reg1, macierz_q1)

lmMoranTest

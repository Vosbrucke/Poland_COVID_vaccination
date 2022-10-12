# Multivariate analysis
library(maptools)
library(tidyverse)
library(knitr)
library(broom)
library(broom.mixed)
library(kableExtra)
library(magrittr)
library(ggtext)
library(rgdal)
library(rgeos)
library(glue)

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


# Linear regression analysis using all variables

# Prepare a formula
formula <- as.formula(Fully_vaccinated_population ~
  Feminization_index +
  Votes_on_PiS_commission +
  Working_age_population_share +
  Doctors_and_staff_per_10_thousand_people_in_2020 +
  Population_per_1_square_km_in_2021 +
  Higher_education_population_share_in_2011 +
  Health_expenditure_per_1_person)

# Run linear regression model
reg1 <- lm(formula, data = df_all)

# Check summary of the lm 
summary(reg1)

# Prepare a table in Github friendly format. Show the estimates of the model
tidy(reg1) %>% 
  arrange(desc(estimate)) %>% 
  kbl(digits = 2, format = "pipe") %>% 
  kable_material(c("striped", "hover"))

# Prepare a table in Github friendly format. Show the statistics summary 
glance(reg1) %>% 
  kbl(digits = 2, format = "pipe") %>% 
  kable_material(c("striped", "hover"))

# Decide which variable is insignificant
tidy(reg1) %>% 
  arrange(desc(p.value)) %>% 
  filter(p.value > 0.05)


# Making a stepwise reduction

# 1st removal of the most insignificant variable
formula1 <- as.formula(Fully_vaccinated_population ~
                        Feminization_index +
                        Votes_on_PiS_commission +
                        Working_age_population_share +
                        Doctors_and_staff_per_10_thousand_people_in_2020 +
                        # Population_per_1_square_km_in_2021 +
                        Higher_education_population_share_in_2011 +
                        Health_expenditure_per_1_person)

# Run linear regression model
reg2 <- lm(formula1, data = df_all)

# Check the summary of lm
summary(reg2)

# Decide which variable is insignificant
tidy(reg2) %>% 
  arrange(desc(p.value)) %>% 
  filter(p.value > 0.05)

# 2nd removal of the most insignificant variable
formula2 <- as.formula(Fully_vaccinated_population ~
                        Feminization_index +
                        Votes_on_PiS_commission +
                        Working_age_population_share +
                        Doctors_and_staff_per_10_thousand_people_in_2020 +
                        # Population_per_1_square_km_in_2021 +
                        Higher_education_population_share_in_2011)
                        # Health_expenditure_per_1_person)

# Run linear regression model
reg3 <- lm(formula2, data = df_all)

# Check the summary of lm
summary(reg3)

# Decide which variable is insignificant
tidy(reg3) %>% 
  arrange(desc(p.value)) %>% 
  filter(p.value > 0.05)

# 3rd removal of the most insignificant variable
formula3 <- as.formula(Fully_vaccinated_population ~
                        Feminization_index +
                        Votes_on_PiS_commission +
                        Working_age_population_share +
                        # Doctors_and_staff_per_10_thousand_people_in_2020 +
                        # Population_per_1_square_km_in_2021 +
                        Higher_education_population_share_in_2011)
                        # Health_expenditure_per_1_person)

# Run linear regression model
reg4 <- lm(formula3, data = df_all)

# Check the summary of lm
summary(reg4)

# Decide which variable is insignificant
tidy(reg4) %>% 
  arrange(desc(p.value)) %>% 
  filter(p.value > 0.05)

# No insignificant variables were found

# Test heteroscedasticity of the model residuals
bptest(reg4)

# We reject the null hypothesis and conclude that this regression model does not violate the homoscedasticity assumption
# The residuals are homoscedastic (p value > 0.05)


# Prepare a table in Github friendly format. Show the estimates of the model
tidy(reg4) %>% 
  arrange(desc(estimate)) %>% 
  kbl(digits = 2, format = "pipe") %>% 
  kable_material(c("striped", "hover"))

# Prepare a table in Github friendly format. Show the statistics summary 
glance(reg4) %>% 
  kbl(digits = 2, format = "pipe") %>% 
  kable_material(c("striped", "hover"))

# Standard deviation of residuals calculation
sqrt(glance(reg4)$sigma)

# Coefficient of variability caluclation
glance(reg4)$sigma / mean(df_all$Fully_vaccinated_population) * 100


# Spatial dependency on the residuals from lm model
df_residuals <- df %>%
  mutate(predicted_values = predict(reg4),
         residuals = Fully_vaccinated_population - predicted_values) %>% 
  select(1, 2, WOJ.x, residuals)

# Replace special letters and add prefix to cities to make a name alike to population_voivodeships names.
df_residuals %<>%
  dplyr::mutate(
    # wojewodztwo_nazwa = str_replace_all(wojewodztwo_nazwa, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
    county = str_replace_all(county, c(Ł = "L", Ś = "S", Ż = "Z", Ć = "C")),
    county = str_replace_all(county, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z"))
    # county = ifelse(str_to_title(county) == county, paste0("M.", county), county)
  )

# Write csv
write_csv(df_residuals, "Processed_data/linear_regression_prediction_OLS.csv")

# Make a color palette
palette <- wesanderson::wes_palette("Zissou1", 5)[c(1,5)]

# Make a plot for the actual and modelled vaccination rate using OLS model
augment(reg4) %>% 
  ggplot(aes(x = Fully_vaccinated_population)) +
  geom_point(aes(y = .fitted), color = "white") +
  geom_point(aes(y = .fitted), color = "#F21A00", alpha = 0.5) +
  geom_smooth(aes(y = .fitted), method = "lm", se = F, color = "#F21A00", size = 0.3) +
  geom_point(aes(y = Fully_vaccinated_population), color = "white") +
  geom_point(aes(y = Fully_vaccinated_population), color = "#3B9AB2", alpha = 0.5) +
  geom_line(aes(y = Fully_vaccinated_population), color = "#3B9AB2", size = 0.3) +
  coord_cartesian(xlim = c(30, 80), ylim = c(30, 80)) +
  scale_x_continuous(breaks = seq(30,80,10), labels = paste0(seq(30,80,10), "%")) +
  scale_y_continuous(breaks = seq(30,80,10), labels = paste0(seq(30,80,10), "%")) +
  labs(
    x = "",
    y = "",
    title = "The <span style = 'color:#3B9AB2'>actual</span> and <span style = 'color:#F21A00'>modelled</span> vaccination rate using <i>OLS</i>",
    subtitle = glue("Coefficient of determination (R squared) = {round(glance(reg4)$r.squared * 100, 0)}%"),
    caption = "Source: pkw.gov.pl, gov.pl"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_markdown(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = "white"))


ggsave("Plots/The_actual_and_modelled_vaccination_rate_OLS.png", dpi = 900, width = 15, height = 15, units = "cm")

# Load data on spatial distribution of SEM predictions
distribution_prediction <- read_csv("Processed_data/spatial_ditribution_prediction_SEM.csv")

# Make a plot for the actual and modelled vaccination rate using Spatial Error Model
distribution_prediction %>% 
  ggplot(aes(x = Fully_vaccinated_population)) +
  geom_point(aes(y = predicted_values_SEM), color = "white") +
  geom_point(aes(y = predicted_values_SEM), color = "#F21A00", alpha = 0.5) +
  geom_smooth(aes(y = predicted_values_SEM), method = "lm", se = F, color = "#F21A00", size = 0.3) +
  geom_point(aes(y = Fully_vaccinated_population), color = "white") +
  geom_point(aes(y = Fully_vaccinated_population), color = "#3B9AB2", alpha = 0.5) +
  geom_line(aes(y = Fully_vaccinated_population), color = "#3B9AB2", size = 0.3) +
  coord_cartesian(xlim = c(30, 80), ylim = c(30, 80)) +
  scale_x_continuous(breaks = seq(30,80,10), labels = paste0(seq(30,80,10), "%")) +
  scale_y_continuous(breaks = seq(30,80,10), labels = paste0(seq(30,80,10), "%")) +
  labs(
    x = "",
    y = "",
    title = "The <span style = 'color:#3B9AB2'>actual</span> and <span style = 'color:#F21A00'>modelled</span> vaccination rate using <i>SEM</i>",
    subtitle = glue("Coefficient of determination (R squared) = 76%"),
    caption = "Source: pkw.gov.pl, gov.pl"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_markdown(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = "white"))

ggsave("Plots/The_actual_and_modelled_vaccination_rate_SEM.png", dpi = 900, width = 15, height = 15, units = "cm")


# Correlation plot with all the variables
df_all %>%
  select(
      Fully_vaccinated_population, 
      Feminization_index,
      Votes_on_PiS_commission, 
      Working_age_population_share,
      Doctors_and_staff_per_10_thousand_people_in_2020,
      Population_per_1_square_km_in_2021,
      Higher_education_population_share_in_2011, 
      Health_expenditure_per_1_person
      ) %>% 
  # Rename the columns by removing '_' 
  dplyr::rename_with(~ gsub("_", " ", .x)) %>% 
  cor() %>% 
  ggcorrplot(colors = c("#F21A00", "white", "#3B9AB2"), outline = "white", legend.title = "Correlation")

ggsave("Plots/Correlation_plot_multivariate.png", dpi = 900, width = 20, height = 20, units = "cm", bg = "white")


# Prepare a data frame for two variables in univariate analysis
two_selected <- df %>% 
  select(Fully_vaccinated_population, Votes_on_PiS_commission) %>% 
  pivot_longer(cols = everything(), values_to = "Data", names_to = "Name") %>% 
  mutate(Name = as.factor(Name))

# Prepare a color palette
palette <- wesanderson::wes_palette("Darjeeling1")[c(2,4)]

# Make a boxplot for vaccination rates and votes on PiS commission
ggplot(two_selected, aes(x = Name, y = Data)) +
  geom_boxplot(aes(color = Name)) +
  scale_x_discrete(labels = c("Fully vaccinated population", "Votes on PiS commission")) +
  scale_y_continuous(breaks = seq(20, 80, 10), labels = paste0(seq(20, 80, 10), "%")) +
  scale_color_manual(values = palette) +
  labs(
    x = NULL,
    y = NULL,
    title = glue("Boxplot of the <span style = 'color:{palette[1]}'>**vaccination rates**</span> and <span style = 'color:{palette[2]}'>**votes on PiS commission**</span> in 2019 Sejm and Senat elections<br>across counties in Poland"),
    subtitle = "Data as of 19 May, 2022",
    caption = "Source: pkw.gov.pl, gov.pl"
  ) +
  theme_minimal() +
  theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.position = "none",
        plot.title = element_markdown(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8),
        plot.background = element_rect(fill = "white", color = "white"))

ggsave("Plots/Boxplot_univariate_analysis.png", dpi = 900, width = 23, height = 15, units = "cm")

# Make a violin plot for vaccination rates and votes on PiS commission
ggplot(two_selected, aes(x = Name, y = Data)) +
  geom_violin(aes(color = Name), draw_quantiles = c(0.25, 0.75)) +
  scale_x_discrete(labels = c("Fully vaccinated population", "Votes on PiS commission")) +
  scale_y_continuous(breaks = seq(20, 80, 10), labels = paste0(seq(20, 80, 10), "%"), limits = c(20, 80)) +
  scale_color_manual(values = palette) +
  labs(
    x = NULL,
    y = NULL,
    title = glue("Density distribution of the <span style = 'color:{palette[1]}'>**vaccination rates**</span> and <span style = 'color:{palette[2]}'>**votes on PiS commission**</span> in 2019 Sejm and Senat elections across counties in Poland"),
    subtitle = "Data as of 19 May, 2022",
    caption = "Source: pkw.gov.pl, gov.pl"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.position = "none",
    plot.title = element_markdown(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = "white"))

# Make a density plot for vaccination rates and votes on PiS commission
ggplot(two_selected, aes(x = Data)) +
  geom_density(aes(color = Name), size = 1) +
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(breaks = seq(20, 80, 10), labels = paste0(seq(20, 80, 10), "%"), limits = c(15, 85)) +
  scale_color_manual(values = palette) +
  labs(
    x = NULL,
    y = NULL,
    title = glue("Density distribution of the <span style = 'color:{palette[1]}'>**vaccination rates**</span> and <span style = 'color:{palette[2]}'>**votes on PiS commission**</span> in 2019 Sejm and Senat elections<br>across counties in Poland"),
    subtitle = "Data as of 19 May, 2022",
    caption = "Source: pkw.gov.pl, gov.pl"
  ) +
  coord_cartesian(ylim = c(0, 0.075), expand = FALSE) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.position = "none",
    plot.title = element_markdown(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = "white"))

ggsave("Plots/Density_plot_univariate_analysis.png", dpi = 900, width = 23, height = 15, units = "cm")

ggplot(two_selected, aes(x = Data)) +
  geom_histogram(aes(color = Name), fill = "white") +
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(breaks = seq(20, 80, 10), labels = paste0(seq(20, 80, 10), "%"), limits = c(20, 80)) +
  scale_color_manual(values = palette) +
  labs(
    x = NULL,
    y = "Count",
    title = glue("Histogram for the <span style = 'color:{palette[1]}'>**vaccination rates**</span> and <span style = 'color:{palette[2]}'>**votes on PiS commission**</span> in 2019 Sejm and Senat elections<br>across counties in Poland"),
    subtitle = "Data as of 19 May, 2022",
    caption = "Source: pkw.gov.pl, gov.pl"
  ) +
  coord_cartesian(ylim = c(0, 65), xlim = c(18, 82), expand = FALSE) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.position = "none",
    strip.text = element_blank(),
    plot.title = element_markdown(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = "white")) +
  facet_wrap(~Name, ncol = 1)

ggsave("Plots/Histogram_univariate_analysis.png", dpi = 900, width = 23, height = 15, units = "cm")

# Create a summary
df_all %>% 
  select(Fully_vaccinated_population, Votes_on_PiS_commission) %>% 
  summary()

# Correlation result
df_elections_vaccination %$%
  cor(proc_zaszczepieni_pelna_dawka, KOMITET_PiS)

# Make a plot of correlation of vaccination rates and votes on PiS commission
df_all %>%
  ggplot(aes(Fully_vaccinated_population, Votes_on_PiS_commission)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  coord_cartesian(xlim = c(20, 80), ylim = c(20, 80)) +
  scale_x_continuous(breaks = seq(20,80,20), labels = paste0(seq(20,80,20), "%")) +
  scale_y_continuous(breaks = seq(20,80,20), labels = paste0(seq(20,80,20), "%")) +
  labs(
    x = "Fully vaccinated population", 
    y = "Votes on PiS commission", 
    title = "Correlation between votes cast on PiS party and vaccinated population",
    subtitle = "Based on 2019 Sejm and Senat elections and vaccination rate on May 19, 2022",
    caption = "pkw.gov.pl, gov.pl"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = "white")
  ) 

ggsave("Plots/Correlation_between_votes_cast_on_PiS_party_and_vaccinated_population.png", dpi = 900, width = 15, height = 15, units = "cm")

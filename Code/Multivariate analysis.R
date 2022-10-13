# Multivariate analysis
library(tidyverse)
library(broom)
library(ggcorrplot)
library(kableExtra)
library(magrittr)
library(ggtext)
library(lmtest)
library(wesanderson)
library(glue)

# Linear regression analysis using all variables
df_all <- read_csv("Processed_data/regression_analysis.csv")

# Multivariate analysis
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


# Prepare a formula for linear regression
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
df_residuals <- df_all %>%
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
palette <- wes_palette("Zissou1", 5)[c(1,5)]

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


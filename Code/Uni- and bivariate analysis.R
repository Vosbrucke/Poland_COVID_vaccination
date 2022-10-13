# Uni- and bivariate analysis
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
df <- read_csv("Processed_data/regression_analysis.csv")

# Prepare a data frame for two variables in univariate analysis
two_selected <- df %>% 
  select(Fully_vaccinated_population, Votes_on_PiS_commission) %>% 
  pivot_longer(cols = everything(), values_to = "Data", names_to = "Name") %>% 
  mutate(Name = as.factor(Name))

# Prepare a color palette
palette <- wesanderson::wes_palette("Darjeeling1")[c(2,4)]

# Boxplot for vaccination rates and votes on PiS commission
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


# Histogram for vaccination rates and votes on PiS commission
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


# Violin plot for vaccination rates and votes on PiS commission
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


# Density plot for vaccination rates and votes on PiS commission
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


# Create a summary for bivariate analysis
df %>% 
  select(Fully_vaccinated_population, Votes_on_PiS_commission) %>% 
  summary()

# Correlation result for bivariate analysis
df_elections_vaccination %$%
  cor(proc_zaszczepieni_pelna_dawka, KOMITET_PiS)

# Plot of correlation of vaccination rates and votes on PiS commission
df %>%
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
    subtitle = "Based on 2019 Sejm and Senat elections and vaccination rate by May 19, 2022",
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


# Two-variate regression analysis for only significant factors
formula_bivariate <- as.formula(Fully_vaccinated_population ~
                                     Votes_on_PiS_commission)

reg1 <- lm(formula_bivariate, data = df)

summary(reg1)

tidy(reg1) %>% 
  arrange(desc(estimate)) %>%
  kbl(digits = 2, format = "pipe") %>%
  kable_material(c("striped", "hover"))

glance(reg1) %>%
  kbl(digits = 2, format = "pipe") %>%
  kable_material(c("striped", "hover"))

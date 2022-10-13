library(tidyverse) 
library(magrittr) 
library(sf) 

# Load shapefiles for county and voivodeship
SHP_county <- st_read("Processed_data/Shapefiles/SHP_county.shp")
SHP_voivodeship <- st_read("Processed_data/Shapefiles/SHP_voivodeship.shp")

# Load data for the multivariate analysis
df_all <- read_csv("Processed_data/regression_analysis.csv")

formula3 <- as.formula(Fully_vaccinated_population ~
                         Feminization_index +
                         Votes_on_PiS_commission +
                         Working_age_population_share +
                         Higher_education_population_share_in_2011)

# Run linear regression model
reg4 <- lm(formula3, data = df_all)

df <- df_all %>%
  mutate(predicted_values = predict(reg4),
         residuals = Fully_vaccinated_population - predicted_values) %>% 
  select(1, 2, WOJ.x, residuals)

# Replace special letters and add prefix to cities to make a name alike to population_voivodeships names.
df %<>%
  dplyr::mutate(
         # wojewodztwo_nazwa = str_replace_all(wojewodztwo_nazwa, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
    county = str_replace_all(county, c(Ł = "L", Ś = "S", Ż = "Z", Ć = "C")),
    county = str_replace_all(county, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z"))
    # county = ifelse(str_to_title(county) == county, paste0("M.", county), county)
         )

# Join shapefiles with data frame
SHP_county %<>%
  select(county, geometry, code) %>%
  left_join(df, by = c("county" = "county", "code" = "WOJ.x"))

# Add a theme function. Source: Timo Gressenbacher and Angelo Zehr
theme_map <- function(...) {
  theme_minimal() +
    theme(
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#ECECE9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "white",
                                     color = "white"),
      panel.background = element_rect(fill = "white",
                                      color = "white"),
      legend.background = element_rect(fill = "white",
                                       color = "white"),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11, color = "#494E4F"),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = "#494E4F"),
      plot.title = element_markdown(size = 15, hjust = 0.5,
                                color = "#494E4F"),
      # plot.title.position = "plot",
      # plot.caption.position = "plot",
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "#494E4F",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

# Plot
p <- ggplot() +
  geom_sf(
    data = SHP_county,
    aes(fill = residuals),
    color = "white",
    size = 0.15,
  ) +
  geom_sf(
    data = SHP_voivodeship,
    aes(),
    fill = NA,
    color = "white",
    size = 0.5
  ) +
  labs(x = NULL, y = NULL,
       title = "Residuals between actual and modelled vaccination rates using <b>Ordinary Least Square Model</b>",
       subtitle = "Data as of 30 March, 2022",
       caption = "Source: gov.pl, GUS") +
  scale_fill_gradientn(
    values = c(1, 0.6, 0.5, 0.4, 0),
                       colors = c("#2d8a6e", "#98c28a", "#fff9b5", "#f0a264", "#d43d51"),
                       breaks = c(-25, 0, 25),
                       limits = c(-25, 25),
                       expand = c(0, 0),
                       name = "Residuals distribution:"
    ) +
  theme_map() +
  theme(
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.text = element_text(hjust = 0.5),
        legend.spacing = unit(0.4, "cm"),
        ) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                reverse = 0))

# Save the plot
ggsave("Plots/Residuals_actual_vs_model_distribution_using_OLS.png", dpi = 900, height = 16, width = 25, units = "cm")
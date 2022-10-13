library(tidyverse) 
library(magrittr) 
library(sf) 
library(cowplot)

# Plot inspired by: Timo Gressenbacher and Angelo Zehr

# Load shapefiles for county and voivodeship
SHP_county <- st_read("Processed_data/Shapefiles/SHP_county.shp")
SHP_voivodeship <- st_read("Processed_data/Shapefiles/SHP_voivodeship.shp")

# Load joined data with different factors
df <- read_csv("Processed_data/regression_analysis.csv")

# Join spatial data with data frame
df %<>% 
  right_join(SHP_county %>% 
               mutate(teryt = as.integer(paste0(as.integer(teryt), "000"))) %>% 
               as.data.frame, by = c("Kod" = "teryt", "WOJ.x" = "code"))

# Choose 'Votes on PiS Commission' to y axis
data_y <- df %>% 
  dplyr::select(Votes_on_PiS_commission, Kod) %>% 
  dplyr::rename(c(values_y = "Votes_on_PiS_commission", geo = "Kod"))

# Choose 'Fully vaccinated population' to x axis
data_x <- df %>% 
  dplyr::select(Fully_vaccinated_population, Kod) %>% 
  dplyr::rename(c(values_x = "Fully_vaccinated_population", geo = "Kod"))

# create 3 buckets for data on x axis
quantiles_x<- data_x %>%
  pull(values_x) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for data on y axis
quantiles_y <- data_y %>%
  pull(values_y) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create color scale that encodes two variables
# the special notation with gather is due to readability reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#2a5a5b", # high vaccination rate, high PiS votes
  "2 - 3" = "#567994",
  "1 - 3" = "#6c83b5", # low vaccination rate, high PiS votes
  "3 - 2" = "#5a9178",
  "2 - 2" = "#90b2b3", # medium vaccination rate, medium PiS votes
  "1 - 2" = "#b5c0da",
  "3 - 1" = "#73ae80", # high vaccination rate, low PiS votes
  "2 - 1" = "#b8d6be",
  "1 - 1" = "#e8e8e8" # low vaccination rate, low PiS votes
) %>%
  gather("group", "fill")

# Join data
data <- data_x %>%
  inner_join(data_y, by = "geo")

# cut into groups defined above and join fill
data <- data %>%
  mutate(
    quantiles_x = cut(
      values_x,
      breaks = quantiles_x,
      include.lowest = TRUE
    ),
    quantiles_y = cut(
      values_y,
      breaks = quantiles_y,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(quantiles_x), "-",
      as.numeric(quantiles_y)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group")


# separate the groups
bivariate_color_scale %<>%
  separate(group, into = c("values_x", "values_y"), sep = " - ") %>%
  mutate(values_x = as.integer(values_x),
         values_y = as.integer(values_y))

# For quick change in background color
background_color <- "white" # "white" or the original color "#F5F5F2"

# Add a theme function
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
      plot.background = element_rect(fill = background_color,
                                     color = NA),
      panel.background = element_rect(fill = background_color,
                                      color = NA),
      legend.background = element_rect(fill = background_color,
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11, color = "#494E4F"),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = "#494E4F"),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = "#494E4F"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
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

# Join shapefile with data 
SHP_county <- SHP_county %>% 
  mutate(
    geo = paste0(teryt, "000"),
    geo = as.integer(geo)
    ) %>% 
  select(geo, geometry) %>% 
  inner_join(data, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf()

# Plot a map
map <- ggplot(
  data = SHP_county
) +
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = "none") + # suppress legend
  geom_sf(
    aes(
      fill = fill
    ),
    color = "white",
    size = 0.15,
  ) +
  geom_sf(data = SHP_voivodeship,
          aes(),
          color = "white",
          fill = NA,
          size = 0.5,
  ) +
  scale_fill_identity() +
  labs(x = NULL,
       y = NULL,
       title = "Spatial distribution of vaccination rate and votes on PiS Committee",
       subtitle = "Data as of 19 May, 2022; 2019 elections to the Sejm and Senate of the Republic of Poland",
       caption = "Source: pkw.gov.pl, gov.pl") +
  theme_map()

# Plot a legend
legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = values_y,
      y = values_x),
    fill = "white"
  ) +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = values_x,
      y = values_y,
      fill = fill),
    alpha = 1
  ) +
  # Here I wanted to keep information about the scope of the value. 
  # Using geom_text I added information on two breaks in the quantiles
  geom_text(aes(x = c(1.5, 2.5), y = c(3.7, 3.7), label = paste0(c(round(quantiles_x[2]), round(quantiles_x[3])), "%")), size = 2, color = "#494E4F") +
  geom_text(aes(x = c(3.8, 3.8), y = c(1.5, 2.5), label = paste0(c(round(quantiles_y[2]), round(quantiles_y[3])), "%")), size = 2, color = "#494E4F") +
  scale_fill_identity() +
  labs(x = "lower → higher\nvaccination rate",
       y = "PiS committee votes share\nlower → higher") +
  scale_x_continuous(limits = c(0, 4), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 4), expand = c(0,0)) +
  theme_map() +
  theme(
    panel.border = element_blank(),
    panel.spacing = element_blank(),
    axis.title = element_text(size = 8, color = "#494E4F"),
    panel.grid.major = element_blank()
  ) +
  coord_fixed()

# Drawing two plots together on one plot
p <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, 0, 0.34, 0.34) +
  theme(plot.background = element_rect(fill = background_color, color = background_color))

ggsave("Plots/Two-factor_analysis_bivariate_plot.png", p, dpi = 900, height = 16, width = 25, units = "cm")


library(tidyverse) 
library(magrittr) 
library(sf) 

# Load shapefiles for county and voivodeship
SHP_county <- st_read("Processed_data/Shapefiles/SHP_county.shp")
SHP_voivodeship <- st_read("Processed_data/Shapefiles/SHP_voivodeship.shp")

# Load data on vote results by county. Limit columns
df <- read.csv("Raw_data/wyniki_gl_na_listy_po_powiatach_proc_sejm.csv", sep = ";", dec = ",")[, c(1:5, 10, 12, 13, 15, 17)]

# Replace special letters and add prefix to cities to make a name alike to population_voivodeships names
df %<>%
  dplyr::mutate(
    Województwo = str_replace_all(Województwo, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
    Powiat = str_replace_all(Powiat, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
    Powiat = str_replace_all(Powiat, c(Ł = "L", Ś = "S", Ż = "Z", Ć = "C")),
    Powiat = ifelse(str_to_title(Powiat) == Powiat, paste0("M.", Powiat), Powiat),
    # Chamge the name of jeleniogorski county to karkonoski. In 2020 there was a change in nomenclature
    Powiat = ifelse(Powiat == "jeleniogorski", "karkonoski", Powiat)
  )

# Join shapefiles with data frame
SHP_county %<>%
  select(county, geometry, vovdshp) %>%
  full_join(df, by = c("county" = "Powiat", "vovdshp" = "Województwo"))
  

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
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = "#494E4F"),
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
ggplot() +
  geom_sf(
    data = SHP_county,
    aes(fill = KOMITET.WYBORCZY.PRAWO.I.SPRAWIEDLIWOŚĆ...ZPOW.601.9.19),
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
       title = "Distribution of votes for candidates belonging to the PiS committee",
       subtitle = "2019 elections to the Sejm and Senate of the Republic of Poland",
       caption = "Source: pkw.gov.pl, GUS") +
  scale_fill_gradientn(values = c(0, 0.4, 0.5, 0.6, 1),
                       # From yellow to blue
                       colors = c("#f0a556", "#f8cca4", "#f3f3f3", "#8a88b4", "#0e2b76"),
                       breaks = c(20, 50, 80), 
                       limits = c(20, 80),
                       labels = c("← more votes\nnon other parties", " 50%","more votes →\non PiS"),
                       expand = c(0, 0),
                       name = "Votes percentage:",
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
                                title.hjust = 0.5
                                ))

# Save the plot
ggsave("Plots/Percentage_of_votes_on_PiS.png", dpi = 900, height = 16, width = 25, units = "cm")

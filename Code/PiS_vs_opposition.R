library(tidyverse) 
library(magrittr) 
library(sf) 

# Load shapefiles for county and voivodeship
SHP_county <- st_read("Processed_data/Shapefiles/SHP_county.shp")
SHP_voivodeship <- st_read("Processed_data/Shapefiles/SHP_voivodeship.shp")

# Load data on vote results by county. Limit columns and change some columns names with 'OP' addition
df <- read.csv("Raw_data/wyniki_gl_na_listy_po_powiatach_proc_sejm.csv", sep = ";", dec = ",")[, c(1:4, 10, 12, 13, 15, 17)] %>% 
  set_colnames(c("Kod.TERYT", "Powiat", "Województwo", "Frekwencja", "KOMITET_OP_KO", "KOMITET_Konfederacja", "KOMITET_OP_PSL", "KOMITET_PiS", "KOMITET_OP_SLD"))

# Replace special letters and add prefix to cities to make a name alike to population_voivodeships names
df %<>%
  mutate(
    Województwo = str_replace_all(Województwo, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
    Powiat = str_replace_all(Powiat, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
    Powiat = str_replace_all(Powiat, c(Ł = "L", Ś = "S", Ż = "Z", Ć = "C")),
    Powiat = ifelse(str_to_title(Powiat) == Powiat, paste0("M.", Powiat), Powiat),
    # Chamge the name of jeleniogorski county to karkonoski. In 2020 there was a change in nomenclature
    Powiat = ifelse(Powiat == "jeleniogorski", "karkonoski", Powiat)
  )

# Calculating who won the elections by county
df %<>% 
  group_by(Kod.TERYT) %>% 
  mutate(oposition = sum(across(contains("KOMITET_OP")))) %>% 
  mutate(max_votes = max(across(contains("KOMITET"))),
         max_votes = max(across(.cols = c("max_votes", "oposition")))) %>% 
  mutate(who_won = case_when(
    KOMITET_OP_KO == max_votes ~ "KO",
    KOMITET_Konfederacja == max_votes ~ "Konfederacja",
    KOMITET_OP_PSL == max_votes ~ "PSL",
    KOMITET_PiS == max_votes ~ "PiS",
    KOMITET_OP_SLD == max_votes ~ "SLD",
    oposition == max_votes ~ "Opozycja",
    TRUE ~ "None"))

# Join shapefiles with data frame
SHP_county %<>%
  select(county, geometry, vovdshp) %>%
  left_join(df, by = c("county" = "Powiat", "vovdshp" = "Województwo"))

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
ggplot() +
  geom_sf(
    data = SHP_voivodeship,
    aes(),
    fill = "white"
  ) +
  geom_sf(
    data = SHP_county,
    aes(fill = who_won),
    color = "white",
    size = 0.15,
    alpha = 0.9
  ) +
  geom_sf(
    data = SHP_voivodeship,
    aes(),
    fill = NA,
    color = "white",
    size = 0.5
  ) +
  labs(x = NULL, y = NULL, 
       title = "The vicotrious committee by county with an opposition composed of committees KO, PSL, SLD",
       subtitle = "2019 elections to the Sejm and Senate of the Republic of Poland",
       caption = "Source: pkw.gov.pl, GUS") +
  scale_fill_manual(values = c("#CC3044", "#0e2b76"),
                    name = "") +
  theme_map() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(hjust = 0.5),
    legend.spacing = unit(0.4, "cm"),
  ) +
  guides(fill = guide_legend(title.position = "top"))

# Save the plot
ggsave("Plots/PiS_vs_opposition.png", dpi = 900, height = 16, width = 25, units = "cm")

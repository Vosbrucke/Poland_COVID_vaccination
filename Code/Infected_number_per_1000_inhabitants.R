library(tidyverse) 
library(magrittr) 
library(sf) 
library(readxl)
library(viridis)
library(cowplot)

# Load shapefiles for county and voivodeship
SHP_county <- st_read("Processed_data/Shapefiles/SHP_county.shp")
SHP_voivodeship <- st_read("Processed_data/Shapefiles/SHP_voivodeship.shp")

# Load recent population data downloaded from 
# https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/ludnosc-stan-i-struktura-ludnosci-oraz-ruch-naturalny-w-przekroju-terytorialnym-stan-w-dniu-31-12-2020,6,29.html
population <- readxl::read_xls("Raw_data/Population_by_county.xls")[,c(1:3)] %>%
  set_colnames(c("powiat", "teryt", "ludnosc")) %>%
  filter(row_number() > 7) %>%
  arrange(powiat) %>% 
  # Replace special letters, pull voivodeship code and add a starting letter to use it in the future to detect where the two data frames have 
  # differences
  mutate(WOJ = as.numeric(str_sub(teryt, 1, 2)), 
         powiat = str_replace_all(powiat, c(Ł = "L", Ś = "S", Ż = "Z", Ć = "C")),
         powiat = str_replace_all(powiat, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
         starting_letter = str_sub(powiat, 1, 1),
         powiat = str_remove(powiat, "M.")) %>% 
  # Remove voivodeship data
  filter(starting_letter != "W") 

# Load data on COVID-19 infections
df <- read_csv("Processed_data/daily_infections.csv")[,-1]

# Manupulate the data frame
df %<>%
  # Change teryt column format- remove 't' before the numbers 
  dplyr::mutate(teryt = str_remove(teryt, "[a-z]+"),
         voivodeship = str_replace_all(voivodeship, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
         county = str_replace_all(county, c(Ł = "L", Ś = "S", Ż = "Z", Ć = "C")),
         county = str_replace_all(county, c(ł = "l", ś = "s", ż = "z", ń = "n", ę = "e", ć = "c", ą = "a", ó = "o", ź = "z")),
         across(c(liczba_nowych_zakazen, liczba_przypadkow), ~ ifelse(is.na(.), 0, .))
  ) %>%
  dplyr::group_by(teryt, county) %>% 
  # Calculate the cumulative sum for new infections
  dplyr::summarise(cumsum = max(cumsum(liczba_nowych_zakazen) + cumsum(liczba_przypadkow)))

# Join the data frame with population
df %<>%
  left_join(population, by = c("teryt")) %>%
  drop_na() %>%
  mutate(ludnosc = as.numeric(ludnosc)) %>% 
  mutate(infections_per_1000_inhab = cumsum / ludnosc * 1000)


# Customize legend. Source: Timo Gressenbacher and Angelo Zehr
{
# define number of classes
no_classes <- 6

# extract quantiles
quantiles <- df %>%
  pull(mean(infections_per_1000_inhab)) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1)) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric

if (sum(quantiles >= 10) > no_classes / 3) {
  round_n <- 0
  nsmall <- 0
} else {
  round_n <- 2
  nsmall <- 2
}

# here we create custom labels
labels <- purrr::imap_chr(quantiles, function(., idx){
  return(paste0(format(round(quantiles[idx], round_n), nsmall = nsmall),
                " – ",
                format(round(quantiles[idx + 1], round_n), nsmall = nsmall)
  ))
})

# we need to remove the last label 
labels <- labels[1:length(labels) - 1]

# here we actually create a new 
# variable on the dataset with the quantiles
df %<>%
  mutate(mean_quantiles = cut(infections_per_1000_inhab,
                              breaks = quantiles,
                              labels = labels,
                              include.lowest = T))

}

# Join shapefiles with data frame
SHP_county %<>%
  mutate(county = str_remove(county, "M.")) %>%
  select(geometry, teryt) %>% 
  left_join(df, by = c("teryt"))

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
p1 <- ggplot(
  ) +
  geom_sf(
    data = SHP_county,
    aes(fill = mean_quantiles),
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
         title = "Number of COVID-19 infections\nper 1,000 people since the start of the pandemic",
         subtitle = "Data as of 19 May, 2022",
         caption = "Source: gov.pl, GUS") +
  viridis::scale_fill_viridis(
    option = "magma",
    name = "Liczba zakażonych",
    alpha = 0.8,
    begin = 0.1, # this option seems to be new (compared to 2016):
    # with this we can truncate the
    # color scale, so that extreme colors (very dark and very bright) are not
    # used, which makes the map a bit more aesthetic
    end = 0.9,
    direction = -1,
    discrete = T, # discrete classes, thus guide_legend instead of colorbar
    # direction = 1, # dark is lowest, yellow is highest
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T # display highest value on top
    )) +
   theme_map()

# Center the plot. Otherwise the plot was pushed to the left because of the legend
p2 <- plot_grid(ggplot() + theme_void(), p1 + theme(legend.position = "none"), get_legend(p1), ncol = 3, rel_heights = c(0.03, 1, 0.03), rel_widths = c(0.3, 1, 0.3)) + 
  theme(plot.background = element_rect(fill = "white"))

# Save the plot
ggsave("Plots/Infected_number_per_1000_inhabitants.png", p2, dpi = 900, height = 12, width = 20, units = "cm")
s
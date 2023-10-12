
# Load the data
tuesdata <- tidytuesdayR::tt_load(2023, week = 41)
haunted_places <- tuesdata$haunted_places

# I want to make a halloween themed map of the US with little ghosts or something where each place
library(usmap)
library(ggplot2) 
library(tidyverse)
library(showtext)
library(maps)
library(ggfx)
library(sysfonts)

# Add the font path and make sure it shows on the graph
font_add(family = "Creepster", regular = "/Users/joelsommerfeld/Library/Fonts/Creepster-Regular.ttf")
showtext_auto()
showtext_opts(dpi = 300)

haunts = haunted_places |>
  filter(!state_abbrev %in% c("AK", "HI")) |>
  filter(grepl("theatre|cinema|playhouse|museum|restaurant|steakhouse|park", location, ignore.case = TRUE)) |>
  select(1, 4, 7:8) |>
  na.omit()

# create data frame ------------------------------------------------------  

ggplot(data = haunts) +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), size = 0.33, color = "orange") +
  with_outer_glow(geom_point(aes(x = longitude, y = latitude), size = 0.5, color = "#8b7ac2"),
                  colour = "#d7cff1", sigma = 20, expand = 5) +
  coord_map(clip = "off") +
  theme_void() +
  labs(title = "Haunted Date Destinations Perfect for Friday the 13th",
       subtitle = "They are sure to scare the pants off your date",
       caption = "#TidyTuesday | Source: data.world | Graphic: JSommerfeld36") +
  theme(plot.title = element_text(colour = "#2AB613", family = "Creepster", size = 26, 
                                  hjust = 0.5, face = "bold", margin = margin(t = 20, b = 5)),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = "#2AB613", family = "Creepster", size = 13, 
                                     hjust = 0.5, margin = margin(b = 13)),
        plot.caption = element_text(colour = "#E78644", family = "Creepster", size = 10, 
                                    hjust = 0.5, margin = margin(b = 5)),
        legend.position = "none",
        #plot.margin = unit(c(1, 0.5, 1, 0.5), "cm"),
        plot.background = element_rect(fill = "#131313", colour = "#131313"), 
        panel.background = element_rect(fill = "#131313", colour = "#131313"), 
        panel.border = element_rect(fill = NA, colour = "#131313"))


# Save plot
file = paste0("TidyTuesday_", Sys.Date(), ".png")
ggsave(filename = file, plot = last_plot(), dpi = 500)

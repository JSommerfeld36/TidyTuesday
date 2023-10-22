

library(tidyverse)
library(scales)
library(Hmisc)
library(ggtext)
library(ggimage)
library(taylor)
library(ggchicklet)
library(showtext)


# Load the data
tuesdata <- tidytuesdayR::tt_load(2023, week = 42)

taylor_album_songs <- tuesdata$taylor_album_songs
taylor_all_songs <- tuesdata$taylor_all_songs
taylor_albums <- tuesdata$taylor_albums

# Make font crispy
showtext_opts(dpi = 500)


# Wrangle data to get to album level averages 
taylor_dat = taylor_album_songs |>
  select(album_name, album_release, danceability, energy, loudness, speechiness, 
         acousticness, instrumentalness, liveness, valence) |>
  drop_na() |>
  group_by(album_name) |>
  summarise(album_release = mean(album_release),
            Acousticness = mean(acousticness),
            Danceability = mean(danceability),
            Energy = mean(energy), 
            Loudness = mean(loudness),
            Speechiness = mean(speechiness), 
            Valence = mean(valence)
            ) |>
  mutate(Loudness = rescale(Loudness)) |>
    pivot_longer(cols = -c(album_name, album_release), 
               names_to = "variable",
               values_to = "value")

albums = unique(taylor_dat$album_name)

# Get album covers
album_covers = rbind.data.frame(
  album_1989 = "https://www.billboard.com/wp-content/uploads/2015/06/taylor-swift-1989-album-billboard-1548.jpg",
  fearless = "https://www.billboard.com/wp-content/uploads/2021/04/Taylor-Swift-fearless-album-art-cr-Beth-Garrabrant-billboard-1240-1617974663.jpg",
  lover = "https://www.billboard.com/wp-content/uploads/media/Taylor-Swift-Lover-album-art-2019-billboard-1240.jpg",
  midnights = "https://www.billboard.com/wp-content/uploads/2022/10/taylor-swift-midnights-album-cover-2022-billboard-1240.jpg",
  red = "https://www.billboard.com/wp-content/uploads/2022/10/taylor-swift-red-taylors-version-billboard-1240.jpg",
  speak_now = "https://www.billboard.com/wp-content/uploads/2022/06/taylor-swift-speak-now-billboard-1240.jpg",
  taylor_swift = "https://www.billboard.com/wp-content/uploads/2022/10/taylor-swift-self-titled-billboard-1240.jpg",
  evermore = "https://www.billboard.com/wp-content/uploads/2020/12/taylor-swift-cover-2020-billboard-1240-1607612466.jpg",
  folklore = "https://www.billboard.com/wp-content/uploads/2020/12/Taylor-swift-folklore-cover-billboard-1240-1607121703.jpg",
  reputation = "https://www.billboard.com/wp-content/uploads/2022/10/taylor-swift-reputation-billboard-1240.jpg"
)
album_covers$album_name = albums
colnames(album_covers)[1] = "album_cover"

# Join the album data with the album covers
taylor_dat = left_join(taylor_dat, album_covers)

# Use a Taylor Swift specific palette
pal = palette(taylor::color_palette(album_palettes$lover, n = 6))

# Stacked bar graph from ggplot modified to have rounded corners with ggchicklet
ggplot(taylor_dat, aes(x = reorder(album_name, album_release), y = value, group = album_name, fill = variable)) +
  geom_chicklet(width = 0.8) +
  scale_fill_manual(name = NULL, values = pal) +
  geom_image(aes(x = reorder(album_name, album_release), y = -0.3, image = album_cover), size = 0.08) +
  coord_flip() +
  labs(
    title = "The Sound of Taylor Swift",
    subtitle = "Average album characteristics as calculated by Spotify",
    caption = "#TidyTuesday | Source: {taylor} | Graphic: Joel Sommerfeld"
  ) + 
  theme(
    text = element_text(colour = "#D37F55"),
    
    plot.title = element_text(hjust = 0.15,  margin = margin(t = 15, b = 5),
                              family = "Satisfaction"),
    plot.subtitle = element_text(hjust = 0.15, family = "SignPainter", size = 14),
    plot.caption = element_text(family = "Monaco", size = 6),
    
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y =  element_blank(),
    axis.text.y = element_text(colour="#D37F55", family = "SignPainter", size = 12),
    
    legend.text = element_text(size = 13, family = "SignPainter"),

    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#D8CDC2"),
    panel.background = element_rect(fill = "#D8CDC2"),
    legend.background = element_rect(fill = "#D8CDC2"),
    legend.key = element_rect(fill = "#D8CDC2", color = NA)
  ) 
  
  # Save plot
  file = paste0("TidyTuesday_", Sys.Date(), ".png")
  ggsave(filename = file, plot = last_plot(), dpi = 500)
  
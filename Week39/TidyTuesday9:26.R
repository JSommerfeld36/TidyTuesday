

library(tidyverse)
library(png)
library(ggpubr)
#library(devtools)
#install_github("deepshamenghani/richmondway")

setwd("/Users/joelsommerfeld/Desktop/TidyTuesday/Week39")

# When does Roy Kent swear the most? Turns out the answer is really simple.... when he is talking lots. 

dat = richmondway::richmondway # data 
img <- readPNG("Field.png") # image for background of plot


ggplot(data = dat, aes(x = Episode, y = F_count_RK, group = as.factor(Season))) +
  background_image(img) + 
  geom_line(aes(color =  as.factor(Season), alpha = 1), size = 2) +
  geom_point(aes(color =  as.factor(Season), alpha = 1), size = 4) +
  scale_color_manual(name = "Season", values=c("#EA0406", "#FFD45A",  "#0176F2")) +
  geom_point(color = "white", size = 1) +
  scale_x_continuous(breaks = 1:12, minor_breaks = 1:12, expand = c(.05, .05)) +
  labs(title = "How many f#cks does Roy Kent give?", 
       caption = "Data from: https://github.com/deepshamenghani/richmondway", 
       x = "Episode in Season", 
       y = "Number of f#cks per episode") + 
  guides(alpha = 'none') +
  theme(
    plot.title = element_text(hjust = 0.15),
    plot.caption = element_text(hjust = 1.4, size = 12),
    text=element_text(size=20,  family="Bodoni MT Poster Compressed", colour = "white"),
    axis.text.x=element_text(colour="white"),
    axis.text.y=element_text(colour="white"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#1a6000"),
    panel.background = element_rect(fill = "#1a6000"),
    legend.background = element_rect(fill = "#1a6000"),
    legend.key = element_rect(fill = "#1a6000", color = NA),
  )

# Save plot
file = paste0("TidyTuesday_", Sys.Date(),".png")
ggsave(filename = file, plot = last_plot(), 
       units = "px", width = 2900, height = 2100, dpi = 500)



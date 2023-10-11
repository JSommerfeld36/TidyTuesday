

library(tidyverse)
library(wesanderson)
library(ggrepel)

# Load the data
tuesdata <- tidytuesdayR::tt_load(2023, week = 40)
grants <- tuesdata$grants
grant_opportunity_details <- tuesdata$grant_opportunity_details

# Do a little cleaning of the agency names
numeric_agencies = unique(sub("^\\d+\\s+", "", grants$agency_name))
grants$agency_name = gsub("[-_]", " ", grants$agency_name)

# Change to a numeric variable
grants$expected_number_of_awards = as.numeric(grants$expected_number_of_awards)

grants2 = grants %>%
  filter(agency_name %in% numeric_agencies) %>%
  drop_na(c(estimated_funding, expected_number_of_awards)) %>%
  group_by(agency_name) %>%
  summarise(total_estimated_funding = sum(estimated_funding),
            total_expected_number_of_awards = sum(expected_number_of_awards)) %>%
  arrange(desc(total_estimated_funding)) %>%
  slice(1:15) %>%
  add_column(index = 1:15, loc = rep(1, 15))

# Clean up the names a bit more
grants2$agency_name[grants2$agency_name == "Food for Progress 10.606"] = "Food for Progress"
grants2$agency_name[grants2$agency_name == "69A345 Office of the Under Secretary for Policy"] = "Office of the Under Secretary for Policy"

# Set a nice colour palette
pal = wes_palette("Zissou1", 15, type = "continuous")

# Create the plot
ggplot(grants2, aes(x = total_estimated_funding, y = as.factor(index))) + 
  geom_col(colour = pal, fill = pal, width = 0.5) +
  labs(title = "Top US Grant Agencies Funding Allocations for 2023", 
       x = "", 
       y = "Top 15 Funding Agencies", 
       caption = paste0( "Source: Grants.gov | ", "Graphic: JSommerfeld36")) +
  scale_x_continuous(labels = scales::dollar_format(suffix = " M", scale = 1e-6)) + 
  geom_text_repel(aes(label = agency_name),
                  colour = "white",
                  family = "Verdana",
                  size = 3,
                  nudge_x = 10,
                  nudge_y = 0.1,
                  hjust = -0.3,
                  vjust = -1,
                  box.padding = 0.5,
                  segment.curvature = 0,
                  segment.ncp = 2,
                  segment.angle = 15,
                  arrow = arrow(length = unit(0.015, "npc"))
  ) +
  theme(
    plot.title = element_text(hjust = 0.15, size = 20, face = "bold"),
    plot.caption = element_text(hjust = 1, size = 8),
    text = element_text(size=13,  family = "Verdana", colour = "white"),
    axis.text.x = element_text(colour = "white"),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#1B2329"),
    panel.background = element_rect(fill = "#1B2329"),
    legend.background = element_rect(fill = "#1B2329"),
    legend.key = element_rect(fill = "#1B2329", color = NA)
  )

# Save plot
file = paste0("TidyTuesday_", Sys.Date(),".png")
ggsave(filename = file, plot = last_plot(), 
       units = "px", width = 5050, height = 3550, dpi = 500)

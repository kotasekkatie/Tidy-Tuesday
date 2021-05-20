# global option
options(scipen = 999)

# required packages
library(tidyverse)
library(lubridate)
library(mmbtools)
library(tidytuesdayR)
#library(gganimate)
#library(transformr)
#library(ggpubr)
library(waffle)

#read in data
gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv')
crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')
households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')

# convert to long
crops_long <- crops %>%
  group_by(SubCounty) %>%
  pivot_longer(2:11, names_to = "Category", values_to = "Value") %>%
  arrange(SubCounty) %>%
  replace_na(list(Value = 0)) %>%
  arrange(SubCounty, desc(Value)) %>%
  mutate(Per10000 = Value/10000)

#filter counties
crops_filtered <- crops_long %>%
  filter(SubCounty %in% c("BARINGO", "BOMET", "BUNGOMA", "BUSIA"))

# static plot
#racing_crops <- ggplot(crops_long, aes(x = SubCounty, y = Value, fill = Category)) +
#  geom_col() +
#  scale_y_continuous(limits = c(0, 300000),
#                     breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)) +
#  scale_fill_mncol(palette = "heat", discrete = TRUE) +
#  labs(title = "Harvest by Category and County",
#       x = "",
#       y = "") +
#  transition_states(Category)

#animate(racing_crops,
#        nframes = 150,
#        width = 1500,
#        height = 400)

#anim_save("crops.gif")

# balloon plot
ggballoonplot(crops_filtered, fill = "Value", size = "Value"
              ) +
  scale_fill_viridis_c(option = "C")

# waffle chart
waffle(crops_filtered$Per10000, rows=5, size=0.6)
  
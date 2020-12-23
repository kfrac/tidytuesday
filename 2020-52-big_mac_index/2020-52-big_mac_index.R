library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(sf)
library(rnaturalearth)
library(sp)

tuesdata <- tidytuesdayR::tt_load(2020, week = 52)

big_mac <- tuesdata$`big-mac`

big_mac <- big_mac[,c(1:7)]

big_mac_2020 <- big_mac %>%
  filter(year(.$date) == "2020") %>%
  group_by(date) %>%
  mutate(relative_to_usd = dollar_price /dollar_price[big_mac_2020$iso_a3 == "USA"]*100)

big_mac_2020 <- big_mac %>%
  filter(date >= "2020-02-01") %>%
  mutate(relative_to_usd = dollar_price/dollar_price[iso_a3 == "USA"]*100)

#only necessary columns for join
big_mac_2020_rel_usd <- big_mac_2020[,c(1,2,8)]

#drop eurozone because it has no corresponding ISO A3 code
big_mac_2020_rel_usd <- big_mac_2020_rel_usd[-16,]

#import world / country boundary data
world <- ne_countries(scale = "medium", returnclass = "sf")

#remove Antarctica to make the map look more pretty
world <- world %>% filter(sov_a3 != "ATA")

#generate separate layer of only the big mac countries
big_mac_countries <- subset(world, world$iso_a3 %in% big_mac_2020_rel_usd$iso_a3)

#join big mac countries (from world data) with relative to USD prices (from The Economist)
big_mac_countries <- inner_join(x = big_mac_2020_rel_usd, y = big_mac_countries, by = "iso_a3")

#plot
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = big_mac_countries$geometry, aes(fill = big_mac_countries$relative_to_usd)) +
  scale_fill_gradient("Percent",
                      low = munsell::mnsl("5R 8/6"),
                      high = munsell::mnsl("5R 4/14")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Cost of Big Mac (USD) as a percent of the average Big Mac price in the United States on July 1, 2020", 
          subtitle =  "Americans traveling abroad will find Big Macs relatively inexpensive except for in Lebanon, Sweden & Switzerland") +
  labs(caption = "Data from The Economist")

ggsave("C:/RProjects/tidytuesday/2020-52-big_mac_index/big_mac_index.png", 
       width = 18, height = 12, units = "in", dpi = 300)

simplified <- big_mac_countries[,c("name_long", "relative_to_usd")]

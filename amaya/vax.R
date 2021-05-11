library(tidyverse)
library(dplyr)
library(readxl)
library(scales)
library(viridis)

# read in and plot ca vax over time
ca_time <- read_csv("~blog_data/ca_time.csv") %>%
  filter(race != "Unknown")
ca_tg <- ggplot(ca_time, aes(x = date, y = frac, colour = race)) +
  geom_line() +
  labs(colour = "Race", title = "Proportion of Racial Groups Vaccinated Over Time", 
       subtitle = "California, 2021") +
  xlab("Date") + ylab("Proportion of Racial Group")
ca_tg
# read in and plot tn vax over time 
tn_time <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/tn_time.csv") %>%
  filter(race != "UNKNOWN")
tn_tg <- ggplot(tn_time, aes(x = date, y = frac, colour = race)) +
  geom_line() +
  labs(colour = "Race", title = "Proportion of Racial Groups Vaccinated Over Time", 
       subtitle = "Tennessee, 2021") +
  xlab("Date") + ylab("Proportion of Racial Group")
tn_tg



# read in and plot ca county vaccinations
ca_county <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/ca_county.csv") %>%
  mutate(subregion = tolower(county)) %>%
  select(-county)
ca_counties <- map_data(map = "county", region = "california")
ca_map <- ca_counties %>%
  left_join(ca_county, by = "subregion")
ggplot(ca_map, aes(x = long, y = lat, group = group, fill = total_vax)) +
  geom_polygon(colour = "white") +
  theme_void() +
  coord_fixed(ratio = 1.3) +
  facet_wrap(~administered_date) +
  labs(fill = "Total Vaccinations") +
  scale_fill_viridis(direction = -1) +
  ggtitle("Vaccinations in California, Spring 2021")

# read in and plot tn county vaccinations
options(scipen = 999)
tn_county <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/tn_county.csv") %>%
  mutate(subregion = tolower(COUNTY)) %>%
  select(-COUNTY)
tn_counties <- map_data(map = "county", region = "tennessee")
tn_map <- tn_counties %>%
  left_join(tn_county, by = "subregion") %>%
  na.omit()
ggplot(tn_map, aes(x = long, y = lat, group = group, fill = total_vax)) +
  geom_polygon(colour = "white") +
  theme_void() +
  coord_fixed(ratio = 1.3) +
  facet_wrap(~date) +
  labs(fill = "Total Vaccinations") +
  scale_fill_viridis(direction = -1) +
  ggtitle("Vaccinations in Tennessee, Spring 2021")
  





  
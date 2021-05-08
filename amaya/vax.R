library(tidyverse)
library(dplyr)
library(readxl)

# read in and plot ca vax over time
ca_time <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/ca_time.csv")
ca_tg <- ggplot(ca_time, aes(x = date, y = frac, colour = race)) +
  geom_line()
ca_tg
 

# read in and plot tn vax over time 
tn_time <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/tn_time.csv")
tn_tg <- ggplot(tn_time, aes(x = date, y = frac, colour = race)) +
  geom_line() +
  labs(colour = "Race", title = "Proportion of Racial Groups Vaccinated Over Time in Tennessee, 2021") +
  xlab("Date") + ylab("Proportion of Racial Group")
tn_tg



# read in and plot ca county stuff
ca_county <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/ca_county.csv")


# read in and plot tn county stuff
ca_vax <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/tn_.csv")
  
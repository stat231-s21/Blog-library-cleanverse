library(tidyverse)
library(dplyr)
library(readxl)

# read in and plot ca over time
ca_time <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/ca_time.csv")
ca_tg <- ggplot(ca_time, aes(x = date, y = frac, colour = race)) +
  geom_line()
ca_tg
 

# read in and plot tn over time 
tn_time <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/tn_time.csv")
tn_tg <- ggplot(tn_time, aes(x = date, y = frac, colour = race)) +
  geom_line()
tn_tg



# read in and plot ca county stuff
ca_county <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/ca_county.csv")


# read in and plot tn county stuff
ca_vax <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/tn_.csv")
  
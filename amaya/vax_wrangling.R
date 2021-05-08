library(tidyverse)
library(dplyr)
library(readxl)

# create TN plot: proportion of race vaccinated over time
tn_vax <- read_excel("/Users/smolea/git/Blog-library-cleanverse/amaya/TN.covid.demographics.state.XLSX") %>%
  filter(CATEGORY == "RACE") %>%
  mutate(date = as.Date(DATE),
         race = CAT_DETAIL,
         count = VACCINE_COUNT
         ) %>%
  select(date, race, count) %>%
  # divide by calculated population by race in TN, 2021
  # set unknown counts to 0, potentially leave out
  mutate(frac = case_when(race == "ASIAN" ~ count/121524.55,
                          race == "BLACK OR AFRICAN AMERICAN" ~ count/1163857.976,
                          race == "OTHER/MULTIRACIAL" ~ count/271520.566,
                          race == "WHITE" ~ count/5387356.908,
                          race == "UNKNOWN" ~ 0,
                          TRUE ~ count))
# writing to csv
write_csv(x = tn_vax, "/Users/smolea/git/Blog-library-cleanverse/amaya/tn_time.csv")
tn <- ggplot(tn_vax, aes(x = date, y = frac, colour = race)) +
  geom_line()
tn


# create CA plot: proportion of race vaccinated over time
ca_vax <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/vaccine_demographics_ca.csv") %>%
  filter(demographic_category == "Race/Ethnicity") %>%
  mutate(race0 = demographic_value,
         date = administered_date,
         count = cumulative_fully_vaccinated) %>%
  select(race0, date, count) %>%
  mutate(race = case_when(race0 == "Other Race" ~ "Other/Multiracial",
                            race0 == "Multiracial" ~ "Other/Multiracial",
                            TRUE ~ race0)) %>%
  group_by(race, date) %>%
  summarise(race = race,
            count = sum(count),
            date = date) %>%
  # divide by calculated population by race in CA, 2021
  # set unknown counts to 0, potentially leave out
  mutate(frac = case_when(race == "American Indian or Alaska Native" ~ count/138647.25,
                          race == "Other/Multiracial" ~ count/1319129.55,
                          race == "Black or African American" ~ count/2182703.85,
                          race == "White" ~ count/14514386.4,
                          race == "Latino" ~ count/15564144.15,
                          race == "Native Hawaiian or Other Pacific Islander" ~ count/142608.6,
                          race == "Asian" ~ count/5751880.2,
                          race == "Unknown" ~ 0,
                          TRUE ~ count))
# writing to csv
write_csv(x = ca_vax, "/Users/smolea/git/Blog-library-cleanverse/amaya/ca_time.csv")
ca <- ggplot(ca_vax, aes(x = date, y = frac, colour = race)) +
  geom_line()
ca


# create data for comparison maps
ca_county <- read_csv("/Users/smolea/git/Blog-library-cleanverse/amaya/CA_countydemo.csv") %>%
  filter(demographic_category == "Age Group" & (administered_date == as.Date("2021-04-15") | administered_date == as.Date("2021-02-15"))) %>%
  group_by(administered_date, county) %>%
  summarise(total_vax = sum(cumulative_fully_vaccinated, na.rm = TRUE))
# writing to csv
write_csv(x = ca_county, "/Users/smolea/git/Blog-library-cleanverse/amaya/ca_county.csv")

tn_county <- read_excel("/Users/smolea/git/Blog-library-cleanverse/amaya/TN_County_Vax_Demographics.XLSX") %>%
  mutate(date = as.Date(DATE)) %>%
  filter((date == as.Date("2021-02-15") | date == as.Date("2021-04-15")) & CATEGORY == "SEX") %>%
  group_by(date, COUNTY) %>%
  summarise(total_vax = sum(RECIPIENT_COUNT, na.rm = TRUE))
# writing to csv
write_csv(x = tn_county, "/Users/smolea/git/Blog-library-cleanverse/amaya/tn_county.csv")

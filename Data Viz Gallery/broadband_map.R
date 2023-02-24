# Load packages ----

library(tidyverse)
library(rmapshaper)
library(tigris)
library(here)
library(sf)


# See resources ----

# Request Census API Key: https://api.census.gov/data/key_signup.html
# Load Census API Key in Renviron: https://walker-data.com/tidycensus/reference/census_api_key.html
# Learn more in Broadband TidyTuesday repo: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-11/readme.md
# See all {tigris} functions: https://github.com/walkerke/tigris
# Apply lesson learned from Charlie in Mapping with R (R for the Rest of Us)


# Get data ----

## retrieve TidyTuesday dataset

broadband <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv')

## retrieve sf object using {tigris}

ut_counties <- counties(state = "UT") %>%   # use default year = 2021
  rmapshaper::ms_simplify()


# Do quick EDA using skimr ----

skimr::skim(broadband)

skimr::skim(ut_counties)

## change data types as needed for joining

broadband$COUNTY.ID <- as.character(broadband$COUNTY.ID)

broadband$BROADBAND.AVAILABILITY.PER.FCC <- as.numeric(broadband$BROADBAND.AVAILABILITY.PER.FCC)

broadband$BROADBAND.USAGE <- as.numeric(broadband$BROADBAND.USAGE)


## Merge data ----

ut_counties_sf <- ut_counties %>%
  left_join(broadband, by = c("GEOID" = "COUNTY.ID")) %>%
  select(-c("LSAD", "CLASSFP", "MTFCC", "CSAFP", "CBSAFP", "METDIVFP", "FUNCSTAT", "COUNTY.NAME")) %>%
  rename(fcc_broadband_availability = "BROADBAND.AVAILABILITY.PER.FCC",
         broadband_usage = "BROADBAND.USAGE")

names(ut_counties_sf) <- tolower(names(ut_counties_sf)) # change case


# Test using mapview ----

# ut_counties_sf %>%
#   mapview(zcol = "fcc_broadband_availability")
# 
# ut_counties_sf %>%
#   mapview(zcol = "broadband_usage")


# Map map ----

ut_counties_sf %>%
  ggplot() +
  geom_sf(aes(fill = broadband_usage),
          color = "white",               # change county borders
          size = 0.5) +                  # change stroke width
  scale_fill_viridis_c()


# Use UT because it is home of "Silicon Slopes".
# Highlight the relevant counties: Utah County (Provo), Salt Lake County (Salt Lake City), Summit County (Park City).
# Overlay a layer with point locations for Provo, Salt Lake, and Park City.
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
# View data from Census Bureau's new ACCESS Broadband Dashboard: https://www.census.gov/programs-surveys/community-resilience-estimates/partnerships/ntia/broadband-act.html


# Get data ----

## retrieve TidyTuesday dataset

broadband <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv')

## retrieve sf object using {tigris}

ut_counties <- counties(state = "UT", year = 2019) %>%
  rmapshaper::ms_simplify()


# Do quick EDA using skimr ----

skimr::skim(broadband)

skimr::skim(ut_counties)

## change data type for joining

broadband$COUNTY.ID <- as.character(broadband$COUNTY.ID)


# Merge and clean up data ----

ut_counties_sf <- ut_counties %>%
  left_join(broadband, by = c("GEOID" = "COUNTY.ID")) %>%
  select(-c("LSAD", "CLASSFP", "MTFCC", "CSAFP", "CBSAFP", "METDIVFP", "FUNCSTAT", "COUNTY.NAME")) %>%
  rename(fcc_broadband_avail = "BROADBAND.AVAILABILITY.PER.FCC",
         broadband_usage = "BROADBAND.USAGE")

## change case

names(ut_counties_sf) <- tolower(names(ut_counties_sf))

## change data type

ut_counties_sf$fcc_broadband_avail <- as.numeric(ut_counties_sf$fcc_broadband_avail)

ut_counties_sf$broadband_usage <- as.numeric(ut_counties_sf$broadband_usage)


# Create new sf object for counties comprising "Silicon Slopes" ----

## Utah County geoid: "49049"
## Salt Lake County geoid: "49035"
## Summit County geoid: "49043"

## subset counties 

utah_co_sf <- ut_counties_sf[ut_counties_sf$geoid == "49049", ]
salt_lake_co_sf <- ut_counties_sf[ut_counties_sf$geoid == "49035", ]
summit_co_sf <- ut_counties_sf[ut_counties_sf$geoid == "49043", ]

## union counties; use bind_rows() to create retain county boundaries

silicon_slopes_sf <- utah_co_sf %>%
  st_union(salt_lake_co_sf) %>%
  st_union(summit_co_sf) %>%
  select(name, geometry)

## change name 

silicon_slopes_sf$name <- "Silicon Slopes"


# Test using mapview ----

# ut_counties_sf %>%
#   mapview(zcol = "fcc_broadband_avail)")
# 
# ut_counties_sf %>%
#   mapview(zcol = "broadband_usage")
# 
# mapview(silicon_slopes_sf)


# Map map ----

  ggplot() +
  geom_sf(data = ut_counties_sf,
          aes(fill = broadband_usage),
          color = "white",               # change county borders
          size = 0.5) +                  # change stroke width
  scale_fill_viridis_c("Broadband\nUsage, 2019") +
  geom_sf(data = silicon_slopes_sf,
          color = "white",
          size = 2,
          alpha = 0) +     # make layer transparent
  theme_void()


# Overlay a layer with point locations for Provo, Salt Lake, and Park City.

# Source: https://www.latlong.net/

city_coordinates <- tribble(
  ~city, ~long, ~lat,
  "Salt Lake City", -111.876183, 40.758701,
  "Provo", 	-111.658531, 40.233845,
  "Park City", -111.497971, -111.497971
)
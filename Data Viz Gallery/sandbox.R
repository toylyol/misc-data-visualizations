
#***********
# HEXBIN MAP ----


## References ----


## Load packages ----

library(fontawesome)
library(tidyverse)
library(openxlsx)
library(mapview)
library(here)
library(sf)

path <- here()


## Retrieve subset of Multiple Chronic Conditions (MCC) dataset using CMS API ----
# 
# library(jsonlite)
# 
# url <- "https://data.cms.gov/data-api/v1/dataset/15b08729-6ea2-4789-bf1a-b96b1da8338f/data?filter[Bene_Demo_Lvl][value]=Sex&filter[Bene_Geo_Lvl][value]=State"
# 
# mcc_data <-  jsonlite::fromJSON(url)

# write.xlsx( mcc_data, paste0(path, "/data/mcc_data.xlsx") ) # save a copy of the MCC data


## Load MCC data ----

mcc_data <- read.xlsx( paste0(path, "/data/mcc_data.xlsx") )

### Ensure correct data type ----

mcc_data <- mcc_data %>% 
  mutate( across(c(Prvlnc, Tot_Mdcr_Stdzd_Pymt_PC, Tot_Mdcr_Pymt_PC, Hosp_Readmsn_Rate, ER_Visits_Per_1000_Benes), 
                 as.numeric) )


## Subset MCC data ----

data <- mcc_data %>%
  filter( Bene_Age_Lvl == "<65",
          Bene_Demo_Desc == "Female",
          Bene_MCC == "6+" )


## Load hexagonal U.S. shapefile ----

shapefile <- paste0(path,"/data/us_states_hexgrid.shp") # download from CARTO

hex_map <- read_sf(shapefile)

### Clean up shapefile columns ----

hex_map <- hex_map %>%
  mutate( google_nam = gsub(" \\(United States\\)", "", google_nam) ) %>%
  rename( state_name = google_nam,
          abbrev = iso3166_2 ) %>%
  select( -c(created_at, updated_at, label, bees) ) # remove necessary columns

### Join (non-spatial) with MCC data ----

hex_map <- hex_map %>%
  left_join( data, by = c("state_name" = "Bene_Geo_Desc") )


## Generate map ----

hex_map <- hex_map %>%
  ggplot() +
  geom_sf( aes(fill = Prvlnc) ) +
  coord_sf() +
  theme_void()

# I started to run into issues with geom_sf_label( aes(label = abbrev) ) from https://ggplot2-book.org/maps.html...  
# This produced an error message about using fortify().



#***********
# SLOPEGRAPH ----


## References ----

# The aca_data CSV was downloaded from Kaggle: https://www.kaggle.com/datasets/hhs/health-insurance
# A "light gray" gradient courtesy of ColorHexa: https://www.colorhexa.com/d3d3d3
# There is a package to create a momchromatic palette: https://github.com/cararthompson/monochromeR
# The colorgorical tool is great for generating color palettes that are easy to discriminate.
# https://community.rstudio.com/t/setting-colours-in-ggplot-conditional-on-value/8328/2
# https://r-graphics.org/recipe-appearance-hide-gridlines - remove horizontal gridlines


## Load packages and raw data ----

library(dplyr)
library(ggplot2)
library(here)

path <- here()
raw_data <- read.csv( paste0(path, "/data/aca_data.csv") )


## Clean data ----

### Rename columns ----

data <- raw_data %>% rename(
  
  "state" = "State",                                        
  "uninsured_rate_2010" = "Uninsured.Rate..2010.",                       
  "uninsured_rate_2015" = "Uninsured.Rate..2015.",                        
  "uninsured_rate_change" = "Uninsured.Rate.Change..2010.2015.",
  "insurance_cvg_change" = "Health.Insurance.Coverage.Change..2010.2015.",  # unclear meaning
  "employer_cvg_2015" = "Employer.Health.Insurance.Coverage..2015.",        # unclear meaning
  "marketplace_cvg_2016" = "Marketplace.Health.Insurance.Coverage..2016.",  # unclear meaning
  "marketplace_tax_credits_2016" = "Marketplace.Tax.Credits..2016.",
  "avg_mon_tax_credit_2016" = "Average.Monthly.Tax.Credit..2016.",
  "state_Medicaid_expan_2016" = "State.Medicaid.Expansion..2016.",
  "Medicaid_enroll_2013" = "Medicaid.Enrollment..2013.",
  "Medicaid_enroll_2016" = "Medicaid.Enrollment..2016.",
  "Medicaid_enroll_change" = "Medicaid.Enrollment.Change..2013.2016.",
  "Medicare_enroll_2016" = "Medicare.Enrollment..2016."
  
)

### Convert percentages to decimals ----

data$uninsured_rate_2010 <- as.numeric( gsub("%","",data$uninsured_rate_2010) )/100

data$uninsured_rate_2015 <- as.numeric( gsub("%","",data$uninsured_rate_2015) )/100

data$uninsured_rate_change <- as.numeric( gsub("%","",data$uninsured_rate_change) )/100

### Remove whitespace and dollar signs ----

data$state <- stringr::str_trim(data$state)
data$avg_mon_tax_credit_2016 <- stringr::str_trim(data$avg_mon_tax_credit_2016)
data$avg_mon_tax_credit_2016 <- as.numeric( gsub("\\$","",data$avg_mon_tax_credit_2016) )

### Correct error in dataset ----

data$uninsured_rate_change <- ifelse(data$state == "United States", -0.061, data$uninsured_rate_change)

### Identify states with the top 10 largest uninsured_rate_change ----

top_states <- data %>% 
  select(state, uninsured_rate_change) %>%
  filter(state != "United States") %>%            # remove aggregate value for entire country
  slice_min(uninsured_rate_change, n = 10) %>%    # replace the superseded top_n() function
  pull(state)                                     # isolate data in a column to save as a character vector

top_states[[11]] <- "United States" # add aggregate US to the top_states list to use for filtering

### Add column with rounded magnitude of uninsured_rate_change ----

data <- data %>%
  mutate( pct_change = as.character(round(uninsured_rate_change*100, 0)) )


## Restructure data for slopegraph ----

df_slopegraph <- data %>% 
  filter(state %in% top_states) %>%
  select(state, uninsured_rate_2010, uninsured_rate_2015, pct_change) %>%
  pivot_longer(!c(state,pct_change), names_to = "date", values_to = "rate") 

df_slopegraph$date <- df_slopegraph$date %>%
  str_replace_all("uninsured_rate_2010", "2010") 

df_slopegraph$date <- df_slopegraph$date %>%
  str_replace_all("uninsured_rate_2015", "2015")


## Generate a monochrome color palette ----

# mono_palette <- monochromeR::generate_palette( c(15, 75, 99), 
#                                     modification = "go_lighter", 
#                                     n_colours = 3 )
# 
# # use view_palette(mono_palette) to see the palette in viewer
# # put mono_palette in console to see all hex codes from darkest to lightest
# # [1] "#0F4B63" "#6F93A1" "#CFDBDF"

### Save teal color palette ----

teal_palette <- c("-6" = "#d3d3d3",
                  "-8" = "#CFDBDF",
                  "-9" = "#6F93A1",
                  "-10" = "#0F4B63")

# Starting color: #0F4B63
# Lightness range: 25-85
# Hue filters: 150-177 degrees
# Score importance: pair preference 100%
# Number of colors: 3
# ["rgb(16,74,97)", "rgb(148,228,202)", "rgb(63,161,135)"]
# ["#104a61", "#94e4ca", "#3fa187"]

# Top 10 States with Largest Decrease in Uninsured Rate after Affordable Care Act 
# On average, the unisured rate decreased six percent from 2010 to 2015

## Generate slopegraph ----

slopegraph <- ggplot(data = df_slopegraph, aes(x = date, y = rate, group = state, color = pct_change))  +
  geom_line(alpha = 1, size = 1) +
  geom_point(alpha = 1, size = 4) +
  scale_color_manual(values = teal_palette) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),   # remove horizontal gridlines 
    panel.grid.minor.y = element_blank()
  )



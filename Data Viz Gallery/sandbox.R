
#***********************************************************************************************
# HEXBIN MAP ----


## References ----

# https://r-graph-gallery.com/328-hexbin-map-of-the-usa.html


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
  geom_sf( aes(fill = Prvlnc),
           color = "white") +
  coord_sf() +
  theme_void()

# I started to run into issues with geom_sf_label( aes(label = abbrev) ) from https://ggplot2-book.org/maps.html...  
# This produced an error message about using fortify().



#***********************************************************************************************
# SLOPEGRAPH ----


## References ----

# The aca_data CSV was downloaded from Kaggle: https://www.kaggle.com/datasets/hhs/health-insurance
# A "light gray" gradient courtesy of ColorHexa: https://www.colorhexa.com/d3d3d3
# There is a package to create a momchromatic palette: https://github.com/cararthompson/monochromeR
# The colorgorical tool is great for generating color palettes that are easy to discriminate.
# These dimensions look nice in the viewer: 897 (width) x 659 (height)
# Other nice grays that were considered: #3d3d3d and #5d5d5d
# Add custom fonts: https://stackoverflow.com/questions/71573377/cannot-import-fonts-into-r
# Align {ggtext}: https://stackoverflow.com/questions/64701500/left-align-ggplot-caption
# Learn more {ggrepel}: https://ggrepel.slowkow.com/articles/examples.html
# Learn more about {showtext}: https://cran.rstudio.com/web/packages/showtext/vignettes/introduction.html
# See slopegraph tutorial: https://ibecav.github.io/slopegraph/
# See how to change color of label text only in {ggrepel} if desited: https://stackoverflow.com/questions/49622822/control-colour-of-geom-text-repel
# Learn more about specify custom scale: https://ggplot2.tidyverse.org/reference/scale_manual.html
# Specify a custom scale: https://community.rstudio.com/t/setting-colours-in-ggplot-conditional-on-value/8328/2
# Remove gridlines: https://r-graphics.org/recipe-appearance-hide-gridlines
# Change line types: http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software
# Learn how to install Google fonts: https://github.com/rladies/meetup-presentations_freiburg/blob/master/2021-11-10_ggplot_fonts/ggplot_fonts_RLadiesFreiburg.Rmd
# See hex codes in viridis: https://www.thinkingondata.com/something-about-viridis-library/
# Fix plot/axis alignment: https://ggplot2.tidyverse.org/articles/faq-axes.html#how-can-i-remove-the-space-between-the-plot-and-the-axis
# https://stackoverflow.com/questions/14487188/increase-distance-between-text-and-title-on-the-y-axis
# https://stackoverflow.com/questions/15624656/label-points-in-geom-point


## Load packages and raw data ----

library(tidyverse)
library(openxlsx)
library(ggrepel)
library(ggtext)
library(here)


## Load and clean data ----

path <- here()


# raw_data <- read.csv( paste0(path, "/data/aca_data.csv") )
# 
# ### Rename columns ----
# 
# data <- raw_data %>% rename(
#   
#   "state" = "State",                                        
#   "uninsured_rate_2010" = "Uninsured.Rate..2010.",                       
#   "uninsured_rate_2015" = "Uninsured.Rate..2015.",                        
#   "uninsured_rate_change" = "Uninsured.Rate.Change..2010.2015.",
#   "insurance_cvg_change" = "Health.Insurance.Coverage.Change..2010.2015.",  # unclear meaning
#   "employer_cvg_2015" = "Employer.Health.Insurance.Coverage..2015.",        # unclear meaning
#   "marketplace_cvg_2016" = "Marketplace.Health.Insurance.Coverage..2016.",  # unclear meaning
#   "marketplace_tax_credits_2016" = "Marketplace.Tax.Credits..2016.",
#   "avg_mon_tax_credit_2016" = "Average.Monthly.Tax.Credit..2016.",
#   "state_Medicaid_expan_2016" = "State.Medicaid.Expansion..2016.",
#   "Medicaid_enroll_2013" = "Medicaid.Enrollment..2013.",
#   "Medicaid_enroll_2016" = "Medicaid.Enrollment..2016.",
#   "Medicaid_enroll_change" = "Medicaid.Enrollment.Change..2013.2016.",
#   "Medicare_enroll_2016" = "Medicare.Enrollment..2016."
#   
# )
# 
# ### Convert percentages to decimals ----
# 
# data$uninsured_rate_2010 <- as.numeric( gsub("%","",data$uninsured_rate_2010) )/100
# 
# data$uninsured_rate_2015 <- as.numeric( gsub("%","",data$uninsured_rate_2015) )/100
# 
# data$uninsured_rate_change <- as.numeric( gsub("%","",data$uninsured_rate_change) )/100
# 
# ### Remove whitespace and dollar signs ----
# 
# data$state <- stringr::str_trim(data$state)
# data$avg_mon_tax_credit_2016 <- stringr::str_trim(data$avg_mon_tax_credit_2016)
# data$avg_mon_tax_credit_2016 <- as.numeric( gsub("\\$","",data$avg_mon_tax_credit_2016) )
# 
# ### Correct error in dataset ----
# 
# data$uninsured_rate_change <- ifelse(data$state == "United States", -0.061, data$uninsured_rate_change)
# 
# ### Save a copy of cleaned data ----
# 
# write.xlsx( data, paste0(path, "/data/clean_aca_data.xlsx") )


### Read in cleaned data ----

data <- read.xlsx( paste0(path, "/data/clean_aca_data.xlsx") )


## Restructure data for slopegraph ----

### Identify states with the top 10 largest uninsured_rate_change ----

top_states <- data %>% 
  select(state, uninsured_rate_change) %>%
  filter(state != "United States") %>%            # remove aggregate value for entire country so not in top 10
  slice_min(uninsured_rate_change, n = 10) %>%    # replace the superseded top_n() function
  pull(state)                                     # isolate data in one column to save as a character vector to filter later

top_states[[11]] <- "United States" # add aggregate US to the top_states list to use for filtering


### Add columns to use in formatting ggplot ----

data <- data %>%
  mutate( pct_change = as.character(round(uninsured_rate_change*100, 0)) ) %>%
  mutate( line_style = ifelse(state == "United States", "dotted", "solid") ) %>%   # add col so USA will have special line type
  mutate( abbrev = case_when(state == "Nevada" ~ "NV",
                             state == "Oregon" ~ "OR",
                             state == "California" ~ "CA",
                             state == "Kentucky" ~ "KY",
                             state == "New Mexico" ~ "NM",
                             state == "West Virginia" ~ "WV",
                             state == "Arkansas" ~ "AR",
                             state == "Florida" ~ "FL",
                             state == "Colorado" ~ "CO",
                             state == "Washington" ~ "WA",
                             state == "United States" ~ "USA") ) %>%
  mutate( largest = case_when(abbrev == "CA" ~ "Yes",                              # add col to use for custom color scale
                              abbrev == "OR" ~ "Yes",
                              abbrev == "NV" ~ "Yes",
                              TRUE ~ "No") )
  

### Subset dataframe ----

df_slopegraph <- data %>% 
  filter(state %in% top_states) %>%
  select(state, uninsured_rate_2010, uninsured_rate_2015, pct_change, line_style, abbrev, largest) %>%
  pivot_longer(!c(state, line_style, abbrev, pct_change, largest),  # delineate cols to keep
               names_to = "date", 
               values_to = "rate") 

df_slopegraph$date <- df_slopegraph$date %>%
  str_replace_all("uninsured_rate_2010", "2010") %>%  # change name
  str_replace_all("uninsured_rate_2015", "2015")


## Explore color palettes ----

# mono_palette <- monochromeR::generate_palette( c(15, 75, 99), 
#                                     modification = "go_lighter", 
#                                     n_colours = 3 )
# 
# # use view_palette(mono_palette) to see the palette in viewer
# # put mono_palette in console to see all hex codes from darkest to lightest
# # [1] "#0F4B63" "#6F93A1" "#CFDBDF"
# 
# monochromeR::generate_palette("#2D708EFF", modification = "go_both_ways", n_colors = 3, view_palette = TRUE, 
#                               view_labels = TRUE)
# 
# ### Save monochromatic color palette ----
# 
# mono_palette <- c("-6" = "#d3d3d3",
#                   "-8" = "#CFDBDF",
#                   "-9" = "#6F93A1",
#                   "-10" = "#0F4B63")
# 
# ### Delineate Colorgorical color palette ----
# 
# # The following parameters were used in the Colorgorical tool () in order to generate the color palette:
# # starting color, #0F4B63; lightness range, 25-85; hue filters, 150-177 degrees; score importance, 100% pair preference.
# 
# teal_palette <- c("-6" = "#d3d3d3",
#                   "-8" = "#104a61", 
#                   "-9" = "#94e4ca", 
#                   "-10" = "#3fa187")
# 
# ### Delineate minimal color palette ----
# 
# minimal_palette <- c("-6" = "#CFDBDF",
#                      "-8" = "#CFDBDF", 
#                      "-9" = "#CFDBDF", 
#                      "-10" = "#0F4B63")

## Add Google fonts ----

sysfonts::font_add_google(name = "Open Sans",
                          family = "Open Sans")

showtext::showtext_auto()  # load the font; must be done every session


## Generate slopegraph ----

slopegraph <- ggplot(data = df_slopegraph, 
                     aes(x = date, 
                         y = rate, 
                         group = state, 
                         color = largest,                                  # specify col to use to color points and lines
                         linetype = line_style,                            # specify col to change line style for USA
                         label = paste0(abbrev, "  ", rate*100, "%")))  +
  geom_line(alpha = 1, size = 1.25) +
  geom_point(alpha = 1, size = 4) +
  geom_text_repel( data = df_slopegraph %>% filter(date == "2010"),        # label pts directly; format label for 2010
                   family = "Open Sans",
                   hjust = "left", 
                   fontface = "bold", 
                   size = 3.5, 
                   nudge_x = -.3, 
                   direction = "y") +
  geom_text_repel( data = df_slopegraph %>% filter(date == "2015"),
                   family = "Open Sans",
                   hjust = "right",
                   fontface = "bold", 
                   size = 3.5, 
                   nudge_x = .3, 
                   direction = "y") +
  scale_color_manual(values = c("Yes" = "#2D708EFF",            # specify custom scale colors using largest col
                                "No" = "#c9c9c9")) +
  scale_linetype_manual(values = c("solid" = "solid",           # specify line types
                                   "dotted" = "dotdash")) +
  scale_x_discrete(expand = c(0, 0)) +                          # remove space between plot and axis
  xlab("\nYear") +                                              # change axes titles, using newline to add space between axes
  ylab("Percentage of Uninsured Persons\n") +
  labs(title = "<b>Top 10 States with Largest Decrease in Percentage of Uninsured Persons after the Affordable Care Act</b>",
       subtitle = "On average, the uninsured rate in the United States decreased six percent from 2010 to 2015. <br>The states with the <span style ='color: #0F4B63'>**largest decrease**</span> were California, Oregon, and Nevada.",
       caption = "Source: Kaggle") +
  theme_minimal() +
  theme(
    text = element_text(family = "Open Sans"),                        # set the font family for all {ggtext} elements
    plot.title.position = "plot",                                     # help ensure title aligned with plot
    axis.text.y = element_blank(),                                    # remove y axis labels
    panel.grid.major.y = element_blank(),                             # remove horizontal gridlines 
    panel.grid.minor.y = element_blank(),
    plot.title = element_markdown( margin = margin(0, 0, 3, 0) ),     # use {ggtext} to format title; HTML tags will format text; markdown has to be used for bolding font-weight span style will not work
    plot.subtitle = element_markdown( margin = margin(0, 0, 6, 0) ),  # note order of margins: top, right, bottom, left
    legend.position = "none"                                          # remove legend
  )



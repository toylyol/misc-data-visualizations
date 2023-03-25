#*************************************#
# Sandbox for Static Data Viz Gallery #
#*************************************#

#***********************************************************************************************
# HEXBIN MAP ----


## References ----

# See hexbin map tutorial: https://r-graph-gallery.com/328-hexbin-map-of-the-usa.html
# View MCC data dictionary: https://data.cms.gov/resources/multiple-chronic-conditions-data-dictionary
# Retrieve CARTO hexbin US shapefile: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
# Determine how to add centroids to sf object: https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# See {ggtext} textbox options: https://wilkelab.org/ggtext/reference/element_textbox.html
# Add specific limits to scale_fill_viridis_c: https://stackoverflow.com/questions/48424682/how-do-i-limit-the-range-of-the-viridis-colour-scale
# See all named colors in R: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# See known issue with spacing in bold text with {ggtext}: https://stackoverflow.com/questions/72161118/error-rendering-spaces-in-bold-title-using-ggtext-and-ggplot2
# See issue: https://github.com/rstudio/rstudio/issues/7010
# See issue fix: https://github.com/wilkelab/ggtext/issues/83
# See finishing touches possibilities for legend: https://www.cedricscherer.com/slides/OutlierConf2021_ggplot-wizardry.pdf
# See how to change caption position: https://community.rstudio.com/t/changing-position-of-plot-captions/108973/2
# See legend position options: https://r-graph-gallery.com/239-custom-layout-legend-ggplot2.html
# See guide_legend() options: https://ggplot2.tidyverse.org/reference/guide_legend.html
# View remotes package documentation: https://github.com/r-lib/remotes
# See all fontawesome icons using fontawesome::fa_metadata()$icon_names: https://rstudio.github.io/fontawesome/reference/fa.html

## Load packages ----

library(fontawesome)
library(tidyverse)
library(openxlsx)
library(mapview)
library(ggtext)
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

## Join (non-spatial) with MCC data ----

hex_map <- hex_map %>%
  left_join( data, by = c("state_name" = "Bene_Geo_Desc") )


## Retrieve centroid for labelling ----

hex_map <-  cbind(hex_map, st_coordinates(st_centroid(hex_map)))


## Add Google fonts ----

sysfonts::font_add_google(name = "Lato",
                          family = "Lato")

showtext::showtext_auto()  # load the font; must be done every session


## Generate map ----

hex_map <- hex_map %>%
  ggplot() +
  geom_sf(aes(fill = Prvlnc,                                            # indicate col to make chloropleth
              shape = "No data available"),                             # create an override value for NA values
          color = "white") +                                            # change hexbin border colors
  scale_fill_viridis_c(name = "Prevalence of 6+ Chronic Conditions",    # give the legend a name
                       direction = -1,                                  # reverse the scale, so darker equals larger number
                       labels = scales::percent_format(),               # format the numbers in the legend
                       na.value = "gray68") +                           # specify color NA values
  geom_text(aes(x = X,                                                  # specify long of centroid
                y = Y,                                                  # specify lat of centroid
                label = abbrev),                                        # indicate the col to label each hexbin
            color = "white",                                            # change font color
            family = "Open Sans") +                                     # delineate the custom font
  guides(shape = guide_legend(override.aes = list(fill = "gray68",      # add NA value to legend
                                                  color = "white"),     # set border w/in legend
                              order = 2,
                              title = NULL),
         fill = guide_colorbar(title.position = 'top',                                      # move legend title to top
                               title.hjust = .5, title.theme = element_text(size = 9),      # use code to alter viridis color bar from Cédric Scherer
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines'), 
                               order = 1)) +                                                # ensure viridis scale is first
  labs(title = "<b>In 2018, Oklahoma had the highest prevalence of elderly, female Medicare enrollees with 6+ chronic conditions.</b>",
       subtitle = "The prevalence of six or more chronic conditions in Medicare beneficiaries assigned female at birth aged 65 years or older was 20.5% in OK in 2018.",
       caption = "Source: Centers for Medicare & Medicaid Services") +
  theme_void() +
  theme(
    text = element_text(family = "Open Sans"),                              # set the font family for all {ggtext} elements
    plot.title.position = "plot",                                           # help ensure title aligned with plot
    plot.caption.position = "plot",                                         # move caption to be right-aligned with plot
    plot.title = element_textbox_simple( #face = "bold",                     # use {ggtext} to format title; HTML tags will format text; textbox will wrap automatically
                                         margin = margin(0, 0, 3, 0) ),     
    plot.subtitle = element_textbox_simple( margin = margin(0, 0, 6, 0) ),  # note order of margins: top, right, bottom, left
    legend.position = "top",                                                # move legend above map
    legend.text = element_text(size = 8),                                   # size legend title text
    legend.margin = margin(10, 6, 6, 4)                                     # add cushion between subtitle, legend, and map
    ) 





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
  scale_color_manual(values = c("Yes" = "#2D708EFF",            # specify custom scale colors using 'largest' col
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
    plot.title = element_markdown( margin = margin(0, 0, 3, 0) ),     # use {ggtext} to format title; HTML tags will format text;font-weight span style will not work for bolding
    plot.subtitle = element_markdown( margin = margin(0, 0, 6, 0) ),  # note order of margins: top, right, bottom, left
    legend.position = "none"                                          # remove legend
  )



#***********************************************************************************************
# HEATMAP ----


## References ----

# Keep only cols matching specific string: https://www.statology.org/r-select-columns-containing-string/
# https://r-graph-gallery.com/79-levelplot-with-ggplot2.html
# https://www.r-bloggers.com/2022/10/how-to-create-a-heatmap-in-r/
# https://stackoverflow.com/questions/37015632/how-i-can-divide-each-column-by-a-number-in-r
# https://www.r-bloggers.com/2013/01/the-magic-empty-bracket/
# https://www.simonqueenborough.info/R/basic/lessons/lapply_and_sapply.html
# https://tidyr.tidyverse.org/reference/pivot_longer.html
# https://stackoverflow.com/questions/10686054/outlined-text-with-ggplot2
# https://github.com/GuangchuangYu/shadowtext
# https://stackoverflow.com/questions/72557614/define-color-and-radius-of-the-background-in-shadowtextelement-shadowtext
# https://www.kaggle.com/code/kathakaliseth/mcdonald-s-menu-comparative-nutrition-values
# https://stackoverflow.com/questions/57651144/conditional-formatting-of-axis-text-using-ggplot2
# https://stackoverflow.com/questions/37443499/how-to-fix-adjust-the-width-of-each-band-in-ggplot-geom-tile?noredirect=1&lq=1
# https://stackoverflow.com/questions/23897175/adjust-ggplot2-geom-tile-height-and-width


## Load packages and data ----

library(tidyverse)
library(openxlsx)
library(ggtext)
library(here)

path <- here() 

mcd_data <- read_csv( paste0(path, "/data/mcd_menu.csv") ) # use readr instead of base to avoid substitution of spaces for dots


## Keep only columns with % DV ----

keep_cols <- mcd_data %>%
  select( matches("% Daily Value") ) %>%
  names()

keep_cols <- c("Category", "Item", keep_cols) # add two cols to char vector

mcd_data <- mcd_data %>%
  select( all_of(keep_cols) ) # use select helper function to avoid ambiguous-external-vector warning


## Convert to decimals ---

convertPct <- function(column){column/100}

mcd_data <- mcd_data %>%
  mutate(across(`Total Fat (% Daily Value)`:`Iron (% Daily Value)`,
                convertPct))


## Subset data ---- 

breaky_foods <- c("Egg McMuffin", "Egg White Delight", "Sausage McMuffin", "Steak & Egg McMuffin",
                  "Bacon, Egg & Cheese Biscuit (Regular Biscuit)", "Sausage Biscuit (Regular Biscuit)",
                  "Southern Style Chicken Biscuit (Regular Biscuit)", "Steak & Egg Biscuit (Regular Biscuit)",
                  "Bacon, Egg & Cheese McGriddles", "Sausage McGriddles", "Bacon, Egg & Cheese Bagel",
                  "Steak, Egg & Cheese Bagel", "Big Breakfast (Regular Biscuit)", 
                  "Big Breakfast with Hotcakes (Regular Biscuit)", "Hotcakes",
                  "Cinnamon Melts", "Sausage Burrito","Fruit & Maple Oatmeal")

breaky <- mcd_data %>%
  filter(Item %in% breaky_foods)


## Shape data into long format ----

breaky <- breaky %>%
  pivot_longer(cols = !c(Category, Item),
               names_to = "nutrition",
               values_to = "pct_dv")

breaky$Item <- str_replace_all(breaky$Item, "Regular Biscuit", "Regular")

breaky$nutrition <- str_replace_all(breaky$nutrition, " \\(% Daily Value\\)", "") 


## Add Google fonts ----

sysfonts::font_add_google(name = "Open Sans",
                          family = "Open Sans")

showtext::showtext_auto()  # load the font; must be done every session


## Generate heatmap ----

ggplot(data = breaky, 
       aes(x = nutrition,
           y = Item,
           fill = pct_dv)) +
geom_tile(color = "white",                             # add white border
          size = 0.01) +
geom_shadowtext(aes(label = paste0(pct_dv*100, "%")),  # format pct_dv as percent 
                bg.color="gray37",                     # specify color of shadow
                bg.r=0.1,                              # specify radius of shadow
                color = "white",
                size = 2) +
scale_fill_viridis_c(direction = -1) +
scale_x_discrete(expand = c(0, 0)) +                   # remove space between plot and axis
labs(x = "", y = "",                                   # remove titles from x and y axes
     title = "<b>McDonald's 'Big Breakfast' is a high-cholestrol start to the day.<b>",
     subtitle = "Of the breakfast items listed, the 'Big Breakfast' with or without hotcakes<br>has the worst nutritional value.") +
theme_minimal() +
theme(text = element_text(family = "Open Sans",
                          color = "gray27"),
      axis.text.x = element_text(angle = -40,          # angle the x-axis text
                                 vjust = -0.15,
                                 hjust = -0.01),
      plot.title.position = "plot", 
      plot.title = element_markdown( margin = margin(0, 0, 3, 0) ),
      plot.subtitle = element_markdown( margin = margin(0, 0, 6, 0) ),
      legend.position = "none") +
coord_fixed()                                          # keep tiles square



#***********************************************************************************************
# BROADBAND MAP ----


## References ----

# Request Census API Key: https://api.census.gov/data/key_signup.html
# Load Census API Key in Renviron: https://walker-data.com/tidycensus/reference/census_api_key.html
# Learn more in Broadband TidyTuesday repo: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-11/readme.md
# See all {tigris} functions: https://github.com/walkerke/tigris
# Apply lesson learned from Charlie in Mapping with R (R for the Rest of Us)
# View data from Census Bureau's new ACCESS Broadband Dashboard: https://www.census.gov/programs-surveys/community-resilience-estimates/partnerships/ntia/broadband-act.html
# Learn CRS of a given layer: https://r-spatial.github.io/sf/reference/st_crs.html
# Use fonts from flatly theme (e.g., Lato): https://gist.github.com/reywood/11069512
# See Lato, a Google font: https://fonts.google.com/specimen/Lato
# See functions in {scales} for modifying numerical labels: https://scales.r-lib.org/reference/label_number.html


## Load packages ----

library(tidyverse)
library(ggrepel)
library(tigris)
library(here)
library(sf)

path <- here()


## Get data ----

### retrieve data from Census Bureau's ACCESS Dashboard

broadband <- openxlsx::read.xlsx( paste0(path,"/data/county_data_ACCESS_BROADBAND_Dashboard.xlsx") )

### retrieve sf object using {tigris}

ut_counties <- counties(state = "UT", year = 2021) %>%
  rmapshaper::ms_simplify()

### change case

names(ut_counties) <- tolower(names(ut_counties))


## Do quick EDA using skimr ----
# 
# skimr::skim(broadband)
# 
# skimr::skim(ut_counties)


## Merge and clean up data ----

ut_counties_sf <- ut_counties %>%
  left_join(broadband, by = c("geoid" = "GEO_ID")) %>%
  select(-c("lsad", "classfp", "mtfcc", "csafp", "cbsafp", "metdivfp", "funcstat")) 


## Create new sf object for counties comprising "Silicon Slopes" ----

## Utah County geoid: "49049"
## Salt Lake County geoid: "49035"
## Summit County geoid: "49043"

### subset counties 

utah_co_sf <- ut_counties_sf[ut_counties_sf$geoid == "49049", ]
salt_lake_co_sf <- ut_counties_sf[ut_counties_sf$geoid == "49035", ]
summit_co_sf <- ut_counties_sf[ut_counties_sf$geoid == "49043", ]

### union counties; use bind_rows() to retain county boundaries

silicon_slopes_sf <- utah_co_sf %>%
  st_union(salt_lake_co_sf) %>%
  st_union(summit_co_sf) %>%
  select(name.x, geometry)

### change name 

silicon_slopes_sf$name.x <- "Silicon Slopes"


## Test using mapview ----

# ut_counties_sf %>%
#   mapview(zcol = "fcc_broadband_avail)")
# 
# ut_counties_sf %>%
#   mapview(zcol = "broadband_usage")
# 
# mapview(silicon_slopes_sf)


# ## Create layer with point locations for Provo, Salt Lake, and Park City ----
# 
# # Source: https://www.latlong.net/
# 
# city_coordinates <- tribble(
#   ~city, ~long, ~lat,
#   "Salt Lake City", -111.876183, 40.758701,
#   "Provo", 	-111.658531, 40.233845,
#   "Park City", -111.497971, 40.646061
# )
# 
# city_coordinates <- city_coordinates |> st_as_sf(coords = c("long", "lat"),
#                                                  crs = 4269)                 # use st_crs() to check the CRS of a layer 


## Add Google fonts ----

sysfonts::font_add_google(name = "Lato",
                          family = "Lato")

showtext::showtext_auto()  # load the font; must be done every session


## Make map ----

ggplot() +
  geom_sf(data = ut_counties_sf,
          aes(fill = pct_telework_ACS17_21),
          color = "white",                                      # change county borders
          size = 0.5) +                                         # change stroke width
  scale_fill_viridis_c(name = "Percent of Teleworking Workers",
                       direction = -1,
                       label = scales::label_number(suffix = "%")) +
  geom_sf(data = silicon_slopes_sf,
          color = "white",
          size = 1.85,
          alpha = 0) +                        # make layer transparent
  # geom_sf(data = city_coordinates,          # add point locations layer
  #         color = "white",
  #         size = 3) +
  geom_sf_text(data = silicon_slopes_sf,
               aes(label = name.x),
               color = "white",
               size = 6,
               face = "bold",
               nudge_x = 1.55,
               nudge_y = -0.2) +
  guides(fill = guide_colorbar(title.position = 'top',   
                               title.hjust = .5, title.theme = element_text(size = 9),
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines'))) +  
  labs(caption = "Source: 5-Year American Community Survey, U.S. Census Bureau, 2017-21") +
  theme_void() +
  theme(
    text = element_text(family = "Lato"), 
    plot.caption.position = "plot",                            # move caption to be right-aligned with plot
    legend.position = "top",                                   # move legend above map
    legend.text = element_text(size = 8),                      # size legend title text
    legend.margin = margin(10, 6, 6, 4)                        # add cushion between subtitle, legend, and map
  ) 


# There is a glitch with {ggrepel} that causes the map not to render when the point layer is labelled...
# geom_text_repel(data = city_coordinates,
#                 aes(x = long,
#                     y = lat,
#                     label = city)) +
# coord_sf(default_crs = 4269) 
# https://twitter.com/ClausWilke/status/1275938706646618113/photo/2
# https://github.com/tidyverse/ggplot2/issues/3945
# https://stackoverflow.com/questions/74482800/how-to-plot-coordinates-over-a-shape-file-in-r
# The labels appear at the bottom with lims_method = "geometry_bbox" in coord_sf().
# The map does not render at all with default_crs = NULL or crs = 4269 or default_crs = 4269.
# It looks like the GitHub package {ggsflabel} would fix these issues (e.g., see lims_bbox()), but I did not try it: https://yutannihilation.github.io/ggsflabel/index.html.
# Here is another source to consider: https://yutani.rbind.io/post/geom-sf-text-and-geom-sf-label-are-coming/
# Here is another source to consider: https://community.rstudio.com/t/geom-sf-text-change-position-of-only-one-text-label/73419/6
# Here is another source to consider: https://github.com/slowkow/ggrepel/issues/111
# Consider st_point_on_surface().
########### libraries

library(tidyverse)
library(tigris)
library(tidycensus)
library(ggplot2)
library(janitor)
library(splines)
library(drat)
library(readxl)
library(parallel)
library(stormwindmodel)
library(openxlsx)
library(splines)
library(plyr)
library(hurricaneexposuredata)


library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
library(tidyverse)

########## Data

############## Netcdf4 data (hurricane simulation output)

# **  Using old data for now--issues w/ date-time agreement in CLIMADA output

############# REPLACEW WITH EITHER CLIMADA OR CHAZ DATA ONCE DATES ARE RESOLVED


#storm_25yr_sim_raw <- read_csv("../../Downloads/STORM_25_years.csv")
storm_25yr_sim_raw <- read_csv("01_data/STORM_25_years.csv")
# based on headers, already in knots this time

storm_25yr_sim_3hr <- storm_25yr_sim_raw %>%
  clean_names() %>%
  mutate(year = year + 1,
         #hours_base = timestep_3_hourly * 3,
         hours_base = timestep_3_hourly * 3,
         hour = hours_base %% 24,
         day = ((hours_base - hour) / 24) + 1,
         storm_id = paste(storm_number,
                          str_pad(year, width = 4,
                                  side = "left",
                                  pad = 0),
                          sep = "-")
         #wind = wind * 1.94384449 # converting to knots
  ) %>% 
  mutate(date = 
           paste0(str_pad(year, width = 4, side = "left", pad = 0),
                  str_pad(month, width = 2, side = "left", pad = 0),
                  str_pad(day, width = 2, side = "left", pad = 0),
                  str_pad(hour, width = 2, side = "left", pad = 0),
                  "00"
                  
           )) %>%
  rename(
    "latitude" = "lat",
    "longitude" = "lon",
    "wind" = "max_wind_kt"
  ) %>%
  select(storm_id, date, latitude, longitude, wind)



#######

######### County data <-- now that I'm using placeholder storm data, *could* combine at this stage
      # once the date/time issue is corrected, maybe run that process first in a separate script?

# counties (should default to 2020)

us_counties <- tigris::counties(cb = TRUE, progress_bar = FALSE) %>% # To avoid high-res versions
  clean_names()

us_counties <- tidycensus::county_laea
  # preloaded county geometry; hopefully doesn't cause any issues

# Baseline mortality data for >=65 (should double check if there are any later versions)
counties_mort_65 <- read_tsv("01_data/all_cause_mortality_65_plus_county.txt") %>%
  dplyr::select(-Notes) %>%
  clean_names()

# might as well merge all county data this stage, I think

# Setting up county variables for model
  # original approach manually pulled from a downloaded Excel document; think I can get these with the tidycensus package
    # maybe replace tigris should be the same county data

  # some of these will have to come from TC and/or windfield models

##  vmax_sust: modeled maximum sustained wind speed at the population centroid of the county during the TC in m/s (from hurricaneexposuredata package)
##  sust_dur: duration of sustained wind speeds above 20 m/s at the population centroid of the county during the TC (from hurricaneexposuredata package)
##  exposure: total number of TC exposures experienced by the county during 1999-2015 (derived from hurricaneexposuredata package)
##  poverty_prop: proportion of residents in poverty          
##  white_prop: proportion of residents identifying as white
##  owner_occupied_prop: proportion of homes that are owner-occupied
##  age_pct_65_plus_prop: proportion of residents age 65+
##  median_age: county median age
##  population_density: county population density
##  median_house_value: county median house value
##  no_grad_prop: proportion of residents without a high school diploma
##  year: year the TC occurred
##  coastal: whether the county is coastal (0/1)

  # manually getting populations 65+
    # oh, would be downstream, but may need to filter by minimum population

# Sort of a ~'vestigial' step, but pulling in a table of ACS5 metadata 
  # (vs looking up in `tidycensus`) for now
  # not sure whether to stick with 2020 ACS5 data, or the dicennial census (different variable names)

#tidycensus::load_variables(year = 2020, dataset = 'pl')
  # not sure if I'm missing something, but looks like they don't have full data, so stick with ACS5?

acs_vars <- read.xlsx("01_data/ACS2019_Table_Shells.xlsx")

age_pct_65_plus_ids <- acs_vars %>%
  clean_names() %>%
  filter(table_id == "B01001" &
           stub %in% c("65 and 66 years",
                       "67 to 69 years",
                       "70 to 74 years",
                       "75 to 79 years",
                       "80 to 84 years",
                       "85 years and over") ) %>%
  dplyr::select(-data_release)

county_acs_vars <- tidycensus::get_acs(geography = 'county',
                                       variables = c("B06012_001", 
                                                     "B06012_002",
                                                     "B02001_002",
                                                     "B02001_001",
                                                     "B07013_002",
                                                     "B07013_001",
                                                     age_pct_65_plus_ids$unique_id,
                                                     "B01001_001",
                                                     "B01002_001",
                                                     "B25077_001",
                                                     "B06009_002",
                                                     "B06009_001"
                                       ),
                                       year = 2020,
                                       survey = "acs5",
                                       geometry = FALSE)
coastlines <- read.xlsx("01_data/coastline-counties-list.xlsx", colNames = TRUE, startRow = 3) %>% clean_names()


### Temporary (maybe load from a separate, earlier file)
  # Oh, this is just the estimate for # exposures; so *could* keep from historical data

exposure_draft1 <- hurricaneexposuredata::storm_winds %>% 
  mutate(storm_year = str_extract_all(storm_id, "[0-9]{4}")) %>%
  filter(storm_year %in% c(1999:2015) & vmax_sust > 20) %>%
  select(fips, storm_id) %>%
  unique() %>%
  group_by(fips) %>%
  dplyr::summarize(exposure = n())

###

county_acs_vars_bayesian <- county_acs_vars %>%
  select(-moe) %>%
  pivot_wider(names_from = variable,
              values_from = estimate) %>%
  mutate(poverty_prop = B06012_002 / B06012_001,
         white_prop = B02001_002 / B02001_001,
         owner_occupied_prop = B07013_002 / B07013_001,
         age_pct_65_plus_prop = (B01001_020 +
                                   B01001_021 +
                                   B01001_022 +
                                   B01001_023 +
                                   B01001_024 +
                                   B01001_025 +
                                   B01001_044 +
                                   B01001_045 +
                                   B01001_046 +
                                   B01001_047 +
                                   B01001_048 +
                                   B01001_049) / B01001_001,
         median_age = B01002_001,
         median_house_value = B25077_001,
         no_grad_prop = B06009_002 / B06009_001
  ) %>%
  left_join(clean_names(coastlines), by = c("GEOID" = "state_county_fips")) %>%
  left_join(us_counties, by = c("GEOID" = "geoid")) %>%
  mutate(population_density = B01001_001 / (aland * 0.00000038610), 
         coastal = as.numeric(!is.na(coastline_region)),
         year = 2015,
         #year = sample(2006:2015, size = 1, replace = T),
         #exposure = median(modobj$data$exposure)) %>%
  ) %>%
  left_join(exposure_draft1, by = c("GEOID" = "fips")) %>%
  select(GEOID, poverty_prop:no_grad_prop, coastal:exposure, population_density, median_house_value)

# last 3 lines are pulling in storm simulation data; I guess could run that 'upstream'

# model object

load("01_data/modobj.RData")

source("02_code/pred_function.R")
  #both from Dr. Nethery

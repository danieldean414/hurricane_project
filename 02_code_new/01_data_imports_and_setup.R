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
library(sf)
#devtools::install_github("geanders/stormwindmodel", build_vignettes = TRUE) # latest version
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

# Adding Alice's list of ENSO categorizations

enso_years <- read_xlsx("01_data/ENSO JJASON season.xlsx", range = "A1:C36") %>%
  pivot_longer(cols = 1:3, names_to = "phase", values_to = "year") %>% # right term?
  filter(!is.na('value'))  # for some reason only works w/ 'value' in qoutes?

# not sure if we'd need exact ONI values, but 

enso_years_oni <- read_xlsx("01_data/ENSO JJASON season.xlsx", range = "E1:J71") %>%
  clean_names()

# **  Using old data for now--issues w/ date-time agreement in CLIMADA output

############# Loading pre-generated data
  # STORM (structure/contents seem easier to understand, at least)

  # I think this is consoistent for historical and simulated
#########################################
#Entry	Variable name			Unit		Notes on variable
#1	Year				-		Starts at 0
#2	Month				-	
#3	TC number			-		For every year; starts at 0. 
#4	Time step			3-hourly	For every TC; starts at 0.
#5	Basin ID			-		0=EP, 1=NA, 2=NI, 3=SI, 4=SP, 5=WP
#6	Latitude			Deg		Position of the eye.
#7	Longitude			Deg		Position of the eye. Ranges from 0-360°, with prime meridian at Greenwich.
#8	Minimum pressure		hPa	
#9	Maximum wind speed		m/s	
#10	Radius to maximum winds		km	
#11	Category			-		.
#12	Landfall			-		0= no landfall, 1= landfall
#13	Distance to land		km	

###########################################
storm_names <- c('year', 'month', 'tc_number', 'time_step', 
                 'basin_id', 'latitude','longitude',
                 'min_pressure', 'max_wind_speed','rad_to_max_winds',
                 'category','landfall', 'dist_to_land')

# 10k years with historical data:
  # Oh, STORM data is already converted into .tsv files;
    # 60 files, so need to do some kind of loop <-- maybe just an rbind()?

storm_10k_obs_list <- list.files("01_data/storm_10k_sim_v4/STORMV4/VERSIE4/", 
                                 full.names = TRUE, pattern = ".*NA.*") 

  # Wow, was forgetting to pre-filter to North Atlantic!
    # still too big to read in with a direct call, but loop might work now...

  # hopefully not too much memory here...
# storm_10k_obs_all <- rbind(read_csv(storm_10k_obs_list, col_names = FALSE))
  #hmm; 'resource temporarily unavailable'-- assuming it's memory-based?
    # any way I can reduce RAM load (e.g. pre-aggregating)?
    # or else run on server

storm_10k_obs_na <- rbind(read_csv(storm_10k_obs_list, col_names = FALSE))

names(storm_10k_obs_na) <- storm_names

storm_10k_obs_na_proc <- storm_10k_obs_na  %>%
  group_by(tc_number) %>%
  filter(longitude < 290 & latitude > 25) %>%
  filter(sum(landfall) > 1) %>%
  ungroup() %>%
  mutate(year = year + 1,
         #hours_base = timestep_3_hourly * 3,
         hours_base = time_step * 3,
         hour = hours_base %% 24,
         day = ((hours_base - hour) / 24) + 1,
         storm_id = paste(tc_number,
                          str_pad(year, width = 4,
                                  side = "left",
                                  pad = 0),
                          sep = "-"),
         wind = max_wind_speed / 1.852 # converting km/h to knots
  ) %>% 
  mutate(date = 
           paste0(str_pad(year, width = 4, side = "left", pad = 0),
                  str_pad(month, width = 2, side = "left", pad = 0),
                  str_pad(day, width = 2, side = "left", pad = 0),
                  str_pad(hour, width = 2, side = "left", pad = 0),
                  "00"
                  
           ),
         longitude = longitude -360) %>%# also seems to work as-is in some cases?
  select(storm_id, date, latitude, longitude, wind)

# Oh, after a restart just 

# Pulling up processing step to save storage size (might still be too much  'active' memory)
  # not thinking of a non-loop approach at the moment...

#storm_10k_obs_template <- data.frame(matrix(ncol = 13, nrow = 0))
storm_10k_obs_template <- data.frame(matrix(ncol = 5, nrow  = 0))
names(storm_10k_obs_template) <- c("storm_id", "date","latitude",
                                   "longitude", "wind")

storm_10k_obs <- storm_10k_obs_template

for(file in storm_10k_obs_list){
  readin_tmp <- read_csv(file, col_names = FALSE)
  names(readin_tmp) <- storm_names
  
  processed <- readin_tmp %>%
    mutate(year = year + 1,
           #hours_base = timestep_3_hourly * 3,
           hours_base = time_step * 3,
           hour = hours_base %% 24,
           day = ((hours_base - hour) / 24) + 1,
           storm_id = paste(tc_number,
                            str_pad(year, width = 4,
                                    side = "left",
                                    pad = 0),
                            sep = "-"),
           wind = max_wind_speed / 1.852 # converting km/h to knots
    ) %>% 
    mutate(date = 
             paste0(str_pad(year, width = 4, side = "left", pad = 0),
                    str_pad(month, width = 2, side = "left", pad = 0),
                    str_pad(day, width = 2, side = "left", pad = 0),
                    str_pad(hour, width = 2, side = "left", pad = 0),
                    "00"
                    
             )) %>%
    select(storm_id, date, latitude, longitude, wind)
  
  storm_10k_obs <- rbind(storm_10k_obs, processed)
  print(file)
}
  #OK, hit a memory error; should have included some print function to get file #!
  # 16,617,679 rows, so 33ish files

# Weird, after prefiltering to NA, still crashed after only ~1 million rows!

# OK, should be able to feed into existing steps now
  # oh, feeling like setting up on server is definitely seeming plausible
    # yeah, already up to ~2 GB *just loading* 1/6 of the ~pseudo-historical files

# shouldn't need major modifications;


storm_10k_obs_test_3hr <- storm_10k_obs_test %>%
  clean_names() %>%
  mutate(year = year + 1,
         #hours_base = timestep_3_hourly * 3,
         hours_base = time_step * 3,
         hour = hours_base %% 24,
         day = ((hours_base - hour) / 24) + 1,
         storm_id = paste(tc_number,
                          str_pad(year, width = 4,
                                  side = "left",
                                  pad = 0),
                          sep = "-"),
         wind = max_wind_speed / 1.852 # converting km/h to knots
  ) %>% 
  mutate(date = 
           paste0(str_pad(year, width = 4, side = "left", pad = 0),
                  str_pad(month, width = 2, side = "left", pad = 0),
                  str_pad(day, width = 2, side = "left", pad = 0),
                  str_pad(hour, width = 2, side = "left", pad = 0),
                  "00"
                  
           )) %>%
  select(storm_id, date, latitude, longitude, wind)

# Oh, wow! No wonder there are issues! ~5 million rows just here!
    # so would be ~~12 Gb RAM alltogether
    # so ~30 million to process 
  # I wonder how much memory each row holds? b/c 'procesed' version is 5 vs 13 cols 
    # e.g. maybe could process and discard each file and only merge outputs?
  # Oh, learned a new function!
    # object.size()
      # 525366860 bytes (~500 Mb) for 'raw' version, 191257904 (<200 Kb) for above output 
      # oh, so that *is* an appreciable difference! Could fit 2.7x the rows of 'processed' data
        # so ~45%/~4500 years
        # and keep in mind I have >700 Mb between the 2 at ~2 Gb system memory ,so not untenable
########

  # CHAZ--organized in figure-based .nc files


############# REPLACE WITH EITHER CLIMADA OR CHAZ DATA ONCE DATES ARE RESOLVED


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
  filter(storm_year %in% c(1999:2020) & vmax_sust > 20) %>% # double-check that this aligns with current model
  select(fips, storm_id) %>%
  unique() %>%
  group_by(fips) %>%
  dplyr::summarize(exposure = n())

###
  # adding an extra variable for actual population age 65+; don't think that should cause any issues
county_acs_vars_bayesian <- county_acs_vars %>%
  select(-moe) %>%
  pivot_wider(names_from = variable,
              values_from = estimate) %>%
  mutate(poverty_prop = B06012_002 / B06012_001,
         white_prop = B02001_002 / B02001_001,
         owner_occupied_prop = B07013_002 / B07013_001,
         age_65_plus = (B01001_020 +
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
                                   B01001_049),
         age_pct_65_plus_prop = age_65_plus / B01001_001,
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



########### libraries

library(tidyverse)
#library(tigris)
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
#library(splines)
#library(plyr)
library(hurricaneexposuredata)


#library(ncdf4)
#library(ncdf4.helpers)
#library(PCICt)


########## Data

# Counties -- moving up so I can filter by geometry

us_counties <- get_acs(
  geography = "county",
  variables = "B06012_001",
  year = 2020,
  geometry = TRUE
) 

# includes Alaska, Hawai'i, etc. -- should I filter ahead of time?
    # could use historical tracks, although I think that would be a bit reductive
    # Oh, could use the coastal county lists
      # BUT would have to think through how to avoid excluding inland storms
        # could filter to 
      # 
  # Nice--already in a flat projection so *could* use explicit geo. operations

# OK, all continental states are in the hurricane data

#hurricaneexposuredata::county_centers %>% left_join(us_counties, by = c("fips" = "GEOID")) %>% ggplot() + aes(geometry = geometry) + geom_sf()


######### NOT CURRENLTY USING; <-- now seems to be xausing a crash
# OK, and counties w/ recorded exposures seems decently ~inclusive
#us_counties_buffer <- hurricaneexposuredata::storm_winds %>% 
#  dplyr::select(fips) %>%
#  unique() %>%
#  left_join(us_counties, by = c("fips" = "GEOID")) %>%
#  st_as_sf() %>%
#  st_union() %>%
#  st_buffer(250000)   # I think the units are in meters, so 250 km should be 250000 m
      # at least 'looks right' for what I'd expect 250 miles to look like


coastlines <- read.xlsx("01_data/coastline-counties-list.xlsx", colNames = TRUE, startRow = 3) %>% clean_names()


  

#us_counties <- tidycensus::county_laea %>%
#  filter(!str_detect(GEOID, "02\\d{3}|15\\d{3}")) # removing AK, HI 

# maybe crude, but one way to get a bit more filtering w/o explicit geometry:

#us_counties_fl_me <- us_counties %>%
#  filter(str_detect(GEOID, "12\\d{3}|23\\d{3}"))

# need to add the `area` column, but otherwise OK
# check default unit there; I think it's still m^2
# preloaded county geometry; hopefully doesn't cause any issues

# trying to remember if we can just use data from `modobj` 


#us_counties_nad83 <- st_transform(us_counties, crs = "NAD83")
us_counties_wgs84  <- st_transform(us_counties, crs = "WGS84")

# OK, this includes islands; filtering down to just Atlantic, GoM, coastsL

us_counties_wgs84_atlantic <- (us_counties_wgs84 %>% 
    filter(GEOID %in% (coastlines %>%
                         filter(coastline_region %in% 
                                  c("Atlantic", "Gulf of Mexico")))$state_county_fips))

#sf_use_s2(FALSE) # turns off spherical geometry; apparently causing issues /w buffer?

#us_counties_wgs84_merged_buffer <- us_counties_wgs84 %>%
#  st_union() %>%
#  st_buffer(dist = (250 / 111.32)) # arc degree by default; these are 111.32 km

############## Netcdf4 data (hurricane simulation output)

# Adding Alice's list of ENSO categorizations

#enso_years <- read_xlsx("01_data/ENSO JJASON season.xlsx", range = "A1:C36") %>%
#  pivot_longer(cols = 1:3, names_to = "phase", values_to = "year") %>% # right term?
#  filter(!is.na('value'))  # for some reason only works w/ 'value' in qoutes?

# not sure if we'd need exact ONI values, but 

#enso_years_oni <- read_xlsx("01_data/ENSO JJASON season.xlsx", range = "E1:J71") %>%
#  clean_names()

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

# confused by tc_number
  # e.g. I see maybe 4-5 "tc 0's" for year 0, each in a different month
  # am I somehow changing the years?
    # ohhh--I think I got it: each block of 1000 years restarts at "0"
      # can't think of any downsides to just adding 1000 per file?
        # could do in loop; trying to think how to approach otherwise
    # wasn't coming up before because I was testing with <1000 years

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

# trying one of the climate change models
storm_10k_obs_list <- list.files("01_data/storm_10k_sim_v4/STORMV4/VERSIE4/", 
                                 full.names = TRUE, pattern = ".*NA.*") 

###########################################################################
########## OK, need to work out issues with R env. on ALPINE;
  # let's use say 1000 years for now?

storm_10k_obs_list <- storm_10k_obs_list[9]
  # trying next 1000 to see how similar the subsets are

#############################################################################

  # Wow, was forgetting to pre-filter to North Atlantic!
    # still too big to read in with a direct call, but loop might work now...

  # hopefully not too much memory here...
# storm_10k_obs_all <- rbind(read_csv(storm_10k_obs_list, col_names = FALSE))
  #hmm; 'resource temporarily unavailable'-- assuming it's memory-based?
    # any way I can reduce RAM load (e.g. pre-aggregating)?
    # or else run on server

# OK, I guess need to convert to geometry objects and then back if I'm using an
  # explicit 250km buffer

storm_10k_obs_na <- (read_csv(storm_10k_obs_list, col_names = FALSE))
  # trying to think through an efficient way to add some index for set of years
  # seem to have hit memory limit (~3 GB); doesn't seem to let me read them in again
  
# try:
#storm_10k_obs_na <- dplyr::bind_rows(read_csv(storm_10k_obs_list, col_names = FALSE), .id = 'year_set')
  # oh, turns out `rbind` is superfluous, so already just one dataframe

## Setting up a new version:
  #-prefiltering to 64 knots and/or 32.9 m/s (reach at least this speed)
  # Hoenstly just that might drop it to a manageable amount
  # and then either use st_as_sf() + buffer, or some kind of bounding box 
    # any way to get diagnoals, etc., w/o geometry?

names(storm_10k_obs_na) <- storm_names

# OK, even w/ a lot of extra storms off to SE, hurricanes + <=250km cuts it down to ~10% of orig. 
storm_10k_obs_na_proc_hurricane <- storm_10k_obs_na  %>%
  mutate(change = (as.numeric(year - lag(year) == -999))) %>% 
  mutate(change = ifelse(is.na(change), 0, change)) %>% 
  mutate(year_set = cumsum(change)) %>%
  #mutate(year = year + year_set*1000) %>% # realized I can just add set # to the storm ID
  dplyr::select(-change) %>%
  group_by(year, tc_number, year_set) %>%
  mutate(max_max_wind_speed = max(max_wind_speed)) %>%
  filter(max_max_wind_speed >= 32.9) %>% 
  mutate(longitude = longitude - 360) %>%
  #st_as_sf(coords = c('longitude', 'latitude'),
  #         crs = st_crs(us_counties)) %>%
  #filter(st_intersects(., st_buffer(us_counties_wgs84, 250000)))
  mutate(us_contact = (longitude < st_bbox(us_counties_wgs84_atlantic)$xmax + (250 / 111.32) &
                         latitude > st_bbox(us_counties_wgs84_atlantic)$ymin - (250 / 111.32))) %>%
  #mutate(us_contact = st_is_within_distance(., st_union(us_counties), dist = 250000 )) %>%
  #mutate(us_contact = st_intersects(geometry, us_counties_buffer)) %>%
  #filter((longitude < 290 & latitude > 25)) %>%
  group_by(year, tc_number, year_set) %>%
  dplyr::filter(sum(landfall) > 1) %>%
  dplyr::mutate(cross = (us_contact - lag(us_contact) != 0)) %>% 
  mutate(cross = ifelse(is.na(cross), 0, cross)) %>%
  dplyr::mutate(cross_sum = cumsum(cross),
                max_cross_sum = max(cross_sum)) %>%
  filter(!(us_contact == FALSE & 
             cross_sum == 0) &
           !(us_contact == FALSE &
               cross_sum == max_cross_sum)) %>%
  #add_tally() %>%
  #filter(n > 2) %>%
  filter(sum(us_contact) > 1 & sum(landfall) > 1) %>%
  ungroup() %>%
  mutate(wind = max_wind_speed * 1.9438444924) %>% # converting m/s to knots
  mutate(year = year + 1,
         storm_id = paste(tc_number,
                          str_pad(year, width = 4,
                                  side = "left",
                                  pad = 0), 
                          month,
                          year_set,
                          sep = "-")) %>%
  mutate(start_ymd_hm = paste0(str_pad(year, width = 4, side = "left", pad = 0),
                               str_pad(month, width = 2, side = "left", pad = 0),
                               "010000"),
         date = lubridate::ymd_hm(start_ymd_hm) + time_step * 3 * 60 * 60, # 3-hour units
         date = (as.character(date) %>%
           str_remove_all("\\D") %>%
           substr(1, nchar(.) -2))
         ) %>% #,
  #longitude = longitude -360) %>%# also seems to work as-is?
  #select(storm_id, date, latitude, longitude, wind)
  select(storm_id, date, wind, latitude, longitude) # not sure if this will cause problems

#st_intersection(storm_10k_obs_na_proc_hurricane, st_buffer(us_counties_wgs84, ))

## historical version

storm_hist_proc_hurricane <- hurricaneexposuredata::hurr_tracks  %>%
  #mutate(change = (as.numeric(year - lag(year) == -999))) %>% 
  #mutate(change = ifelse(is.na(change), 0, change)) %>% 
  #mutate(year_set = cumsum(change)) %>%
  #mutate(year = year + year_set*1000) %>% # realized I can just add set # to the storm ID
  #dplyr::select(-change) %>%
  #group_by(year, tc_number, year_set) %>%
  mutate(max_max_wind_speed = max(wind)) %>%
  filter(max_max_wind_speed >= 64) %>%  # equivalent to 32.9 m/s, I think
  mutate(longitude = longitude - 360) %>%
  #st_as_sf(coords = c('longitude', 'latitude'),
  #         crs = st_crs(us_counties)) %>%
  #filter(st_intersects(., st_buffer(us_counties_wgs84, 250000)))
  mutate(us_contact = (longitude < st_bbox(us_counties_wgs84_atlantic)$xmax + (250 / 111.32) &
                         latitude > st_bbox(us_counties_wgs84_atlantic)$ymin - (250 / 111.32))) %>%
  #mutate(us_contact = st_is_within_distance(., st_union(us_counties), dist = 250000 )) %>%
  #mutate(us_contact = st_intersects(geometry, us_counties_buffer)) %>%
  #filter((longitude < 290 & latitude > 25)) %>%
  #group_by(year, tc_number, year_set) %>%
  group_by(storm_id) %>%
  #dplyr::filter(sum(landfall) > 1) %>% # no landfall ID, but assuming this is equivalent?
  dplyr::mutate(cross = (us_contact - lag(us_contact) != 0)) %>% 
  mutate(cross = ifelse(is.na(cross), 0, cross)) %>%
  dplyr::mutate(cross_sum = cumsum(cross),
                max_cross_sum = max(cross_sum)) %>%
  filter(!(us_contact == FALSE & 
             cross_sum == 0) &
           !(us_contact == FALSE &
               cross_sum == max_cross_sum)) %>%
  add_tally() %>%
  filter(n > 2) %>%
  filter(sum(us_contact) > 1) %>% # & sum(landfall) > 1) %>%
  ungroup() 


###################################################3
    # Trying full 10k years
###################################################

  # have to reset b/c I'm overwriting /w subset
storm_10k_obs_list <- list.files("01_data/storm_10k_sim_v4/STORMV4/VERSIE4/", 
                                 full.names = TRUE, pattern = ".*NA.*") 

# trying one of the climate change models
storm_10k_obs_list <- list.files("01_data/storm_10k_sim_v4/STORMV4/VERSIE4/", 
                                 full.names = TRUE, pattern = ".*NA.*") 


storm_10k_obs_list <- storm_10k_obs_list
# trying next 1000 to see how similar the subsets are

#############################################################################

# Wow, was forgetting to pre-filter to North Atlantic!
# still too big to read in with a direct call, but loop might work now...

# hopefully not too much memory here...
# storm_10k_obs_all <- rbind(read_csv(storm_10k_obs_list, col_names = FALSE))
#hmm; 'resource temporarily unavailable'-- assuming it's memory-based?
# any way I can reduce RAM load (e.g. pre-aggregating)?
# or else run on server

# OK, I guess need to convert to geometry objects and then back if I'm using an
# explicit 250km buffer

storm_10k_obs_na <- (read_csv(storm_10k_obs_list, col_names = FALSE))
# trying to think through an efficient way to add some index for set of years
# seem to have hit memory limit (~3 GB); doesn't seem to let me read them in again

# try:
#storm_10k_obs_na <- dplyr::bind_rows(read_csv(storm_10k_obs_list, col_names = FALSE), .id = 'year_set')
# oh, turns out `rbind` is superfluous, so already just one dataframe

## Setting up a new version:
#-prefiltering to 64 knots and/or 32.9 m/s (reach at least this speed)
# Hoenstly just that might drop it to a manageable amount
# and then either use st_as_sf() + buffer, or some kind of bounding box 
# any way to get diagnonals, etc., w/o geometry?

names(storm_10k_obs_na) <- storm_names

# OK, even w/ a lot of extra storms off to SE, hurricanes + <=250km cuts it down to ~10% of orig. 
storm_10k_all_obs_na_proc_hurricane <- storm_10k_obs_na  %>%
  mutate(change = (as.numeric(year - lag(year) == -999))) %>% 
  mutate(change = ifelse(is.na(change), 0, change)) %>% 
  mutate(year_set = cumsum(change)) %>%
  #mutate(year = year + year_set*1000) %>% # realized I can just add set # to the storm ID
  dplyr::select(-change) %>%
  group_by(year, tc_number, year_set) %>%
  mutate(max_max_wind_speed = max(max_wind_speed)) %>%
  filter(max_max_wind_speed >= 32.9) %>% 
  mutate(longitude = longitude - 360) %>%
  #st_as_sf(coords = c('longitude', 'latitude'),
  #         crs = st_crs(us_counties)) %>%
  #filter(st_intersects(., st_buffer(us_counties_wgs84, 250000)))
  mutate(us_contact = (longitude < st_bbox(us_counties_wgs84)$xmax + (250 / 111.32) &
                         latitude > st_bbox(us_counties_wgs84)$ymin - (250 / 111.32))) %>%
  #mutate(us_contact = st_is_within_distance(., st_union(us_counties), dist = 250000 )) %>%
  #mutate(us_contact = st_intersects(geometry, us_counties_buffer)) %>%
  #filter((longitude < 290 & latitude > 25)) %>%
  group_by(year, tc_number, year_set) %>%
  dplyr::filter(sum(landfall) > 1) %>%
  dplyr::mutate(cross = (us_contact - lag(us_contact) != 0)) %>% 
  mutate(cross = ifelse(is.na(cross), 0, cross)) %>%
  dplyr::mutate(cross_sum = cumsum(cross),
                max_cross_sum = max(cross_sum)) %>%
  filter(!(us_contact == FALSE & 
             cross_sum == 0) &
           !(us_contact == FALSE &
               cross_sum == max_cross_sum)) %>%
  add_tally() %>%
  filter(n > 2) %>%
  filter(sum(us_contact) > 1 & sum(landfall) > 1) %>%
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
                          year_set,
                          sep = "-"),
         #wind = max_wind_speed / 1.852 # converting km/h to knots
         wind = max_wind_speed * 1.9438444924 # converting m/s to knots
  ) %>% 
  mutate(date = 
           paste0(str_pad(year, width = 4, side = "left", pad = 0),
                  str_pad(month, width = 2, side = "left", pad = 0),
                  str_pad(day, width = 2, side = "left", pad = 0),
                  str_pad(hour, width = 2, side = "left", pad = 0),
                  "00"
                  
           )) %>% #,
  #longitude = longitude -360) %>%# also seems to work as-is?
  #select(storm_id, date, latitude, longitude, wind)
  select(storm_id, date, wind, latitude, longitude) # not sure if this will cause problems


save(storm_10k_all_obs_na_proc_hurricane, file = "01_data/storm_10k_all_obs_na_proc_hurricane.rda")
#st_intersection(storm_10k_obs_na_proc_hurricane, st_buffer(us_counties_wgs84, ))


####################################################

####################################################




#########################3
names(storm_10k_obs_na) <- storm_names

storm_10k_obs_na_proc <- storm_10k_obs_na  %>%
  mutate(change = (as.numeric(year - lag(year) == -999))) %>% 
  mutate(change = ifelse(is.na(change), 0, change)) %>% 
  mutate(year_set = cumsum(change)) %>%
  #mutate(year = year + year_set*1000) %>% # realized I can just add set # to the storm ID
  dplyr::select(-change) %>%
  mutate(us_contact = (longitude < 294 & latitude > 19.63)) %>%
  #filter((longitude < 290 & latitude > 25)) %>%
  group_by(year, tc_number, year_set) %>%
  dplyr::filter(sum(landfall) > 1) %>%
  dplyr::mutate(cross = (us_contact - lag(us_contact) != 0)) %>% 
  mutate(cross = ifelse(is.na(cross), 0, cross)) %>%
  dplyr::mutate(cross_sum = cumsum(cross),
                max_cross_sum = max(cross_sum)) %>%
  filter(!(us_contact == FALSE & 
             cross_sum == 0) &
           !(us_contact == FALSE &
               cross_sum == max_cross_sum)) %>%
  add_tally() %>%
  filter(n > 2) %>%
  #filter(sum(us_contact) > 1 & sum(landfall) > 1) %>%
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
                          year_set,
                          sep = "-"),
         #wind = max_wind_speed / 1.852 # converting km/h to knots
         wind = max_wind_speed * 1.9438444924 # converting m/s to knots
  ) %>% 
  mutate(date = 
           paste0(str_pad(year, width = 4, side = "left", pad = 0),
                  str_pad(month, width = 2, side = "left", pad = 0),
                  str_pad(day, width = 2, side = "left", pad = 0),
                  str_pad(hour, width = 2, side = "left", pad = 0),
                  "00"
                  
           )) %>% #,
         #longitude = longitude -360) %>%# also seems to work as-is?
  select(storm_id, date, latitude, longitude, wind)


# Retaining all data
storm_10k_obs_na_all_proc <- storm_10k_obs_na  %>%
  mutate(change = (as.numeric(year - lag(year) == -999))) %>% 
  mutate(change = ifelse(is.na(change), 0, change)) %>% 
  mutate(year_set = cumsum(change)) %>%
  #mutate(year = year + year_set*1000) %>% # realized I can just add set # to the storm ID
  dplyr::select(-change) %>%
  #mutate(us_contact = (longitude < 294 & latitude > 19.63)) %>%
  #filter((longitude < 290 & latitude > 25)) %>%
  group_by(year, tc_number, year_set) %>%
  dplyr::filter(sum(landfall) > 1) %>%
  #dplyr::mutate(cross = (us_contact - lag(us_contact) != 0)) %>% 
  #mutate(cross = ifelse(is.na(cross), 0, cross)) %>%
  #dplyr::mutate(cross_sum = cumsum(cross),
  #              max_cross_sum = max(cross_sum)) %>%
  #filter(!(us_contact == FALSE & 
  #           cross_sum == 0) &
  #         !(us_contact == FALSE &
  #             cross_sum == max_cross_sum)) %>%
  add_tally() %>%
  filter(n > 2) %>%
  #filter(sum(us_contact) > 1 & sum(landfall) > 1) %>%
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
                          year_set,
                          sep = "-"),
         #wind = max_wind_speed / 1.852 # converting km/h to knots
         wind = max_wind_speed * 1.9438444924 # converting m/s to knots
  ) %>% 
  mutate(date = 
           paste0(str_pad(year, width = 4, side = "left", pad = 0),
                  str_pad(month, width = 2, side = "left", pad = 0),
                  str_pad(day, width = 2, side = "left", pad = 0),
                  str_pad(hour, width = 2, side = "left", pad = 0),
                  "00"
                  
           )) %>% #,
  #longitude = longitude -360) %>%# also seems to work as-is?
  select(storm_id, date, latitude, longitude, wind)

# Alright, probably not the cleanest, but think I have the right years!
  # ohh--is the date format set up to recognize 5-digit years? I guess I'll find out

# OK, think I can remove the rest of the hurricane data
  # *do* need to keep the county data, etc.
###########################################################################################################


###############################################################################################################

######### County data <-- now that I'm using placeholder storm data, *could* combine at this stage
      # once the date/time issue is corrected, maybe run that process first in a separate script?

# counties (should default to 2020)

#us_counties <- tigris::counties(cb = TRUE, progress_bar = FALSE) %>% # To avoid high-res versions
#  clean_names()



# Baseline mortality data for >=65 (should double check if there are any later versions)
counties_mort_65 <- read_tsv("01_data/Multiple Cause of Death, 2018-2021, Single Race_2019_only.txt") %>%
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

acs_vars <- read_xlsx("01_data/ACS2019_Table_Shells.xlsx")

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
  # did I delete a line here? Piped straight into the saving step

save(county_acs_vars, file = "01_data/county_acs_vars.rda") # just in case I need to reload offline



#####################################################################################################
  ## Probably could remove ~functionally, but could be useful for methods

# Adding a step to help keep track of where all the 'NA' results are coming from downstream
  # *possible* it's just replication of these
# variables are:
  # B06009_001/2: used to estimate % with HS degree
  # B06012_001/2: % below poverty
  # B07013_1/2: % in ownder-occupied housing
  # B25077: Median housing value (USD)

#######################################333


# OK, let's remove those and see if that stops the propagation of NAs

us_counties %>% mutate(missing_data = GEOID %in% unique(county_acs_vars_na$GEOID)) %>%
  ggplot() + aes(geometry = geometry, fill = missing_data) %>% geom_sf(color = NA) + theme_void() 

county_acs_vars_na <- county_acs_vars %>% filter(is.na(estimate))
county_acs_vars_moe_na <- county_acs_vars %>% filter(is.na(moe))

# OK, 78 missing counties for everything but B25077_001, which only has 5 missing


# That's odd--so I see the 78 unique IDs for missing data
unique(county_acs_vars_na$GEOID)
# but if I try matching it to `us_counties`, I only get 5:
us_counties %>% 
  mutate(missing_data = GEOID %in% unique(county_acs_vars_na$GEOID)) %>%
  filter(missing_data == TRUE)

# ohh-that makes sense; it's that the majority are Puerto Rico's counties
# mapping remaining counties--looks like 4 in continental US, maybe 1 that's realistically exposed 

# ~non-territorial US counties are:
# Kalawao County, Hawaii  (?)
# Buffalo County, South Dakota: pop 1,948       
# Mellette County, South Dakota: pop 1,918
# Kenedy County, Texas: pop ~350
# Loving County, Texas: pop ~64

#######################################################################################
#######################################################################################

### Temporary (maybe load from a separate, earlier file)
  # Oh, this is just the estimate for # exposures; so *could* keep from historical data

exposure_draft1 <- hurricaneexposuredata::storm_winds %>% 
  mutate(storm_year = str_extract_all(storm_id, "[0-9]{4}")) %>%
  filter(storm_year %in% c(1999:2020) & vmax_sust > 17.4) %>% # double-check that this aligns with current model
  select(fips, storm_id) %>%
  unique() %>%
  group_by(fips) %>%
  dplyr::summarize(exposure = n()) %>%
  mutate(exposure = replace_na(exposure, 1)) # maybe 0? Also out of training data's range

  # need to decide how to handle 

###
  # adding an extra variable for actual population age 65+; don't think that should cause any issues
    # didn't seem to; another for mortality rates
    # also has full county names--I guess more convenient
county_acs_vars_bayesian <- county_acs_vars %>%
  filter(!is.na(estimate)) %>%
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
  inner_join(us_counties) %>% #, by = c("GEOID" = "geoid")) %>%
  filter(!is.na(median_house_value)) %>%
  mutate(aland = as.numeric(st_area(geometry))) %>%
  mutate(population_density = B01001_001 / (aland * 0.00000038610), # looks like m^2 --> miles^2
         coastal = as.numeric(!is.na(coastline_region)),
         year = 2015, # why is year 2015?
         #year = sample(2006:2015, size = 1, replace = T),
         #exposure = median(modobj$data$exposure)) %>%
  ) %>%
  left_join(exposure_draft1, by = c("GEOID" = "fips")) %>%
  select(GEOID, poverty_prop:no_grad_prop, coastal:exposure, population_density, median_house_value) %>%
  mutate(exposure = replace_na(exposure, 0)) %>% # 1 or 0? (0 seems to be 'out of scope')
  left_join(counties_mort_65, by = c("GEOID" = "county_code"))

# Huh--somehow gives a few "infinite" population densities; maybe 0 people??
  # *Still 5 NA's for median house vale, 78 for no_grad_pop, etc.
  # so still getting those NA's; make sure I saved filtered version 
    # might be pulling in somewhere else?
    # oh, maybe it's that those values are *missing* 

# Using a `right_join` operation yeilds up to 9 NAs; 4 for most
  # All dated 2015

# 46113: ~retired code for Shannon County, SD (FIPS code = 46113); 
  # renamed to Oglala Lakota County (FIPS 46102).          
# 51515: Bedford City County, VA
# 02261: Valdez-Cordova County, AK        
# 02270: Wade Hampton Census Area 02270 FIPS Code, Alaska 
  # possibly renamed to Kusilvak?

  #!! Not sure how it will handle unexposed counties (exposure = NA); I guess a bit ~tautologically, only exposed counties
    # are in the `modobj` dataset
    # maybe filter out? Although seems possible that there'd be impacts on countied not in the 
    # 1999-2015 window
    # or bump up to one; arbitrary, but I'd guess 1 and 0 impacts would look similar?
      # could put in '0', but don't know if that would cause extrapolation issues

# last 3 lines are pulling in storm simulation data; I guess could run that 'upstream'

# model object

load("02_code/modobj.RData")

source("02_code_new/pred_function.R")
  #both from Dr. Nethery



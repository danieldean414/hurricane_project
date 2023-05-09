## Libraries

#library(tidyverse)
library(janitor)
library(ggplot2)
library(stormwindmodel)
library(Hmisc)
library(tigris)
library(stringr)
library(readr)
library(purrr)
library(dplyr)
library(tidyverse)
library(openxlsx)


#### input data (for summit)


# copying from github; seems like there might be version differences on Y260?

us_counties <- tigris::counties(cb = TRUE, progress_bar = FALSE) # To avoid high-res versions




storm_sim_col_names <- c('Year',
                         'Month',
                         'Storm number',
                         'Timestep (3-hourly)',
                         'Lat',
                         'Lon',
                         'Max wind (kt)',
                         'Min Pres (hPa)')


#Negative

#amo_neg_250 <- read_csv("Neg_250_years.csv", col_names = FALSE)
amo_neg_250 <- read_csv("../tc_exposure/Synthetic_data/Neg_250_years_CONUS.csv", col_names = FALSE)

colnames(amo_neg_250) <- storm_sim_col_names


# cleaning format


amo_neg_250_clean <- amo_neg_250 %>%
  #filter(Year <= 20) %>% # why was I filtering year here?
  clean_names() %>%
  mutate(year = year + 1,
         storm_id = paste(storm_number,str_pad(year, width = 4,
                                               side = "left",
                                               pad = 0),sep = "-"),
         date = ( (lubridate::as_datetime(
           paste0(
             "1", str_pad(year, width = 3, side = 'left', pad = 0),
             "-",
             str_pad(month, width = 2, side = 'left', pad = 0),
             "-01"
           )
         )) + lubridate::hours(timestep_3_hourly * 3))
  ) %>%
  mutate(date = as.character(date), 
         date = str_sub(date, start = 0, end = 16) ) %>%
  rename( "latitude" = "lat",
          "longitude" = "lon",
          "wind" = "max_wind_kt") %>%
  select(storm_id, date, latitude, longitude, wind)

##
## Removing storms with less than 3 events recorded (~70)

amo_neg_250_clean <- amo_neg_250_clean %>%
  group_by(storm_id) %>%
  nest() %>%
  mutate(length_events = purrr::map(.x = data, .f = ~nrow(.x))) %>%
  unnest(length_events) %>%
  arrange(desc(length_events)) %>%
  unnest(data) %>%
  filter(length_events > 1) %>% 
  select(-length_events)

# Positive


#amo_pos_250 <- read_csv("pos_250_years.csv", col_names = FALSE)
amo_pos_250 <- read_csv("../tc_exposure/Synthetic_data/pos_250_years_CONUS.csv", col_names = FALSE)

colnames(amo_pos_250) <- storm_sim_col_names


# cleaning format


amo_pos_250_clean <- amo_pos_250 %>%
  #filter(Year <= 20) %>% # why was I filtering year here?
  clean_names() %>%
  mutate(year = year + 1,
         storm_id = paste(storm_number,str_pad(year, width = 4,
                                               side = "left",
                                               pad = 0),sep = "-"),
         date = ( (lubridate::as_datetime(
           paste0(
             "1", str_pad(year, width = 3, side = 'left', pad = 0),
             "-",
             str_pad(month, width = 2, side = 'left', pad = 0),
             "-01"
           )
         )) + lubridate::hours(timestep_3_hourly * 3))
  ) %>%
  mutate(date = as.character(date), 
         date = str_sub(date, start = 0, end = 16) ) %>%
  rename( "latitude" = "lat",
          "longitude" = "lon",
          "wind" = "max_wind_kt") %>%
  select(storm_id, date, latitude, longitude, wind)

##


amo_pos_250_clean <- amo_pos_250_clean %>%
  group_by(storm_id) %>%
  nest() %>%
  mutate(length_events = purrr::map(.x = data, .f = ~nrow(.x))) %>%
  unnest(length_events) %>%
  arrange(desc(length_events)) %>%
  unnest(data) %>%
  filter(length_events > 1) %>% 
  select(-length_events)



#All

#amo_all_250 <- read_csv("all_250_years.csv", col_names = FALSE)
amo_all_250 <- read_csv("../tc_exposure/Synthetic_data/all_250_years_CONUS.csv", col_names = FALSE)

colnames(amo_all_250) <- storm_sim_col_names


# cleaning format


amo_all_250_clean <- amo_all_250 %>%
  #filter(Year <= 20) %>% # why was I filtering year here?
  clean_names() %>%
  mutate(year = year + 1,
         storm_id = paste(storm_number,str_pad(year, width = 4,
                                               side = "left",
                                               pad = 0),sep = "-"),
         date = ( (lubridate::as_datetime(
           paste0(
             "1", str_pad(year, width = 3, side = 'left', pad = 0),
             "-",
             str_pad(month, width = 2, side = 'left', pad = 0),
             "-01"
           )
         )) + lubridate::hours(timestep_3_hourly * 3))
  ) %>%
  mutate(date = as.character(date), 
         date = str_sub(date, start = 0, end = 16) ) %>%
  rename( "latitude" = "lat",
          "longitude" = "lon",
          "wind" = "max_wind_kt") %>%
  select(storm_id, date, latitude, longitude, wind)

##
## Removing storms with less than 3 events recorded (~70)

amo_all_250_clean <- amo_all_250_clean %>%
  group_by(storm_id) %>%
  nest() %>%
  mutate(length_events = purrr::map(.x = data, .f = ~nrow(.x))) %>%
  unnest(length_events) %>%
  arrange(desc(length_events)) %>%
  unnest(data) %>%
  filter(length_events > 1) %>% 
  select(-length_events)


#######


load("01_data/amo_all_250_clean_get_grid_alt_date_CONUS.rda")
load("01_data/amo_neg_250_clean_get_grid_alt_date_CONUS.rda")
load("01_data/amo_pos_250_clean_get_grid_alt_date_CONUS.rda")

# corresponds to amo_<>_250_clean_get_grid, I think

head(amo_all_250_clean_get_grid)

# copying in last few lines of code after crash (should be fixed now, but seemed easier to rerun here)

# all



amo_all_250_clean_get_grid_TEST_comb <- do.call("rbind", amo_all_250_clean_get_grid)

amo_all_250_clean_get_grid_TEST_comb <- amo_all_250_clean_get_grid_TEST_comb %>%
  rownames_to_column("storm_id") %>%
  mutate(storm_id = str_extract(storm_id,
                                "[0-9]-[0-9]*"))


# negative 
amo_neg_250_clean_get_grid_TEST_comb <- do.call("rbind", amo_neg_250_clean_get_grid)

amo_neg_250_clean_get_grid_TEST_comb <- amo_neg_250_clean_get_grid_TEST_comb %>%
  rownames_to_column("storm_id") %>%
  mutate(storm_id = str_extract(storm_id,
                                "[0-9]-[0-9]*"))


# positive 
amo_pos_250_clean_get_grid_TEST_comb <- do.call("rbind", amo_pos_250_clean_get_grid)

amo_pos_250_clean_get_grid_TEST_comb <- amo_pos_250_clean_get_grid_TEST_comb %>%
  rownames_to_column("storm_id") %>%
  mutate(storm_id = str_extract(storm_id,
                                "[0-9]-[0-9]*"))

# Merging (not positive this is the most efficient approach; should be able to iterate otherwhise)

amo_merged_250_clean_get_grid <- (amo_pos_250_clean_get_grid_TEST_comb %>% mutate(scenario = "positive")) %>%
  full_join((amo_neg_250_clean_get_grid_TEST_comb %>% mutate(scenario = "negative"))) %>%
  full_join((amo_all_250_clean_get_grid_TEST_comb %>% mutate(scenario = "all"))) %>%
  group_by(storm_id, gridid) %>%
  filter(vmax_sust == max(vmax_sust)) %>% 
  arrange(storm_id, gridid)

## Not positive why some storms generate 2 impacts; needs more investigation/attention (e.g. would addition be the more appropriate response?)


## Adding more-precise chronological data

  # I think I'm fine on exposures  w/ T480
  
  # Anything else from this?

###################3333  Predict function (pasting in to maybe alleviate dependencies)

load('02_code/modobj.RData')
#load('modobj.RData')


## function to predict (should work with predict() in R)
predict.tcExcessMort<-function(modobj=modobj, newdata){
  ##  newdata should be a data frame with county/storm events in the rows and the following variables in the columns
  ##  note that all variables should be un-transformed-- any necessary transformations will be performed within the function
  ##  all variables should be numeric, with the coastal counties variable coded as binary 0/1
  
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
  
  newdata$population_density<-(newdata$population_density-modobj$popdens_scale[1])/modobj$popdens_scale[2]
  newdata$median_house_value<-(newdata$median_house_value-modobj$medhouseval_scale[1])/modobj$medhouseval_scale[2]
  newdata$year<-(newdata$year-modobj$year_scale[1])/modobj$year_scale[2]
  
  ## transform covariates as needed for the model ##
  X<-model.matrix(~rcspline.eval(vmax_sust,inclx = T,knots=modobj$vmaxsust_knots)+
                    sust_dur+exposure+poverty_prop+white_prop+owner_occupied_prop+
                    age_pct_65_plus_prop+median_age+population_density+median_house_value+
                    no_grad_prop+rcspline.eval(year,inclx = T,knots=modobj$year_knots)+coastal,data=newdata)
  
  ## add appropriate names to line up with those in the matrix of posterior samples ##
  vnm_proc<-c('intercept','vmax_sust_s1','vmax_sust_s2','vmax_sust_s3',"sust_dur","exposure","poverty_prop","white_prop","owner_occupied_prop","age_pct_65_plus_prop",
              "median_age","population_density","median_house_value","no_grad_prop","year_s1","year_s2","coastal")
  
  colnames(X)<-vnm_proc
  
  ## get posterior samples of the predictions for each TC/county in newdata ##
  post_pred<-X%*%t(modobj$beta_post)
  
  return(post_pred)
}

####################333
  # And I'm going to want to set it up as a loop, instead of in parallel

## Setup

us_counties <- tigris::counties()
counties_mort_65 <- read_tsv("01_data/all_cause_mortality_65_plus_county.txt")



# ACS data:

library(tidycensus)


acs_vars <- read.xlsx("01_data/ACS2019_Table_Shells.xlsx")
acs_tables <- read.xlsx("01_data/2019_DataProductList.xlsx")


age_pct_65_plus_ids <- acs_vars %>%
  filter(Table.ID == "B01001" &
           Stub %in% c("65 and 66 years",
                       "67 to 69 years",
                       "70 to 74 years",
                       "75 to 79 years",
                       "80 to 84 years",
                       "85 years and over") )



county_acs_vars <- tidycensus::get_acs(geography = 'county',
                                       variables = c("B06012_001", 
                                                     "B06012_002",
                                                     "B02001_002",
                                                     "B02001_001",
                                                     "B07013_002",
                                                     "B07013_001",
                                                     age_pct_65_plus_ids$UniqueID,
                                                     "B01001_001",
                                                     "B01002_001",
                                                     "B25077_001",
                                                     "B06009_002",
                                                     "B06009_001"
                                       ),
                                       #state = 'Colorado',
                                       #county = "Denver",
                                       survey = "acs5",
                                       geometry = FALSE)

coastlines <- read.xlsx("01_data/coastline-counties-list.xlsx", colNames = TRUE, startRow = 3) %>% clean_names()
  # Oh, need to upload the coastlines file as well; maybe some others (e.g. `us_counties`)




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
  left_join(us_counties, by = "GEOID") %>%
  mutate(population_density = B01001_001 / ALAND,
         coastal = as.numeric(!is.na(coastline_region)),
         year = 2015,
         #year = sample(2006:2015, size = 1, replace = T),
         exposure = median(modobj$data$exposure)) %>%
  select(GEOID, poverty_prop:no_grad_prop, coastal:exposure, population_density, median_house_value)


# Oh, why didn't I think of this--just try exporting .rd or even csv

# Exporting

save(county_acs_vars_bayesian, file = "01_data/county_acs_vars_bayesian.rda")
save(amo_merged_250_clean_get_grid, file = "01_data/amo_merged_250_clean_get_grid.rda")

#################################################################################3
## Might be worth seeing if it's able to handle it straight out?


test_bayesian_data_conus <- (amo_merged_250_clean_get_grid %>% 
                               left_join(county_acs_vars_bayesian, 
                                         by = c("gridid" = "GEOID")))



test_bayesian_conus <- predict(modobj, newdata = (test_bayesian_data_conus %>% 
                                                    filter(!is.na(median_house_value))))


save(test_bayesian_conus, "test_bayesian_conus.rda")


test_bayesian_data_conus_split <- split(test_bayesian_data_conus,
                                        f = paste(test_bayesian_data_conus$storm_id,
                                                  test_bayesian_data_conus$scenario,
                                                  sep = "_"))


#### Attempting loop (I doubt parallel run would work based on previous experience)
  # I'm going to try using `lapply` instead; might make it easier to keep syntax and useable output


#test_bayesian_conus_list <- tibble()

#for(storm_id_TMP in test_bayesian_data_conus_split){
#  test_bayesian_data_conus_TMP <- storm_id_TMP %>% filter(!is.na(median_house_value))
#  test_bayesian_conus_TMP <- predict(modobj, newdata = test_bayesian_data_conus_TMP) %>%
#    as.tibble()
#  test_bayesian_conus_list <- append(test_bayesian_conus_list, test_bayesian_conus_TMP)
#}

modobj_predict <- function(x){
  A <- predict(modobj, newdata = x)
  return(A)
}

test_bayesian_conus_processed <- lapply(test_bayesian_data_conus_split, FUN = modobj_predict)

save(test_bayesian_conus_processed, "test_bayesian_conus_processed.rda")



## Loading output


load("01_data/test_bayesian_conus_processed_lapply.rda")


test_bayesian_conus_processed_lapply_comb <- do.call("rbind", test_bayesian_conus_processed_lapply)


load("01_data/test_bayesian_conus_rowmeans.rda")

test_bayesian_conus_rowmeans

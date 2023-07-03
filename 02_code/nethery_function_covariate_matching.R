# Tracking down county traits:

load("01_data/modobj.RData")
library(readxl)
library(tidyverse)


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

# vmax_sust <- done
# sust_dur <- done

# [ ] exposure <- ?? Use count from `stormdata?`

    # based on appendix, sounds like they might be defining it as "
      # a) total number of TC exposures experienced by the county during 1999-2015 (exposure)
      # b) e	primary	exposure	assessment	(local	peak	sustained	wind	in	the	county	of	21	m/s	or	higher)
    # ** Their model is built around specific storms, AND total list is shorter than list of counties, 
      # so might be that 

exposure_draft1 <- hurricaneexposuredata::storm_winds %>% 
  mutate(storm_year = str_extract_all(storm_id, "[0-9]{4}")) %>%
  filter(storm_year %in% c(1999:2015) & vmax_sust > 20) %>%
  select(fips, storm_id) %>%
  unique() %>%
  group_by(fips) %>%
  dplyr::summarize(exposure = n())



# Still not matching exposure (~17 seems to be closest, but mean/median is too low at ~2 vs ~5)

# Oh, could jsut use their data exactly--should match 1:1
  # where did I get that idea?? Different lengths (~3220 in this version, 2452 in `modobj`; no FIPS codes provided
      # also, I'm pretty sure `modobj` is built around *storms*, so repeats are possible)
        # does look different each time; they do use different years, so might expect variation

modobj$data %>%
  select(fips, exposure) # argh--doesn't include fips codes in model data

  # Huh, so I apparently just stuck with this as a placeholder?? 

## Process for accessing census data (still requires manaul lookup, I think)

library(openxlsx)
# ACS Variable tables (to easily search)

acs_vars <- read.xlsx("01_data/ACS2019_Table_Shells.xlsx")
acs_tables <- read.xlsx("01_data/2019_DataProductList.xlsx")

## 


# 65+ <- not seeing a single variable here; can try combining

age_pct_65_plus_ids <- acs_vars %>%
  filter(Table.ID == "B01001" &
           Stub %in% c("65 and 66 years",
                       "67 to 69 years",
                       "70 to 74 years",
                       "75 to 79 years",
                       "80 to 84 years",
                       "85 years and over") )


# ACS data:

library(tidycensus)

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
  mutate(population_density = B01001_001 / (ALAND * 0.00000038610),  # looks like m^2 --> miles^2
    coastal = as.numeric(!is.na(coastline_region)),
    year = 2015,
    #year = sample(2006:2015, size = 1, replace = T),
    #exposure = median(modobj$data$exposure)) %>%
  ) %>%
    left_join(exposure_draft1, by = c("GEOID" = "fips")) %>%
  select(GEOID, poverty_prop:no_grad_prop, coastal:exposure, population_density, median_house_value)

#county_acs_vars_bayesian$year = sample(2006:2015,replace=T, nrow(county_acs_vars_bayesian))

# OK, I'm pretty sure the issue with population density was that `us_counties` is in square meters, and 
  # the model assumes/uses square miles; conversion factor is 0.00000038610

# OK, now trying to integrate w/ model; need to think through easiest way to append data

library(splines)


test_bayesian_data <- (hurricaneexposuredata::storm_winds %>% 
                         left_join(county_acs_vars_bayesian, 
                                   by = c("fips" = "GEOID")))

test_bayesian <- predict(modobj, newdata = (test_bayesian_data %>% filter(!is.na(median_house_value))))

####

test_bayesian_data_conus <- (amo_merged_250_clean_get_grid %>% 
                         left_join(county_acs_vars_bayesian, 
                                   by = c("gridid" = "GEOID")))


test_bayesian_conus <- predict(modobj, newdata = (test_bayesian_data_conus %>% 
                                                    filter(!is.na(median_house_value))))

#### OK, filtering for rep dropped RAM load, but still too large; let's try a loop and/or parallelization?
  # If so, maybe try filtering by county? (Trying to anticipate ~nonlinear responses)

test_bayesian_conus_list <- list()

for(storm_id_TMP in test_bayesian_data_conus_split){
  test_bayesian_data_conus_TMP <- storm_id_TMP %>% filter(!is.na(median_house_value))
  test_bayesian_conus_TMP <- predict(modobj, newdata = test_bayesian_data_conus_TMP) %>%
    data.frame()
  test_bayesian_conus_list <- append(test_bayesian_conus_list, test_bayesian_conus_TMP)
}



#### Nope--still not enough memory (same figure of 28.7 Gb as well); let's try parallel processing?

# Processing for setup:

test_bayesian_data_conus_split <- split(test_bayesian_data_conus,
                                 f = paste(test_bayesian_data_conus$storm_id,
                                            test_bayesian_data_conus$scenario,
                                           sep = "_"))

# parallel processing

  # It isn't handling "external" objects well

# Setting up function?

modobj_predict <- function(x){
  A <- predict(modobj, newdata = x)
  return(A)
}

# Another method?

result_list <- llply(test_bayesian_data_conus_split, function(x) {
  # do the replacing
  return(x)
})


library(parallel)
#all
start <- proc.time()
#cl <- makeCluster(7, "modobj") # 8 for ~full utilization
cl <- makeCluster(5) # 8 for ~full utilization
clusterEvalQ(cl, source('02_code/pred_function.R'))
#clusterEvalQ(cl, library(Hmisc))
test_bayesian_data_conus_split_par <- parLapply(cl,
                                        test_bayesian_data_conus_split,
                                        modobj_predict
                                        )
stopCluster(cl)
end <- proc.time()
print(end - start) 



  # trying to figure out how it lines up with FIPS; seems to drop rows w/ missing data
test_bayesian_10 <- predict(modobj, newdata = test_bayesian_data[1:10,])
    # So I think every row is a county (w/ 1000 predictions?); I have to assume in order provided
      # However, full dataset is shorter than provided list; presumably dropping relevant NAs
        # could create ~cascading mismatches since it jsut outputs a numeric dataframe
          # so need to prefilter carefully (trying that above--let's see if it matches)

    # OK, removing the 342 counties without median house value data accounts for difference in length


test_bayesian_fixed_year <- predict(modobj, newdata = (test_bayesian_data %>% mutate(year = 2015)))

county_mortality_pred_bayesian <- function(fips = counties_mort_65$`County Code`,
                                  storm_data = (hurricaneexposuredata::storm_winds %>% 
                                                  left_join(county_acs_vars_bayesian, 
                                                            by = c("fips" = "GEOID"))),
                                  pred_model = modobj,
                                  counties = counties_mort_65,
                                  windspeed_min = 0,
                                  windspeed_max = Inf,
                                  storm_ids = ".*") {
  estimates <- counties %>%
    clean_names() %>%
    filter(county_code %in% fips) %>%
    inner_join(
      storm_data %>%
        filter(vmax_sust > windspeed_min &
                 vmax_sust <= windspeed_max) %>%
        filter(str_detect(storm_id, storm_ids)) %>% 
        mutate(storm_year = as.numeric(
          str_extract(storm_id, "[0-9]{4}"))) %>%
        mutate(storm_years = length(unique(storm_year))),
      mutate(
        storm_years = max(storm_year - min(storm_year)) + 1
      ),
      by = c("county_code" = "fips")
    )
  
  if(nrow(estimates > 0)){
    estimates <- estimates %>%
      mutate(est_excess_death_rate = 
               #predict(pred_model, newdata = data.frame( windspeed = vmax_sust)),
               predict(pred_model, newdata = storm_data),
             est_excess_deaths = (est_excess_death_rate *  population) / 100000)
    return(estimates %>%
             select(county_code, state, county, vmax_sust, storm_id, storm_year, est_excess_death_rate, est_excess_deaths, population, deaths, crude_rate, storm_years))
  }
  
  else{
    return("No data meeting parameters")
  }
  
}

county_mortality_pred_bayesian(pred_model = predict.tcExcessMort)



predict.tcExcessMort(newdata = (hurricaneexposuredata::storm_winds %>% 
                                  left_join(county_acs_vars_bayesian, 
                                            by = c("fips" = "GEOID"))))
# poverty

# acs_vars %>% filter(str_detect(Stub, "[Pp]overty")) # Uncomment to see output
  
  # I think the best match is B06012_002 / B06012_001  (looks to jsut be % of population below 100% of poverty)

# pct white

# B02001_002 / B02001_001

# owner occupied

# B07013_002 / B07013_001

# 65+ <- not seeing a single variable here; can try combining

age_pct_65_plus_ids <- acs_vars %>%
  filter(Table.ID == "B01001" &
           Stub %in% c("65 and 66 years",
                       "67 to 69 years",
                       "70 to 74 years",
                       "75 to 79 years",
                       "80 to 84 years",
                       "85 years and over") )


# Then take sum and divide by B01001_001

# Median age: (just one variable!)
# B01002_001

# population density <-- doesn't appear to be recorded per se

# B01001_001 / [area] <-- using shape file

# median house value

# B25077_001 
  # (for all "owner-occupied units"; any way to catch rentals as well?)

# Less than HS diploma (assuming for >=25 population)

# B06009_002 / B06009_001

# year <- use either median or latest in dataset
  # can I see what the ~training range was? <-- max was 2015, mean/median 2006
  # 2015 seems like it might be a better approximation?



# coastal <-- can use that file; already verified matching




#den_acs_tracts_geo <- tidycensus::get_acs(geography = 'tract',
#                                          table = 'B01001',
#                                          state = 'Colorado',
#                                          county = "Denver",
#                                          survey = "acs5",
#                                          geometry = TRUE,
#                                         progress_bar = FALSE)

## Generating age ranges by health outcome:


acs_stroke <- acs_vars %>%
  filter(Table.ID == "B01001" &
           Stub %in% c("35 to 39 years", 
                       "40 to 44 years",
                       "45 to 49 years",
                       "50 to 54 years",
                       "55 to 59 years",
                       "60 to 64 years",
                       "65 to 69 years",
                       "70 to 74 years",
                       "75 to 79 years",
                       "80 to 84 years") )

acs_dementia <- acs_vars %>%
  filter(Table.ID == "B01001" &
           Stub %in% c("55 to 59 years",
                       "60 to 64 years",
                       "65 to 69 years",
                       "70 to 74 years",
                       "75 to 79 years",
                       "80 to 84 years") )


acs_a_c_mort <- acs_vars %>%
  filter(Table.ID == "B01001" &
           Stub %in% c("18 and 19 years",
                       "20 years",
                       "21 years",
                       "22 to 24 years",
                       "25 to 29 years",
                       "30 to 34 years",
                       "35 to 39 years", 
                       "40 to 44 years",
                       "45 to 49 years",
                       "50 to 54 years",
                       "55 to 59 years",
                       "60 to 64 years",
                       "65 to 69 years",
                       "70 to 74 years",
                       "75 to 79 years",
                       "80 to 84 years",
                       "85 years and over") )


library(hurricaneexposure)
library(parallel)
library(tidyr)
library(stringr)
library(readr)

storm_25yr_sim_raw <- read_csv("STORM_25_years.csv")
  # loading Alex's 25-year sample data


#### Generally cleaning up/transforming input to be compatible with `get_grid_winds`
storm_25yr_sim_3hr <- storm_25yr_sim_raw %>%
  clean_names() %>%
  mutate(year = year + 1, # it doesn't seem to like a "0" year
         hours_base = timestep_3_hourly * 3, # timesteps to hours
         hour = hours_base %% 24, # "leftover" hours per day
         day = ((hours_base - hour) / 24) + 1, # hours to day increment
         storm_id = paste(storm_number,
                          str_pad(year, width = 4,
                                  side = "left",
                                  pad = 0),
                          sep = "-") # consistent storm ID system (# + year)
         #wind = wind * 1.94384449 # converting to knots (if provided in m/s)
  ) %>% 
  mutate(date = 
           paste0(str_pad(year, width = 4, side = "left", pad = 0),
                  str_pad(month, width = 2, side = "left", pad = 0),
                  str_pad(day, width = 2, side = "left", pad = 0),
                  str_pad(hour, width = 2, side = "left", pad = 0),
                  "00"
                  
           )) %>% # converting into yyyy:mm:dd:hh:mm format for input
  rename(
    "latitude" = "lat",
    "longitude" = "lon",
    "wind" = "max_wind_kt"
  ) %>% # getting right col names for input format
  select(storm_id, date, latitude, longitude, wind) # cutting down to just relevant data

# Spltting up dataframe into a list of storm-specific dataframes
  # to match single-storm input format; also allows parallelization

storm_25yr_sim_list_v2_3hr <- split(storm_25yr_sim_3hr,
                                    f = storm_25yr_sim_3hr$storm_id)


# Using `parallel` function family 

# If generalizating for vignette, maybe add a step to make sure it's not
#calling for more resources than the local CPU has available

# e.g. 
n_cores <- detectCores() # checking number of CPU cores available


start <- proc.time() # this and `end` are just to comapre run times at different # cores 

#cl <- makeCluster(8) # setting up ~parallel cluster (shouldn't exceed local resources) 
    # Seemed to hit diminishing returns by ~6 cores

cl <- makeCluster(n_cores - 1)  # Tells `parLapply` to run with one fewer core than CPU has

# Subsetting to avoid running the full data frame:

storm_winds_25yr_grid_3hr <- parLapply(cl,
                                       #storm_25yr_sim_list_v2_3hr,
                                       storm_25yr_sim_list_v2_3hr[1:3],
                                       get_grid_winds)
stopCluster(cl) # ~dissolves the set of parallel R clusters

end <- proc.time()
print(end - start)  # Just to see how long it took to run


##### This format is still split up by storm, but we can re-unite the dataframe using `rbind`:

storm_winds_25yr3h_grid_comb <- do.call("rbind",
                                        storm_winds_25yr_grid_3hr)

storm_winds_25yr3h_grid_comb <- storm_winds_25yr3h_grid_comb %>%
  rownames_to_column("storm_id") %>%
  mutate(storm_id = str_extract(storm_id,
                                "[0-9]-[0-9]*")) # to correct for the ~suffix added to avoid row repeats
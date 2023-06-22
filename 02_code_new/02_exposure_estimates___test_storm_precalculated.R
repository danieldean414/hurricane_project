# Run after `01_data_imports_and_setup` 
  # temporarily using a older, pre-processed set of storm data
    # CLIMADA date/time issues

storm_10k_obs_50k <- storm_10k_obs[1:50000,] # yeah, trying 50k rows out of the ~3.4 million

storm_10k_obs_50k_split <- split(storm_10k_obs_50k,
                                    f = storm_10k_obs_50k$storm_id)

########## Parallel Computing Step: relatively intensive, so run with caution
  # ideally run once and save output as .rda
####################################################################
####################################################################


# Setting up parameters for parallel run:
# Realized I need to comment out to make it practical to run/render

start <- proc.time()
cl <- makeCluster(6) # 8 for ~full utilization
storm_winds_storm_10k_obs_50k <- parLapply(cl,
                                   (storm_10k_obs_50k_split),
                                  get_grid_winds)
stopCluster(cl)
end <- proc.time()
print(end - start) 

  #well, that didn't take long! 
    # user  system elapsed 
    # 2.40    0.85  460.54 

  #think I'll try with the full dataset

#save(storm_winds_25yr_grid_3hr, file = "03_output/storm_winds_25yr_grid_3hr.rda")

####################################################################
####################################################################

# Loading saved output (Exported as .rda separately) 

load(file = "03_output/storm_winds_25yr_grid_3hr.rda")


## Rearranging from split format back to standard dataframe


storm_winds_25yr3h_grid_comb <- do.call("rbind", storm_winds_25yr_grid_3hr)

storm_winds_25yr3h_grid_comb <- storm_winds_25yr3h_grid_comb %>%
  rownames_to_column("storm_id") %>%
  mutate(storm_id = str_extract(storm_id,
                                "[0-9]-[0-9]*"))

# Any reason not to merge with county data at this stage?
  # needed components from wind:
##  vmax_sust: modeled maximum sustained wind speed at the population centroid of the county during the TC in m/s (from hurricaneexposuredata package)
##  sust_dur: duration of sustained wind speeds above 20 m/s at the population centroid of the county during the TC (from hurricaneexposuredata package)


tc_merged_data <- storm_winds_25yr3h_grid_comb %>% 
  left_join(county_acs_vars_bayesian, by = c('gridid' = 'GEOID'))

# 199 NAs in most cases (presumably limited by census data) <-- ~600 NAs for median housing value
  # HOWEVER, **~344k**  NA's for `exposure` 
    # might be that a lot of non-exposed counties are included, plus replication
    # but should check that approach makes sense
    # Also ~72k NAs for population density

tc_merged_data$gridid %>% table()
# 199 versions of each county (by FIPS)
  # multiplied by # of storm IDs in this data set
  # trying to think through how to efficiently vet whether the results make sense
    # e.g. at some point should apply a lower threshold





########################################################3
# Example plots: 
  # maybe best to have in a separate plotting/visualizaiton file


# Mean winds
tc_merged_data %>%
  group_by(gridid) %>%
  dplyr::summarize(mean_windspeed = mean(vmax_sust)) %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = mean_windspeed, geometry = geometry) +
  geom_sf() +
  theme_minimal() + 
  scale_fill_viridis_c(option = 'magma') +
  labs(fill = "Windspeed (m/s)",
       title = "Mean Sustained Windspeed (m/s) in 25y Simulation")

# max winds [useful?]

tc_merged_data %>%
  group_by(gridid) %>%
  dplyr::summarize(mean_windspeed = max(vmax_sust)) %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = mean_windspeed, geometry = geometry) +
  geom_sf() +
  theme_minimal() +
  scale_fill_viridis_c(option = 'magma') + 
  labs(fill = "Windspeed (m/s)",
       title = "Maximum Sustained Windspeed (m/s) in 25y Simulation")

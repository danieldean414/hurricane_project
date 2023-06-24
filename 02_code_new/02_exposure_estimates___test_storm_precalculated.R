# Run after `01_data_imports_and_setup` 
  # temporarily using a older, pre-processed set of storm data
    # CLIMADA date/time issues

storm_10k_obs_50k <- storm_10k_obs[1:50000,] %>% # yeah, trying 50k rows out of the ~3.4 million
  mutate(longitude = longitude - 360)

storm_10k_obs_50k_split <- split(storm_10k_obs_50k,
                                    f = storm_10k_obs_50k$storm_id)

# seeing if the longitude format (seems to show degrees west as deg. east + west)
  # whereas the `floyd_tracks` dataset shows it as negative degrees east
  # so I think use 180 - longitude
  # seeing if that gives fewer NAs
    # although then I'd expect *no* valid results...

########## Parallel Computing Step: relatively intensive, so run with caution
  # ideally run once and save output as .rda
####################################################################
####################################################################


# Setting up parameters for parallel run:
# Realized I need to comment out to make it practical to run/render

start <- proc.time()
cl <- makeCluster(6) # 8 for ~full utilization
storm_winds_storm_10k_obs_leg_lon_50k <- parLapply(cl,
                                   (storm_10k_obs_50k_split),
                                  get_grid_winds)
stopCluster(cl)
end <- proc.time()
print(end - start) 


storm_winds_storm_10k_obs_leg_lon_50k_comb <- do.call("rbind", storm_winds_storm_10k_obs_leg_lon_50k)


  #well, that didn't take long! 
    # user  system elapsed 
    # 2.40    0.85  460.54 

  #think I'll try with the full dataset
    # also pretty quick, although sort of alarmingly messed up the UI
    # huh--some 'NA' results; trying to figure out what might be causing that
      # some possibilities:
        # looks like the *dates* are NAs, and other variables are 0's, so maybe a formatting issue?
        # maybe there's a minimum windspeed?
        # Noticing the NA's are for certain gridid's (FIPS), so maybe that's the output for non-exosed areas?
        # specific column is `date_time_max_wind`; might be significant
storm_10k_obs <- storm_10k_obs_na_proc %>%
  add_count(storm_id) %>% 
#  filter(n > 2) %>% # not sure if there's a functional minimum
  dplyr::select(-n)
  # from looking around, causes for the error I'm getting could be at least one of:
    # too few points to interpolate
    # wrong order for e.g. dates -- seems unlikely sans e.g. bracketing issue

storm_10k_obs_split <- split(storm_10k_obs,
                                 f = storm_10k_obs$storm_id)

# Yeah, so even with changing from ~positive to ~negative notation for degrees East,
  #still ~56% NA's (didn't check last time)
  # also sust_dur and gust_dur are showing up as uniformly 0's--don't remember that

# trying to figure out which CRS to use
  # either WGS84  or NAD83 , probably

us_counties_nad83 <- st_transform(us_counties, crs = "NAD83")
us_counties_wgs84  <- st_transform(us_counties, crs = "WGS84")



# trying to think if there's an efficient way to check for boundary overlaps
  # e.g. maybe a lot of NA storms just don't clip the US?

# are we supposed to preserve the storm_ids with this? <- looks like it doesn't use them
  # so would need to e.g. add that as a new variable if I want to separate them

########## Parallel Computing Step: relatively intensive, so run with caution
# ideally run once and save output as .rda
####################################################################
####################################################################


# Setting up parameters for parallel run:
# Realized I need to comment out to make it practical to run/render

start <- proc.time()
cl <- makeCluster(6) # 8 for ~full utilization
storm_winds_storm_10k_obs <- parLapply(cl,
                                           (storm_10k_obs_split),
                                           get_grid_winds)
stopCluster(cl)
end <- proc.time()
print(end - start) 

storm_winds_storm_10k_obs_comb <- do.call("rbind", storm_winds_storm_10k_obs)


# argh, keep hitting that same issue; tempted to try running a loop to see where it breaks?
  # 12.6k storms
  # I guess could at least see if it breaks quickly
    # yeah, wow--breaks on very 1st run <- not sure why the 50k subset worked, then
    # does work for misc. others
    # huh; what's going on...

  # OK, plotting again, I think the clipping might be confusing it?
    # e.g. 1st storm somehow clips out maybe 3 times
    # other thought would be that if it's somehow conflating storms?
    
  # So, I think make a ~'helper' column to track if they're in the zone at least once

for(storm in storm_10k_obs_split){
  storm_in <- storm
  grid_winds <- get_grid_winds(storm_10k_obs_split)
  
}



storm_counties <- storm_10k_obs_winds_comb$gridid %>% unique()


us_counties_wgs84 %>%
  filter(GEOID %in% storm_counties) %>%
  ggplot() +
  aes(geometry = geometry) +
  geom_sf() +
  theme_classic() 

ggplot() +
  geom_sf(aes(
    geometry = (us_counties_wgs84 %>%
                  filter(GEOID %in% storm_counties))$geometry
  )) +
  geom_sf(aes(
    geometry = ((storm_10k_obs_50k_split$`1-0021` %>%
                   st_as_sf(coords = c('longitude', 'latitude'), 
                            crs = "WGS84"))$geometry)
  ))

# OK, 

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

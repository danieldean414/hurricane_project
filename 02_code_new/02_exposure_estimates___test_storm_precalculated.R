# Run after `01_data_imports_and_setup` 
  # temporarily using a older, pre-processed set of storm data
    # CLIMADA date/time issues


# seeing if the longitude format (seems to show degrees west as deg. east + west)
  # whereas the `floyd_tracks` dataset shows it as negative degrees east
  # so I think use 180 - longitude
  # seeing if that gives fewer NAs
    # although then I'd expect *no* valid results...

########## Parallel Computing Step: relatively intensive, so run with caution
  # ideally run once and save output as .rda
####################################################################
####################################################################

# Decided to add 'historical' data as a point of reference
  # either of ~primary interest, or as more of a ~supplemental thing


load("01_data/storm_hist_proc_hurricane_64_knots_plus.rda")
# filtered w/ same conditions (minus landfall) as simualted data
# called "storm_hist_proc_hurricane"


storm_hist_proc_hurricane_split <- split(storm_hist_proc_hurricane,
                                               f = storm_hist_proc_hurricane$storm_id)

start <- Sys.time()#proc.time()
cl <- makeCluster(7) # 8 for ~full utilization
storm_hist_proc_hurricane_split_ggw <- parLapply(cl,
                                                       (storm_hist_proc_hurricane_split),
                                                       get_grid_winds)
stopCluster(cl)
end <- Sys.time() #proc.time()
print(end - start) 

time_na <- end-start

Sys.time()
storm_hist_proc_hurricane_ggw <- do.call("rbind", storm_hist_proc_hurricane_split_ggw)
Sys.time()

save(storm_hist_proc_hurricane_ggw, file = "01_data/storm_hist_proc_hurricane_ggw.rda")




# trying the filtered NA/US within 1 degree; thinking I could compare specific storms to get a sense of accuracy
# full dataset seems to crash in ~3.5 hours (issue with `seq()` encoutering a non-finite value?)




#########3

##################################################

# Trying full 10k years (should rename current 1k test)


storm_10k_all_obs_na_proc_hurricane_split <- split(storm_10k_all_obs_na_proc_hurricane,
                                               f = storm_10k_all_obs_na_proc_hurricane$storm_id)

start <- Sys.time()#proc.time()
cl <- makeCluster(7) # 8 for ~full utilization
storm_10k_all_obs_na_proc_hurricane_split_ggw <- parLapply(cl,
                                                       (storm_10k_all_obs_na_proc_hurricane_split),
                                                       get_grid_winds)
stopCluster(cl)
end <- Sys.time() #proc.time()
print(end - start) 

time_na <- end-start

Sys.time()
storm_10k_all_obs_na_proc_hurricane_ggw <- do.call("rbind", storm_10k_all_obs_na_proc_hurricane_split_ggw)
Sys.time()

save(storm_10k_all_obs_na_proc_hurricane_ggw, file = "01_data/storm_10k_all_obs_na_proc_hurricane_ggw.rda")

# trying the filtered NA/US within 1 degree; thinking I could compare specific storms to get a sense of accuracy
  # full dataset seems to crash in ~3.5 hours (issue with `seq()` encoutering a non-finite value?)

storm_10k_obs_na_proc_hurricane_split <- split(storm_10k_obs_na_proc_hurricane,
                                     f = storm_10k_obs_na_proc_hurricane$storm_id)

start <- Sys.time()#proc.time()
cl <- makeCluster(7) # 8 for ~full utilization
storm_10k_obs_na_proc_hurricane_split_ggw <- parLapply(cl,
                                           (storm_10k_obs_na_proc_hurricane_split),
                                           get_grid_winds)
stopCluster(cl)
end <- Sys.time() #proc.time()
print(end - start) 

time_na <- end-start

Sys.time()
storm_10k_obs_na_proc_hurricane_ggw <- do.call("rbind", storm_10k_obs_na_proc_hurricane_split_ggw)
Sys.time()

###############################



  # ran for ~2 hours before crashing (cannot allocate vector of size 19 Kb)
    # maybe missed it, but session was only at ~2.8 Gb

storm_10k_obs_na_all_proc <- storm_10k_obs_na_all_proc %>%
  add_tally() %>%
  filter(n > 3) %>% # doubt it's this simple, but worth a shot
  dplyr::select(-n)

storm_10k_obs_na_all_proc_split <- split(storm_10k_obs_na_all_proc,
                                     f = storm_10k_obs_na_all_proc$storm_id)

start <- Sys.time()#proc.time()
cl <- makeCluster(7) # 8 for ~full utilization
storm_10k_obs_na_all_proc_split_ggw <- parLapply(cl,
                                             (storm_10k_obs_na_all_proc_split),
                                             get_grid_winds)
stopCluster(cl)
end <- Sys.time() #proc.time()
print(end - start) 

time_na <- end-start

Sys.time()
storm_10k_obs_na_all_proc_ggw <- do.call("rbind", storm_10k_obs_na_all_proc_split_ggw)
Sys.time()


####################3

# Setting up parameters for parallel run:
# Realized I need to comment out to make it practical to run/render

ncores <- detectCores()
start <- proc.time()
cl <- makeCluster(ncores-1) # 8 for ~full utilization
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
#storm_10k_obs <- storm_10k_obs_na_proc %>%
#  add_count(storm_id) %>% 
#  filter(n > 2) %>% # not sure if there's a functional minimum
#  dplyr::select(-n)
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

# taking a weirdly long time; trying with 50k rows again...

storm_10k_obs_split_try3 <- split(storm_10k_obs[100000:nrow(storm_10k_obs),],
                             f = (storm_10k_obs[100000:nrow(storm_10k_obs),])$storm_id)

storm_10k_obs_na_split <- split(storm_10k_obs_na_proc,
                                f = (storm_10k_obs_na_proc)$storm_id)
  
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

start <- Sys.time()#proc.time()
cl <- makeCluster(7) # 8 for ~full utilization
storm_10k_obs_na_split_winds3 <- parLapply(cl,
                                           (storm_10k_obs_split_try3),
                                           get_grid_winds)
stopCluster(cl)
end <- Sys.time() #proc.time()
print(end - start) 

time_50k <- end-start

storm_10k_obs_na_split_winds3 <- do.call("rbind", storm_10k_obs_na_split_winds3)


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



storm_counties <- storm_10k_obs_na_proc_ggw$gridid %>% unique()
# number of counties:
storm_10k_obs_na_proc_ggw$gridid %>% unique() %>% length()

# checking county traits with variables
  # doesn't have populations yet...
county_acs_vars_bayesian %>% filter(GEOID %in% storm_counties)

#############3
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

# OK, (jumping ahead), trying to plot storms giving 'NA' results against counties
  # I wonder if there'd be a way to give storm + county combinations

ggplot() +
  geom_sf(aes(
    geometry = (us_counties_wgs84 %>%
                  filter(GEOID %in% storm_counties))$geometry
  )) +
  geom_sf(aes(
    geometry = geometry,
    color = wind
  ), data = (((storm_10k_obs_na_proc %>% filter(storm_id %in% storm_ids_na$storm_id)) %>%
                mutate(longitude = longitude -360) %>%
                st_as_sf(coords = c('longitude', 'latitude'), 
                         crs = "WGS84"))))

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


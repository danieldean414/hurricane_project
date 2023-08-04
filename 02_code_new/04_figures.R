

load("01_data/test_bayesian_rowwise_95_cis.rda")
load("01_data/test_bayesian_rowwise_hist95_cis.rda")

# filtering to 1000 years to hopefully slow working times
  # wow, already >8 Gb just loading things, though!
test_bayesian_rowwise_95_cis_working <- test_bayesian_rowwise_95_cis %>%
  filter(str_detect(storm_id, "^1"))

# Have various exploratory plots up, but aiming to set up formal 'figures'



# ~supplementary plot showing explict geographic buffer (vs 'bounding box' approach

ggplot() +
  geom_sf(
    aes(geometry = geometry),
    data = (hurricaneexposuredata::storm_winds %>% 
              dplyr::select(fips) %>%
              unique() %>%
              left_join(us_counties, by = c("fips" = "GEOID")) %>%
              st_as_sf() %>%
              st_union() %>% st_buffer(250000))) +
  geom_sf(
    aes(geometry = geometry),
    data = (hurricaneexposuredata::storm_winds %>% 
              dplyr::select(fips) %>%
              unique() %>%
              left_join(us_counties, by = c("fips" = "GEOID")) %>%
              st_as_sf() %>%
              st_union()))
  
# not currently using; was running into issues w/ explicit geography operations


# Figure 1: Plot of study populations, maybe other relevant traits

##########################################################################################
  # still trying to think through what's relevant as far as 
    # a) reader interest (e.g. poverty %) and 
    # b) functional implications (because I want to say most of the variables were non-significant)

# I don't think additional variables shuld cause problems, so could e.g. facet by populations and rates

  # Are the county_vars prefiltered? -- 3134 rows, so no

  # I guess not exactly enlightening -- more uniform than I'd expected, actually 

county_acs_vars_bayesian %>%
  left_join(us_counties_wgs84) %>%
  filter(GEOID %in% unique(test_bayesian_rowwise_95_cis_working$gridid)) %>%
#  left_join(counties_mort_65, by = c("GEOID" = "county_code")) %>%
  ggplot() +
  aes(
    geometry = geometry,
    fill = age_65_plus
  ) +
  geom_sf() +
  theme_void() + 
  scale_fill_viridis_c() +
  labs(
    title = "Popualtion (65 and Older) in Atlantic TC-Exposed US Counties",
    fill = "Residents 65 and Older"
  )

exposured_county_ids <- test_bayesian_rowwise_summarized$gridid %>% unique()

# general summary
county_acs_vars_bayesian %>%
  left_join(us_counties_wgs84) %>%
  filter(GEOID %in% storm_counties) %>% 
  summary()

# exposed pop
county_acs_vars_bayesian %>% filter(GEOID %in% storm_counties & coastal == 1) %>%
  left_join(us_counties_wgs84) %>%
  filter(GEOID %in% storm_counties & GEOID %in% exposured_county_ids) %>% 
  summary()

# Could probably run some kind of paired test if it's relevant
  # e.g. exposed group looks slightly older, lower poverty, less white

#total pop
county_acs_vars_bayesian %>%
  left_join(us_counties_wgs84) %>%
  filter(GEOID %in% storm_counties) %>% 
  dplyr::select(age_65_plus) %>% 
  sum()

# exposed pop
county_acs_vars_bayesian %>% filter(GEOID %in% unique(test_bayesian_rowwise_95_cis_working$gridid)) %>%
  left_join(us_counties_wgs84) %>%
  filter(GEOID %in% (unique(test_bayesian_rowwise_95_cis_working$gridid)) ) %>% 
  dplyr::select(age_65_plus) %>% 
  sum()

# 27,125,160 <-- does that seem right? 
    # US overall has 330,000,0000 ish, so around 9% of total US population
      # east is a lot more densely settled than West, so not implausible

#coastal
# exposed pop

county_acs_vars_bayesian %>% 
  filter(GEOID %in% unique(test_bayesian_rowwise_95_cis_working$gridid) & 
           coastal == 1) %>%
  left_join(us_counties_wgs84) %>%
  filter(GEOID %in% (unique(test_bayesian_rowwise_95_cis_working$gridid)) ) %>% 
  dplyr::select(age_65_plus) %>% 
  sum()

# 10,274,866 exposed


# Oh, jsut realized I haven't been "saving" the code on exploratory plots;

#Mean annual category 1 deaths
test_bayesian_rowwise_hist95_cis %>%
  unnest(data) %>%
  filter(vmax_sust >= 32.9) %>% 
  unnest(impact_summary) %>% 
  group_by(gridid) %>%
  mutate(cat_1_deaths_yr = sum(mean)/30) %>%
  dplyr::select(gridid, cat_1_deaths_yr) %>%
  unique() %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID"))  %>% 
  ggplot() + aes(geometry = geometry, fill = cat_1_deaths_yr) +
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void()

  # could also not divide by years, but then not compatible with historical vs simualted data

  #I guess same issue of distributions, though...

# Return periods and related
  # I wonder if it's possible to use 1/x as the transformation? 

test_bayesian_rowwise_95_cis_working %>%
  unnest(data) %>% 
  filter(vmax_sust >= 32.9) %>%
  ungroup() %>% 
  group_by(gridid) %>% 
  add_tally() %>%  
  dplyr::select(gridid, n) %>% 
  unique() %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  mutate(return_period = 1/(n/1000)) %>%
  ggplot() + aes(geometry = geometry, fill = return_period) +
  geom_sf() +
  scale_fill_viridis_c(trans = 'log') +
  theme_void()

test_bayesian_rowwise_95_cis_working %>% 
  unnest(data) %>%
  filter(vmax_sust >= 32.9) %>%
  ungroup() %>% 
  group_by(gridid) %>% 
  add_tally() %>% 
  dplyr::select(gridid, n) %>% 
  unique() %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>%
  mutate(return_period = 1/(n/1000)) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = return_period) + 
  geom_sf() +
  scale_fill_viridis_c(trans=scales::trans_new("1_over_x",
                                               transform = function(x) {1/(x+0.0001)}, 
                                               inverse = function(x) {-1/(x+0.0001)})) + 
  theme_void()

# or might be predefined


test_bayesian_rowwise_95_cis_working %>% 
  unnest(data) %>%
  filter(vmax_sust >= 32.9) %>%
  ungroup() %>% 
  group_by(gridid) %>% 
  add_tally() %>% 
  dplyr::select(gridid, n) %>% 
  unique() %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>%
  mutate(return_period = 1/(n/1000)) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = return_period) + 
  geom_sf() +
  scale_fill_viridis_c(trans= 'reciprocal') + 
  theme_void()
    # trying to set limits to deal with infinite; no luck so far
        # is a transformation called ~trim_tails(?), but not sure how to apply both
        # could figure out what it's doing numerially and write a custom function to do both 
      # 

################################################333
  # incidentally, I wonder how close the census and CDC populations are?

county_acs_vars_bayesian %>%
  left_join(us_counties_wgs84) %>%
  filter(GEOID %in% storm_counties) %>%
  left_join(counties_mort_65, by = c("GEOID" = "county_code")) %>%
  ggplot() + aes(x = population, y = age_65_plus) +
  geom_point() + 
  geom_abline(slope = 1) +
  coord_fixed()
  # oh, that isn't good...
    # So if it was e.g. a matter of older data, would still expect close to 1:1
    # Seems like the CDC 'population' variable is *way* higher (up to 1.5 million, vs ~<8e5)
      #~53%

    # a few preliminary thoughts:
      # maybe the CDC data's not actually filtered to >=65 only? 1.5 million seems plausible
      # Some kind of unit confusion? If it were the Census figures that were ~2x, I might suggest some duplication issue

      # OK, e.g. Cook County, IL (the ~1.5 million one), has a total of >5 million
          # so actually my census data math is seeming more suspect -- check variables, etc.
      # yeah, comparing that and a few, it's almost exactly 1:2
        # The census data I'm reading in has male and female versions, but I'm explicitly adding those in

      # The 2x relationship seems hard to explain otherwise
        # Oh, maybe if I had multiple years selected it's adding up the "populations" for each?
        # Ok, I think that was it! 
        # E.g. just downloaded 2018+19 vs 2019 only, and the former does show roughly double the population
        
  # OK, updated it with (provisionally) a new version using only 2018 data, and is now close to 1:1


  # hmm-going back to the plot, it looks like there are more TX counties with missing data for at least 2018
    # 6 of them, another in some more-northern state

####################################################3
  # Figure 2: Storm tracks?
      # Unless/until I add climate data, let's do a hisotrical vs simualted comparison:
      # should put in appropriate files, I think
      # Ideally use IBTRACS; actually I guess should be similar

## looking into NAs again...
storm_10k_obs_na_proc_ggw %>% 
  rownames_to_column("storm_id_raw") %>%
  mutate(storm_id = str_extract(storm_id_raw, "\\d\\-\\d{4}\\-\\d")) %>%
  filter(storm_id %in% storm_10k_obs_na_proc$storm_id)


historical_tracks <- hurricaneexposuredata::hurr_tracks %>% 
  mutate(date = lubridate::ymd_hm(date))


ggplot() +
  geom_sf(aes(
    geometry = (us_counties_wgs84 %>%
                  filter(GEOID %in% storm_counties))$geometry
  )) +
  geom_sf(aes(color = wind, 
    geometry = (geometry)
  ), data = (historical_tracks %>%
               st_as_sf(coords = c('longitude', 'latitude'), 
                        crs = "WGS84") %>%
               group_by(storm_id) %>%
               st_cast("LINESTRING"))
  ) +
  theme_void()



## Figure 2: Some Visual for Storm Paths
  # trying to see if I can get it to plot between lines

ggplot() +
  geom_sf(aes(
    geometry = (us_counties_wgs84 %>%
                  filter(GEOID %in% storm_counties))$geometry
  )) +
  geom_sf(aes(color = wind, 
              geometry = (geometry)
  ), 
  data = (historical_tracks %>%
               
               group_by(storm_id) %>% 
               summarise(do_union=F) %>%
               mutate(
                 lineid = row_number(), # create a lineid
                 longitude_end = lead(longitude), # create the end point coords for each start point
                 latitude_end = lead(latitude)
               ) %>% 
               st_as_sf(coords = c('longitude', 'latitude'), 
                                             crs = "WGS84") %>%  
               unite(start, longitude, latitude) %>%
               unite(end, longitude_end, latitude_end) %>%
               filter(end != 'NA_NA') %>%
               gather(start_end, coords, start, end) %>% 
               separate(coords, c("longitude", "latitude")) %>% 
               mutate_at(vars("longitude", "latitude"), as.numeric) %>% 
               st_as_sf(coords = c('longitude', 'latitude'), crs = "WGS84") %>%
               group_by(storm_id) %>%
               dplyr::summarize() %>%
               st_cast(., to = "LINESTRING"))
             
              # alternative would be to see if I can somehow plot map w/o geom_sf?


## Figure 3: Sustained Exposures

tc_merged_data <- storm_10k_obs_na_proc_ggw %>% #storm_winds_25yr3h_grid_comb %>% 
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
test_bayesian_data_hist %>%
  group_by(gridid) %>%
  dplyr::summarize(mean_windspeed = mean(vmax_sust)) %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = mean_windspeed, geometry = geometry) +
  geom_sf() +
  theme_minimal() + 
  scale_fill_viridis_c(option = 'magma') +
  labs(fill = "Windspeed (m/s)",
       title = "Mean Sustained Windspeed (m/s) in 1000-Year Simulation") +
  theme_void()



storm_10k_obs_na_proc_ggw %>%
  group_by(gridid) %>%
  dplyr::summarize(mean_windspeed = mean(vmax_sust)) %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = mean_windspeed, geometry = geometry) +
  geom_sf() +
  theme_minimal() + 
  scale_fill_viridis_c(option = 'magma') +
  labs(fill = "Windspeed (m/s)",
       title = "Mean Sustained Windspeed (m/s) in 1000-Year Simulation") +
  theme_void()

test_bayesian_rowwise_summarized_hist %>%
  dplyr::select(data) %>%
  unnest(data)
  group_by(gridid) %>%
  dplyr::summarize(mean_windspeed = mean(vmax_sust)) %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = mean_windspeed, geometry = geometry) +
  geom_sf() +
  theme_minimal() + 
  scale_fill_viridis_c(option = 'magma') +
  labs(fill = "Windspeed (m/s)",
       title = "Maximum Sustained Windspeed (m/s) Observed 1988-2018") +
  theme_void()

# max winds [useful?]

storm_10k_obs_na_proc_ggw %>%
  group_by(gridid) %>%
  dplyr::summarize(mean_windspeed = max(vmax_sust)) %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = mean_windspeed, geometry = geometry) +
  geom_sf() +
  theme_minimal() +
  scale_fill_viridis_c(option = 'magma') + 
  labs(fill = "Windspeed (m/s)",
       title = "Maximum Sustained Windspeed (m/s) in 1000-Year Simulation") +
  theme_void()

# Difference map

test_bayesian_rowwise_summarized_hist %>%
  group_by(gridid) %>%
  dplyr::select(data) %>%
  unnest(data) %>%
  dplyr::summarize(mean_windspeed_hist = mean(vmax_sust)) %>%
  full_join(
    (test_bayesian_rowwise_summarized %>%
       group_by(gridid) %>%
       dplyr::select(data) %>%
       unnest(data) %>%
       dplyr::summarize(mean_windspeed_sim = mean(vmax_sust))),
    by = "gridid"
  ) %>%
  mutate(mean_windspeed_diff = mean_windspeed_sim - mean_windspeed_hist) %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = mean_windspeed_diff, geometry = geometry) +
  geom_sf() +
  theme_minimal() +
  scale_fill_viridis_c(option = 'magma') + 
  labs(fill = "Windspeed (m/s)",
       title = "Difference in Mean Sustained Windspeed (m/s) between Historical and Simulated TCs") +
  theme_void()

#####################
  # Exposure frequency (~return period)


test_bayesian_rowwise_summarized_hist %>%
  group_by(gridid) %>%
  dplyr::select(data) %>%
  unnest(data) %>%
  dplyr::summarize(exposures = n(),
                   return_pd_hist = 1/(exposures/30)) %>%
  full_join(
    (test_bayesian_rowwise_summarized %>%
       group_by(gridid) %>%
       dplyr::select(data) %>%
       unnest(data) %>%
       dplyr::summarize(exposures = n(),
                        return_pd_sim = 1/(exposures/1000))), # ideally find way to automate
    by = "gridid"
  ) %>%
  mutate(return_pd_diff = return_pd_sim - return_pd_hist) %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = return_pd_diff, geometry = geometry) +
  geom_sf() +
  theme_minimal() +
  scale_fill_viridis_c(option = 'cividis', trans = 'log', na.value = 'white') + 
  labs(fill = "Average Years Between Exposures",
       title = "Average Years Between >= 17.4 m/s Exposures 1988-2018") +
  theme_void()

#################################
## Excess outputs from model

test_bayesian_rowwise_summarized_hist %>%
  dplyr::select(-impact) %>%
  unnest(c(data:impact_upper)) %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = impact_median, geometry = geometry) +
  geom_sf() +
  theme_minimal() +
  scale_fill_viridis_c(option = 'magma') + 
  labs(fill = "RR?",
       title = "Median RR by County in Historic TCs") +
  theme_void()
  
  #why the NAs?
  
test_bayesian_rowwise_summarized %>%
  dplyr::select(-impact) %>%
  unnest(c(data:impact_upper)) %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = impact_median, geometry = geometry) +
  geom_sf() +
  theme_minimal() +
  scale_fill_viridis_c(option = 'magma') + 
  labs(fill = "RR?",
       title = "Median RR by County in 100 Synthetic TC Seasons") +
  theme_void()

# Realized I need to summarize by county
test_bayesian_rowwise_summarized %>%
  dplyr::select(-impact) %>%
  unnest(c(data:impact_upper)) %>%
  group_by(gridid) %>%
  dplyr::summarize(impact_median = median(impact_median, na.rm = T))  %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = impact_median, geometry = geometry) +
  geom_sf() +
  theme_minimal() +
  scale_fill_viridis_c(option = 'magma') + 
  labs(fill = "RR?",
       title = "Median RR by County in 100 Synthetic TC Seasons") +
  theme_void()


test_bayesian_rowwise_summarized_hist %>%
  dplyr::select(-impact) %>%
  unnest(c(data:impact_upper)) %>%
  group_by(gridid) %>%
  dplyr::summarize(impact_median = median(impact_median, na.rm = T))  %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = impact_median, geometry = geometry) +
  geom_sf() +
  theme_minimal() +
  scale_fill_viridis_c(option = 'magma') + 
  labs(fill = "RR?",
       title = "Median RR by County in Historic TC Seasons") +
  theme_void()


# Attempt at a difference plot

test_bayesian_rowwise_summarized_hist %>%
  dplyr::select(-impact) %>%
  unnest(c(data:impact_upper)) %>%
  group_by(gridid) %>%
  dplyr::summarize(impact_median_hist = median(impact_median, na.rm = T))  %>%
  full_join(
    (test_bayesian_rowwise_summarized %>%
       dplyr::select(-impact) %>%
       unnest(c(data:impact_upper)) %>%
       group_by(gridid) %>%
       dplyr::summarize(impact_median_sim = median(impact_median, na.rm = T)) ),
    by = "gridid"
  ) %>%
  mutate(impact_median_diff = impact_median_sim - impact_median_hist) %>%
  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
  ggplot() +
  aes(fill = impact_median_diff, geometry = geometry) +
  geom_sf() +
  theme_minimal() +
  scale_fill_viridis_c(option = 'cividis') + 
  labs(fill = "RR?",
       title = "Difference in RR Between Simualted and Historic TC Seasons") +
  theme_void()

# the simulated data still looks kind of spotty as far as ~risk; e.g. that band through N Florida
  # huh; so the simualted version is 76519 rows, vs 5009 for historical;
    # 30 years for latter, but difference is only ~15x, which suggests more like 450 years
      # maybe there's still a subset happening?
  # I mean, based on the simulated storm IDs, *looks* like there are years 0002-0999 represented
    # 1688 storms represented (presumably skipping sub-threshold ones?)
      # vs 106 historical ones -- still that ~15x multiplier



####################3


## OK, I'm liking this presentation: cumulative attributable mortality per year <-- probably manually adjsut at this point

test_bayesian_rowwise_95_cis %>% 
  unnest(impact_summary) %>% 
  unnest(impact_summary) %>%
  mutate(intervals = rep(c("central", "upper", "lower") )) %>%
  pivot_wider(values_from = impact_summary, names_from = intervals) %>% 
  dplyr::select(-impact, - data) %>% 
  group_by(gridid) %>% 
  dplyr::summarise(., across(where(is.numeric), sum)) %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = central / 1000) +
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void()

# OK, so even w/ filtering, there are still 1000 years represented here

test_bayesian_rowwise_hist_95_cis %>% 
  unnest(impact_summary) %>% 
  unnest(impact_summary) %>%
  mutate(intervals = rep(c("central", "upper", "lower") )) %>%
  pivot_wider(values_from = impact_summary, names_from = intervals) %>% 
  dplyr::select(-impact, - data) %>% 
  group_by(gridid) %>% 
  dplyr::summarise(., across(where(is.numeric), sum)) %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = central / 30) + # 2018 - 1988 -- ideally find one starting in 1980 
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void()

# trying a difference plot


test_bayesian_rowwise_95_cis %>% 
  unnest(impact_summary) %>% 
  unnest(impact_summary) %>%
  mutate(intervals = rep(c("central_sim", "upper_sim", "lower_sim") )) %>%
  pivot_wider(values_from = impact_summary, names_from = intervals) %>% 
  dplyr::select(-impact, - data) %>% 
  group_by(gridid) %>% 
  dplyr::summarise(., across(where(is.numeric), ~sum(.x)/1000)) %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  full_join(
    test_bayesian_rowwise_hist_95_cis %>% 
      unnest(impact_summary) %>% 
      unnest(impact_summary) %>%
      mutate(intervals = rep(c("central_hist", "upper_hist", "lower_hist") )) %>%
      pivot_wider(values_from = impact_summary, names_from = intervals) %>% 
      dplyr::select(-impact, - data) %>% 
      group_by(gridid) %>% 
      dplyr::summarise(., across(where(is.numeric), ~sum(.x)/30)) %>% 
      left_join(us_counties_wgs84, by = c("gridid" = "GEOID"))
    
  ) %>% mutate(central_diff = central_sim - central_hist) %>% ggplot() + 
  aes(geometry = geometry, fill = central_diff) +
  geom_sf() + 
  scale_fill_viridis_c(option = "cividis") + 
  theme_void()
    # wow, a lot higher; I guess make sure I'm filtering these by the same criteria

# example of looking for specific storms by impact, etc.:
test_bayesian_rowwise_hist_95_cis %>% 
  unnest(impact_summary) %>% 
  unnest(impact_summary) %>%
  mutate(intervals = rep(c("central_hist", "upper_hist", "lower_hist") )) %>%
  pivot_wider(values_from = impact_summary, names_from = intervals) %>% 
  unnest(data) %>%
  arrange(desc(central_hist))

  # can also use `str_detect` for specific storms:

test_bayesian_rowwise_hist_95_cis %>% 
  unnest(impact_summary) %>% 
  unnest(impact_summary) %>%
  mutate(intervals = rep(c("central_hist", "upper_hist", "lower_hist") )) %>%  pivot_wider(values_from = impact_summary, names_from = intervals) %>% unnest(data) %>% filter(str_detect(storm_id, "Katrina")) %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID"))  %>% ggplot() + 
  aes(geometry = geometry, fill = central_hist) +
  geom_sf() + 
  scale_fill_viridis_c(option = "cividis") + 
  theme_void()

# OK, so even w/ filtering, there are still 1000 years represented here

test_bayesian_rowwise_hist_95_cis %>% 
  unnest(impact_summary) %>% 
  unnest(impact_summary) %>%
  mutate(intervals = rep(c("central", "upper", "lower") )) %>%
  pivot_wider(values_from = impact_summary, names_from = intervals) %>% 
  dplyr::select(-impact, - data) %>% 
  group_by(gridid) %>% 
  dplyr::summarise(., across(where(is.numeric), sum)) %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = central / 30) + # 2018 - 1988 -- ideally find one starting in 1980 
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void()



# ideally track down IBTrACS or something similar 

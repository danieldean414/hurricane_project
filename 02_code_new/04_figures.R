
# really could improve naming conventions;
  # these are the version prefiltered to storms reaching at least a cat 1 hurricane
#load("01_data/test_bayesian_rowwise_95_cis.rda") #full 10k years (runs, but very slowly)
#load("01_data/test_bayesian_rowwise_95_cis_working.rda") #1000-year version

load(file = "01_data/test_bayesian_rowwise_95_cis_2K.rda")
test_bayesian_rowwise_95_cis_2k <- test_bayesian_rowwise_storm_95_cis
  # for later: should set up months as a 'native' column


load(file = "01_data/test_bayesian_rowwise_95_cis_hist.rda")
test_bayesian_rowwise_95_cis_hist <- test_bayesian_rowwise_hist_95_cis
  # I guess should have matched names upstream

#rm(test_bayesian_rowwise_95_cis) # I guess downside of recycling names
  # probably better to just name appropriately the 1st time

load("us_counties_wgs84.rda")

  # also version w/o the summary column, but I guess may as well use it
  # so 200 decades -- maybe "hard code" with a paramter up-front?

n_decades <- 2000/10 # just to minimize chances of mixing up...

#test_bayesian_rowwise_95_cis <- test_bayesian_rowwise_95_cis %>% filter(str_detect(storm_id, "[3|4]$"))
  # shortening for ~testing purposes
  # so now represent 2000 years, nominally

#load("01_data/test_bayesian_rowwise_hist95_cis.rda")
  #full dataset

# load("test_bayesian_rowwise_95_cis_100_yr.rda")

# loads from test_bayesian_rowwise_95_cis_working.rda

#test_bayesian_rowwise_95_cis <- test_bayesian_rowwise_95_cis_working %>% 
#  filter(as.numeric(str_extract(storm_id, "\\d{4}")) <= 100)

#just under 1/10th the length, so checks out; still ~55k rows

#save(test_bayesian_rowwise_95_cis, 
#     file = "test_bayesian_rowwise_95_cis_100_yr.rda")

# Since I'm going to be rerunning anyway, no real reason to use 1000 yrs
  # let's try e.g. 100 so I can get through planning/trial steps faster

# map; might be missing some setup/context

  # oh, need to go back and filter out counties with <100 people


##############################################################
#            Fig 1: Regions and Populations                  #
##############################################################


# OK, so thinking about how to handle uncertainty again;
  # initial approach of just pooling all impacts doesn't seem well-founded
  # in that at nothing else, counties have ~fixed traits in the model
  # so I'd say either use a simple approach of adding e.g. all 2.5th percentiles
  # or pool impact estimates by county for uncertainty and add across regions

#geometry included in population
  # wow, already ~6 minutes w/ 100 years, so ~10 hrs with linear scaling
    # any way to pare down?
  # huh, more like a minute this time (did remove `impact` section)
    # not sure if that helped, or if it's just ~noise

# so this should be exp. per 10k years; should be ~1000-8000 ish for coasts?
  # huh--seems like; max is only ~1200, and Q3 is ~800
    # any way I'm missing data?
    # Ohh--need to add `totals` in there as well

# remove storm IDs, etc.; exposures averages across
  # output is # per 10,000 years 

  # don't get mixed up with ~per-region calculations

  # not sure if I want to set up #/decade this early...

########################################33
########################################

  # testing version with impacts (by percentile/county)
  # replace orig. version if it works
  # had tried getting 95% CIs across all (vs adding up e.g. 2.5%s)
    # but not sure that would be accurate
    # b/c it's modeling discrete storms,not bulk ~random noise

# filter to Rhode Island (5 counties; cleaner than e.g. arbitrary # rows):
  # filter(str_detect(gridid, "^44")) %>%

  # also adding in geometery; seems more efficient than 2 versions of each
    # I guess see if the geometry column adds too much 'mass'; worst case
    # could remove 


# OK, "live" versions are 
  # tc_exp_impact_counties_months
  # tc_exp_impact_counties
# And add up to get larger regions (rather than ~re-sample)

impact_na <- tibble(exposures = as.integer(0),
                    mean = as.double(0),
                    lower = as.double(0),
                    upper = as.double(0))

tc_exp_impact_counties <- (test_bayesian_rowwise_95_cis_2k %>% 
                             dplyr::select(-impact) %>%
                             unnest(data) %>%
                             dplyr::select(gridid, storm_id, id, age_65_plus,
                                           date_time_max_wind, vmax_sust, impact_summary) %>%
                             filter(age_65_plus >= 100) %>%
                             mutate(hurricane = case_when(vmax_sust >= 32.9 ~ "hurricane_force",
                                                          vmax_sust < 32.9 ~ "sub_hurricane_force")) %>%
                             group_by(gridid, hurricane, age_65_plus) %>%
                             #summarise(exposures = n()) %>%
                             add_count(name = "exposures") %>% 
                             mutate(impact_lower = purrr::map(.x = impact_summary,
                                                              .f = ~.x[1]),
                                    impact_center = purrr::map(.x = impact_summary, 
                                                               .f = ~.x[2]), 
                                    impact_upper = purrr::map(.x = impact_summary,
                                                              .f = ~.x[3])) %>%
                             dplyr::select(-storm_id, -id, -date_time_max_wind,
                                           -vmax_sust, -impact_summary) %>%
                             unique() %>% 
                             arrange(gridid)) %>%
  unnest(c(impact_lower, impact_center, impact_upper)) %>%
  group_by(gridid, age_65_plus, hurricane) %>% 
  mutate(mean = sum(mean), lower = sum(lower), upper = sum(upper)) %>% 
  unique() %>% 
  nest() %>% 
  pivot_wider(names_from = hurricane, values_from = data)  %>%
  replace_na(list(hurricane_force = list(impact_na))) %>%
  mutate(total_exposures = purrr::map2(.x = sub_hurricane_force,
                                       .y = hurricane_force,
                                       .f = ~(.x + .y))) %>% 
  pivot_longer(cols = c(sub_hurricane_force, 
                        hurricane_force, total_exposures),
               names_to = "tc_category", 
               values_to = "impact_summary") %>% 
  unnest(impact_summary) %>%
  left_join((us_counties_wgs84 %>%
               dplyr::select(GEOID, NAME,
                             state, region, geometry)),
            by = c("gridid" = "GEOID")) 
    # weird--runs much faster than I expected 


tc_exp_impact_regions <- tc_exp_impact_counties %>%
  group_by(region, tc_category) %>%
  mutate(exposures = sum(exposures),
  age_65_plus = sum(age_65_plus),
  mean = sum(mean),
  lower = sum(lower),
  upper = sum(upper),
  geometry = st_union(geometry)
  ) %>%
  dplyr::select(-gridid, -state, -NAME) %>%
  unique()


tc_exp_impact_states <- tc_exp_impact_counties %>%
  group_by(state, tc_category) %>%
  mutate(exposures = sum(exposures),
         age_65_plus = sum(age_65_plus),
         mean = sum(mean),
         lower = sum(lower),
         upper = sum(upper),
         geometry = st_union(geometry)
  ) %>%
  dplyr::select(-gridid, -NAME) %>%
  unique()
  
  

# monthly version of combined object
tc_exp_impact_counties_months <- (test_bayesian_rowwise_95_cis_2k %>% 
                                    dplyr::select(-impact) %>%
                                    unnest(data) %>%
                                    dplyr::select(gridid, storm_id, id, age_65_plus,
                                                  date_time_max_wind, vmax_sust, impact_summary) %>%
                                    filter(age_65_plus >= 100) %>%
                                    mutate(hurricane = case_when(vmax_sust >= 32.9 ~ "hurricane_force",
                                                                 vmax_sust < 32.9 ~ "sub_hurricane_force"),
                                           month = lubridate::month(date_time_max_wind)) %>%
                                    group_by(gridid, hurricane, age_65_plus, month) %>%
                                    #summarise(exposures = n()) %>%
                                    add_count(name = "exposures") %>% 
                                    mutate(impact_lower = purrr::map(.x = impact_summary,
                                                                     .f = ~.x[1]),
                                           impact_center = purrr::map(.x = impact_summary, 
                                                                      .f = ~.x[2]), 
                                           impact_upper = purrr::map(.x = impact_summary,
                                                                     .f = ~.x[3])) %>%
                                    dplyr::select(-storm_id, -id, -date_time_max_wind,
                                                  -vmax_sust, -impact_summary) %>%
                                    unique() %>% 
                                    arrange(gridid)) %>%
  unnest(c(impact_lower, impact_center, impact_upper)) %>%
  group_by(gridid, age_65_plus, hurricane, month) %>% 
  mutate(mean = sum(mean), lower = sum(lower), upper = sum(upper)) %>% 
  unique() %>% 
  nest() %>% 
  pivot_wider(names_from = hurricane, values_from = data)  %>%
  replace_na(list(hurricane_force = list(impact_na))) %>%
  mutate(total_exposures = purrr::map2(.x = sub_hurricane_force,
                                       .y = hurricane_force,
                                       .f = ~(.x + .y))) %>% 
  pivot_longer(cols = c(sub_hurricane_force, 
                        hurricane_force, total_exposures),
               names_to = "tc_category", 
               values_to = "impact_summary") %>% 
  unnest(impact_summary) %>%
  left_join((us_counties_wgs84 %>%
               dplyr::select(GEOID, NAME,
                             state, region, geometry)),
            by = c("gridid" = "GEOID")) 

#

tc_exp_impact_months_regions <- tc_exp_impact_counties_months %>%
  group_by(region, tc_category, month) %>%
  mutate(exposures = sum(exposures),
         age_65_plus = sum(age_65_plus),
         mean = sum(mean),
         lower = sum(lower),
         upper = sum(upper),
         geometry = st_union(geometry)
  ) %>%
  dplyr::select(-gridid, -state, -NAME) %>%
  unique()


tc_exp_impact_months_states <- tc_exp_impact_counties_months %>%
  group_by(state, tc_category) %>%
  mutate(exposures = sum(exposures),
         age_65_plus = sum(age_65_plus),
         mean = sum(mean),
         lower = sum(lower),
         upper = sum(upper),
         geometry = st_union(geometry)
  ) %>%
  dplyr::select(-gridid, -NAME) %>%
  unique()

    # historical versions (for e.g. supplemental figs)


tc_exp_impact_counties_hist <- (test_bayesian_rowwise_95_cis_hist %>% 
                             dplyr::select(-impact) %>%
                             unnest(data) %>%
                             dplyr::select(gridid, storm_id, id, age_65_plus,
                                           date_time_max_wind, vmax_sust, impact_summary) %>%
                             filter(age_65_plus >= 100) %>%
                             mutate(hurricane = case_when(vmax_sust >= 32.9 ~ "hurricane_force",
                                                          vmax_sust < 32.9 ~ "sub_hurricane_force")) %>%
                             group_by(gridid, hurricane, age_65_plus) %>%
                             #summarise(exposures = n()) %>%
                             add_count(name = "exposures") %>% 
                             mutate(impact_lower = purrr::map(.x = impact_summary,
                                                              .f = ~.x[1]),
                                    impact_center = purrr::map(.x = impact_summary, 
                                                               .f = ~.x[2]), 
                                    impact_upper = purrr::map(.x = impact_summary,
                                                              .f = ~.x[3])) %>%
                             dplyr::select(-storm_id, -id, -date_time_max_wind,
                                           -vmax_sust, -impact_summary) %>%
                             unique() %>% 
                             arrange(gridid)) %>%
  unnest(c(impact_lower, impact_center, impact_upper)) %>%
  group_by(gridid, age_65_plus, hurricane) %>% 
  mutate(mean = sum(mean), lower = sum(lower), upper = sum(upper)) %>% 
  unique() %>% 
  nest() %>% 
  pivot_wider(names_from = hurricane, values_from = data)  %>%
  replace_na(list(hurricane_force = list(impact_na))) %>%
  mutate(total_exposures = purrr::map2(.x = sub_hurricane_force,
                                       .y = hurricane_force,
                                       .f = ~(.x + .y))) %>% 
  pivot_longer(cols = c(sub_hurricane_force, 
                        hurricane_force, total_exposures),
               names_to = "tc_category", 
               values_to = "impact_summary") %>% 
  unnest(impact_summary) %>%
  left_join((us_counties_wgs84 %>%
               dplyr::select(GEOID, NAME,
                             state, region, geometry)),
            by = c("gridid" = "GEOID")) 
# weird--runs much faster than I expected 




# monthly version of combined object
tc_exp_impact_counties_months_hist <- (test_bayesian_rowwise_95_cis_hist %>% 
                                    dplyr::select(-impact) %>%
                                    unnest(data) %>%
                                    dplyr::select(gridid, storm_id, id, age_65_plus,
                                                  date_time_max_wind, vmax_sust, impact_summary) %>%
                                    filter(age_65_plus >= 100) %>%
                                    mutate(hurricane = case_when(vmax_sust >= 32.9 ~ "hurricane_force",
                                                                 vmax_sust < 32.9 ~ "sub_hurricane_force"),
                                           month = lubridate::month(date_time_max_wind)) %>%
                                    group_by(gridid, hurricane, age_65_plus, month) %>%
                                    #summarise(exposures = n()) %>%
                                    add_count(name = "exposures") %>% 
                                    mutate(impact_lower = purrr::map(.x = impact_summary,
                                                                     .f = ~.x[1]),
                                           impact_center = purrr::map(.x = impact_summary, 
                                                                      .f = ~.x[2]), 
                                           impact_upper = purrr::map(.x = impact_summary,
                                                                     .f = ~.x[3])) %>%
                                    dplyr::select(-storm_id, -id, -date_time_max_wind,
                                                  -vmax_sust, -impact_summary) %>%
                                    unique() %>% 
                                    arrange(gridid)) %>%
  unnest(c(impact_lower, impact_center, impact_upper)) %>%
  group_by(gridid, age_65_plus, hurricane, month) %>% 
  mutate(mean = sum(mean), lower = sum(lower), upper = sum(upper)) %>% 
  unique() %>% 
  nest() %>% 
  pivot_wider(names_from = hurricane, values_from = data)  %>%
  replace_na(list(hurricane_force = list(impact_na))) %>%
  mutate(total_exposures = purrr::map2(.x = sub_hurricane_force,
                                       .y = hurricane_force,
                                       .f = ~(.x + .y))) %>% 
  pivot_longer(cols = c(sub_hurricane_force, 
                        hurricane_force, total_exposures),
               names_to = "tc_category", 
               values_to = "impact_summary") %>% 
  unnest(impact_summary) %>%
  left_join((us_counties_wgs84 %>%
               dplyr::select(GEOID, NAME,
                             state, region, geometry)),
            by = c("gridid" = "GEOID")) 

##

tc_exp_impact_months_hist_regions <- tc_exp_impact_counties_months_hist %>%
  group_by(region, tc_category, month) %>%
  mutate(exposures = sum(exposures),
         age_65_plus = sum(age_65_plus),
         mean = sum(mean),
         lower = sum(lower),
         upper = sum(upper),
         geometry = st_union(geometry)
  ) %>%
  dplyr::select(-gridid, -state, -NAME) %>%
  unique()


tc_exp_impact_months_hist_states <- tc_exp_impact_counties_months_hist %>%
  group_by(state, tc_category, month) %>%
  mutate(exposures = sum(exposures),
         age_65_plus = sum(age_65_plus),
         mean = sum(mean),
         lower = sum(lower),
         upper = sum(upper),
         geometry = st_union(geometry)
  ) %>%
  dplyr::select(-gridid, -NAME) %>%
  unique()

##### 
save(tc_exp_impact_counties, file = "tc_exp_impact_counties.rda")
save(tc_exp_impact_regions, file = "tc_exp_impact_regions.rda")
save(tc_exp_impact_states, file = "tc_exp_impact_states.rda")
save(tc_exp_impact_months_regions, file = "tc_exp_impact_months_regions.rda")
save(tc_exp_impact_months_states, file = "tc_exp_impact_months_states.rda")
save(tc_exp_impact_counties_hist, file = "tc_exp_impact_counties_hist.rda")

save(tc_exp_impact_months_hist_regions, file = "tc_exp_impact_months_hist_regions.rda")
save(tc_exp_impact_months_hist_states, file = "tc_exp_impact_months_hist_states.rda")

load("us_states.rda")
########################3


# aggregating geometry for map backgrounds


us_states <- get_acs(
  geography = "state",
  variables = "B06012_001",
  year = 2020,
  geometry = TRUE
) %>% 
  dplyr::select(GEOID, NAME, geometry)

#tidycensus::state_laea #object included in `tidycensus`
# states (borders)

study_counties <- tc_exp_impact_counties %>%
  ungroup() %>%
  dplyr::select(state, region, NAME, geometry) %>%
  unique() %>%
  st_as_sf() %>%
  st_transform(crs = "WGS84")  

study_states <- tc_exp_impact_counties %>%
  ungroup() %>%
  dplyr::select(state, region) %>% 
  unique() %>%
  left_join(us_states, by = c('state' = 'NAME'))  %>%
  st_as_sf() %>%
  st_transform(crs = "WGS84")  

study_regions <- study_states %>%
  group_by(region) %>%
  mutate(geometry = st_union(geometry)) %>%
  dplyr::select(-state, -GEOID) %>%
  unique()

################################33

# Oh, I don't think regions, etc., should change w/ historical vs
  # synth. data



##############

#   #   #   #   #   #   #   #   #   #   #   #   #
  #   #   #   #   #   #   #   #   #   #   #   #   #
          # Figures & Summary values #
#   #   #   #   #   #   #   #   #   #   #   #   #
  #   #   #   #   #   #   #   #   #   #   #   #   #

tc_exp_impact_regions_months_boxplots <- tc_exp_impact_months_regions %>% 
  ggplot(aes(x = month, y = mean/200, ymin = lower/200,
             ymax = upper/200, color = tc_category)) + 
  geom_errorbar(position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6), 
             size = 0.5, shape = 4)+ 
  geom_point(aes(size = exposures/200), 
             alpha = 0.3,
             position = position_dodge(width = 0.6)) +
  facet_wrap(. ~ region) +
  theme_classic() + 
  geom_hline(yintercept = 0) +
  labs(x = "Month",
       y = "Attributable Excess Deaths per Decade",
       size = "Storms per Century",
       color = "Hurricane Status\n(>=32.9 m/s Exposures)", 
       title = "Simulated Tropical Cyclone Attributable Deaths by Region per Decade")
  

tc_exp_impact_regions_months_boxplots_hist <- tc_exp_impact_months_hist_regions %>% 
  ggplot(aes(x = month, y = mean/3, ymin = lower/3,
             ymax = upper/3, color = tc_category)) + 
  geom_errorbar(position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6), 
             size = 0.5, shape = 4)+ 
  geom_point(aes(size = exposures/3), 
             alpha = 0.3,
             position = position_dodge(width = 0.6)) +
  facet_wrap(. ~ region) +
  theme_classic() + 
  geom_hline(yintercept = 0) +
  labs(x = "Month",
       y = "Attributable Excess Deaths per Decade",
       size = "Storms per Century",
       color = "Hurricane Status\n(>=32.9 m/s Exposures)", 
       title = "Simulated Tropical Cyclone Attributable Deaths by Region per Decade")



# Maps

   # monthly -- actually might check out w/ annual
exposures_map_months <- ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region, alpha = 0.2),
          fill = NA,
          alpha = 0.2,
          linewidth = 4,
          data = study_regions) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = 'white', 
          color = 'black',
          data = study_states) +
  
  geom_sf(aes(geometry = geometry,
              fill = exposures/200),
          linewidth = 0.5, color = NA, 
          data = tc_exp_impact_counties_months) +
  
  
  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 0.5,
          data = study_regions)+ 
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5, 
          color = 'black',
          fill = NA, 
          data = study_states) +
  facet_wrap(. ~ tc_category + month, ncol = 6) +
  
  theme_void() +
  scale_fill_viridis_c(option = "turbo") +
  
  scale_color_viridis_d(option = "plasma") +
  labs(color = "NCEI Region",
       fill = "Cyclone Days per Decade")


exposures_map <- ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region, alpha = 0.2),
          fill = NA,
          alpha = 0.2,
          linewidth = 4,
          data = study_regions) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = 'white', 
          color = 'black',
          data = study_states) +
  
  geom_sf(aes(geometry = geometry,
              fill = exposures/200),
          linewidth = 0.5, color = NA, 
          data = tc_exp_impact_counties) +
  
  
  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 0.5,
          data = study_regions)+ 
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5, 
          color = 'black',
          fill = NA, 
          data = study_states) +
  facet_wrap(. ~ tc_category, ncol = 3) +
  
  theme_void() +
  scale_fill_viridis_c(option = "turbo") +
  
  scale_color_viridis_d(option = "plasma") +
  labs(color = "NCEI Region",
       fill = "Cyclone Days per Decade")


person_exposures_map <- ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region, alpha = 0.2),
          fill = NA,
          alpha = 0.2,
          linewidth = 4,
          data = study_regions) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = 'white', 
          color = 'black',
          data = study_states) +
  
  geom_sf(aes(geometry = geometry,
              fill = (age_65_plus * exposures)/200),
          linewidth = 0.5, color = NA, 
          data = tc_exp_impact_counties) +
  
  
  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 0.5,
          data = study_regions)+ 
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5, 
          color = 'black',
          fill = NA, 
          data = study_states) +
  facet_wrap(. ~ tc_category, ncol = 3) +
  
  theme_void() +
  scale_fill_viridis_c(option = "turbo", trans = "log") +
  
  scale_color_viridis_d(option = "plasma") +
  labs(color = "NCEI Region",
       fill = "Person-Exposures per Decade")


excess_mort_map <- ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region, alpha = 0.2),
          fill = NA,
          alpha = 0.2,
          linewidth = 4,
          data = study_regions) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = 'white', 
          color = 'black',
          data = study_states) +
  
  geom_sf(aes(geometry = geometry,
              fill = mean/200),
          linewidth = 0.5, color = NA, 
          data = tc_exp_impact_counties) +
  
  
  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 0.5,
          data = study_regions)+ 
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5, 
          color = 'black',
          fill = NA, 
          data = study_states) +
  facet_wrap(. ~ tc_category, ncol = 3) +
  
  theme_void() +
  scale_fill_viridis_c(option = "turbo") +
  
  scale_color_viridis_d(option = "plasma") +
  labs(color = "NCEI Region",
       fill = "Median Attributable Mortality per Decade")

#   #   #   #   #   #   #   #   #   #   #   #   #
  #   #   #   #   #   #   #   #   #   #   #   #   #
#   #   #   #   #   #   #   #   #   #   #   #   #
  #   #   #   #   #   #   #   #   #   #   #   #   #












# trying version with aggregating estimates across counties for CIs
  # OK, conceptually, that would be summarizing the ~range of 
  # impacts possible for a given storm/category, vs total impact
  # so I think adding storm estimates together makes more sense
    # for the intended metric
  # or report as ~impacts per-storm 
tc_exp_impact_agg_counties_months <- (test_bayesian_rowwise_95_cis %>% 
                                    dplyr::select(-impact_summary) %>%
                                    unnest(data) %>%
                                    dplyr::select(gridid, storm_id, id, age_65_plus,
                                                  date_time_max_wind, vmax_sust, impact) %>%
                                    filter(age_65_plus >= 100) %>%
                                    mutate(hurricane = case_when(vmax_sust >= 32.9 ~ "hurricane_force",
                                                                 vmax_sust < 32.9 ~ "sub_hurricane_force"),
                                           month = lubridate::month(date_time_max_wind)) %>%
                                    group_by(gridid, age_65_plus, month) %>%
                                      mutate(impact_total = purrr::map(.x = impact, 
                                                                             .f = ~rbind(.x))) %>%
                                      group_by(gridid, hurricane, age_65_plus, month) %>%
                                    #summarise(exposures = n()) %>%
                                    add_count(name = "exposures") %>% 
                                    mutate(impact_lower = purrr::map(.x = impact_summary,
                                                                     .f = ~.x[1]),
                                           impact_center = purrr::map(.x = impact_summary, 
                                                                      .f = ~.x[2]), 
                                           impact_upper = purrr::map(.x = impact_summary,
                                                                     .f = ~.x[3])) %>%
                                    dplyr::select(-storm_id, -id, -date_time_max_wind,
                                                  -vmax_sust, -impact_summary) %>%
                                    unique() %>% 
                                    arrange(gridid)) %>%
  unnest(c(impact_lower, impact_center, impact_upper)) %>%
  group_by(gridid, age_65_plus, hurricane, month) %>% 
  mutate(mean = sum(mean), lower = sum(lower), upper = sum(upper)) %>% 
  unique() %>% 
  nest() %>% 
  pivot_wider(names_from = hurricane, values_from = data)  %>%
  replace_na(list(hurricane_force = list(impact_na))) %>%
  mutate(total_exposures = purrr::map2(.x = sub_hurricane_force,
                                       .y = hurricane_force,
                                       .f = ~(.x + .y))) %>% 
  pivot_longer(cols = c(sub_hurricane_force, 
                        hurricane_force, total_exposures),
               names_to = "tc_category", 
               values_to = "impact_summary") %>% 
  unnest(impact_summary) %>%
  left_join((us_counties_wgs84 %>%
               dplyr::select(GEOID, NAME,
                             state, region, geometry)),
            by = c("gridid" = "GEOID"))

  #could aggregate previous version from this

 #OK, and now should be easier to aggregate for plotting

  # OK, I'm seeing 140019243 as the counties' total population
  # 140,019,243; for reference,~330,000,000  in US; *way* too high
    # OK, I hadn't un-grouped by month early enough; now getting 27,469,952

  # oh, but since I *do* want to keep months, etc., in there,
    # I guess could nest everything but gridid and region, 
      # then add up, and unnest

tc_exp_impact_regions_months  <- tc_exp_impact_counties_months %>%
  ungroup() %>% 
  group_by(gridid, region, 
           age_65_plus) %>% 
  nest() %>% 
  group_by(region) %>% 
  mutate(age_65_plus_region = sum(age_65_plus)) %>%
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  dplyr::select(region, month, 
                tc_category, 
                age_65_plus_region, 
                exposures, mean, 
                lower, upper) %>% 
  unique() %>% 
  group_by(region, month,
           tc_category) %>%
  mutate(exposures = sum(exposures),
         mean = sum(mean), 
         lower = sum(lower),
         upper = sum(upper),
         age_65_plus_region = mean(age_65_plus_region)) %>%
  unique() 


# adapting a plot: 
tc_exp_impact_regions_months %>% 
  ggplot(aes(x = month, y = mean/200, ymin = lower/200,
             ymax = upper/200, color = tc_category)) + 
  geom_errorbar(position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6))+ 
  geom_point(aes(size = exposures/200), 
             alpha = 0.3,
             position = position_dodge(width = 0.6)) +
  facet_wrap(. ~ region) +
  theme_classic() + 
  geom_hline(yintercept = 0) +
  labs(x = "Month",
       y = "Attributable Excess Deaths per Decade",
       size = "Storms per Century",
       color = "Hurricane Status\n(>=32.9 m/s Exposures)", 
       title = "Simulated Tropical Cyclone Attributable Deaths by Region per Century") %>%


#################################################33
##################################################3
## Trying aggregating by a larger region while keeping counties constant
  # oh, actually wouldn't need to change
  # just instead of un-nessting and adding the 95% CIs, 
    # calculate new ones from pooled impacts

  # OK, one other difference: current bifurcation between hurricane,
    # sub-hurricane, effects doesn't support the same workflow
  # oh, could do 2 'group_by_ operations with different names and then 
  # pivot_longer?
    # so would need to pivot_wider for over/under hurricane first, then 
    # pivot longer with the 3 columns and decide how to handle NULL results


impact_na <- tibble(exposures = as.integer(0),
                    mean = as.double(0),
                    lower = as.double(0),
                    upper = as.double(0))

tc_exp_impact_counties <- (test_bayesian_rowwise_95_cis %>% 
                             dplyr::select(-impact_summary) %>%
                             unnest(data) %>%
                             dplyr::select(gridid, storm_id, id, age_65_plus,
                                           date_time_max_wind, vmax_sust, impact) %>%
                             filter(age_65_plus >= 100) %>%
                             mutate(hurricane = case_when(vmax_sust >= 32.9 ~ "hurricane_force",
                                                          vmax_sust < 32.9 ~ "sub_hurricane_force")) %>%
                             group_by(gridid, hurricane, age_65_plus) %>%
                             #summarise(exposures = n()) %>%
                             add_count(name = "exposures") %>% 
                             mutate(impact_lower = purrr::map(.x = impact,
                                                              .f = ~.x[1]),
                                    impact_center = purrr::map(.x = impact_summary, 
                                                               .f = ~.x[2]), 
                                    impact_upper = purrr::map(.x = impact_summary,
                                                              .f = ~.x[3])) %>%
                             dplyr::select(-storm_id, -id, -date_time_max_wind,
                                           -vmax_sust, -impact_summary) %>%
                             unique() %>% 
                             arrange(gridid)) %>%
  unnest(c(impact_lower, impact_center, impact_upper)) %>%
  group_by(gridid, age_65_plus, hurricane) %>% 
  mutate(mean = sum(mean), lower = sum(lower), upper = sum(upper)) %>% 
  unique() %>% 
  nest() %>% 
  pivot_wider(names_from = hurricane, values_from = data)  %>%
  replace_na(list(hurricane_force = list(impact_na))) %>%
  mutate(total_exposures = purrr::map2(.x = sub_hurricane_force,
                                       .y = hurricane_force,
                                       .f = ~(.x + .y))) %>% 
  pivot_longer(cols = c(sub_hurricane_force, 
                        hurricane_force, total_exposures),
               names_to = "tc_category", 
               values_to = "impact_summary") %>% 
  unnest(impact_summary) %>%
  left_join((us_counties_wgs84 %>%
               dplyr::select(GEOID, NAME,
                             state, region, geometry)),
            by = c("gridid" = "GEOID"))


########################################33
###########################################

exp_person_summary <- test_bayesian_rowwise_95_cis %>%
  dplyr::select(-impact, -impact_summary) %>%
  unnest(data) %>%
  dplyr::select(gridid, storm_id, id, age_65_plus,
                date_time_max_wind, vmax_sust) %>%
  filter(age_65_plus >= 100) %>%
  mutate(hurricane = case_when(vmax_sust >= 32.9 ~ "hurricane force",
                               vmax_sust < 32.9 ~ "sub-hurricane force")) %>%
  group_by(gridid, hurricane, age_65_plus) %>%
  #summarise(exposures = n()) %>%
  add_count(name = "exposures") %>%
  mutate(exposures = exposures / n_decades) %>%
  dplyr::select(-storm_id, -id, -date_time_max_wind, -vmax_sust) %>%
  unique() %>%
  pivot_wider(names_from = hurricane,
              values_from = exposures) %>%
  mutate("total" = sum(`hurricane force`, 
                       `sub-hurricane force`, 
                       na.rm = TRUE)) %>%
  pivot_longer(cols = c(`hurricane force`, 
                        `sub-hurricane force`,
                        `total`),
               names_to = "tc_cat",
               values_to = "exposures") %>%
  mutate(person_exposures = exposures * 
           age_65_plus)


# monthly version


exp_person_summary_months <- test_bayesian_rowwise_95_cis %>%
  dplyr::select(-impact, -impact_summary) %>%
  unnest(data) %>%
  dplyr::select(gridid, storm_id, id, age_65_plus,
                date_time_max_wind, vmax_sust) %>%
  filter(age_65_plus >= 100) %>%
  mutate(hurricane = case_when(vmax_sust >= 32.9 ~ "hurricane force",
                               vmax_sust < 32.9 ~ "sub-hurricane force")) %>%
mutate(month = lubridate::month(date_time_max_wind),
month = factor(month,
               levels = c(6,7,8,9,10,11))) %>% 
  mutate(month_group = case_when(month %in% c(6,7) ~ "Early",
                                 month %in% c(8,9) ~ "Middle",
                                 month %in% c(10,11) ~ "Late")) %>%
  group_by(gridid, hurricane, month) %>%
  add_count(name = "exposures") %>%
  mutate(exposures = exposures/n_decades) %>%
  dplyr::select(-storm_id, -id, -date_time_max_wind, -vmax_sust) %>%
  unique() %>%
  pivot_wider(names_from = hurricane,
              values_from = exposures) %>%
  mutate("total" = sum(`hurricane force`, 
                       `sub-hurricane force`, 
                       na.rm = TRUE)) %>%
  pivot_longer(cols = c(`hurricane force`, 
                        `sub-hurricane force`,
                        `total`),
               names_to = "tc_cat",
               values_to = "exposures") %>%
  mutate(person_exposures = exposures * 
           age_65_plus)



exp_person_summary_months <- test_bayesian_rowwise_95_cis %>%
  dplyr::select(-impact, -impact_summary) %>%
  unnest(data) %>%
  dplyr::select(gridid, storm_id, id, age_65_plus,
                date_time_max_wind, vmax_sust) %>%
  filter(age_65_plus >= 100) %>%
  mutate(hurricane = vmax_sust >= 32.9) %>%
  mutate(#month = as.factor(
    #as.numeric(
    #  gsub((str_extract(storm_id, "-\\d{1,2}-")),
    #                             pattern = "-", 
    #                             replacement = ""))), 
    month = lubridate::month(date_time_max_wind),
    month = factor(month,
                   levels = c(6,7,8,9,10,11))
  
    ) %>% 
  mutate(month_group = case_when(month %in% c(6,7) ~ "Early",
                                 month %in% c(8,9) ~ "Middle",
                                 month %in% c(10,11) ~ "Late")) %>%
  group_by(gridid, hurricane, month) %>%
  #summarise(exposures = n()) %>%
  add_count(name = 'exposures') %>%
  dplyr::select(-storm_id, -id, -date_time_max_wind, -vmax_sust) %>%
  unique() %>%
  pivot_wider(names_from = hurricane,
              values_from = exposures) %>%
  mutate("total" = sum(`hurricane force`, 
                       `sub-hurricane force`, 
                       na.rm = TRUE)) %>%
  pivot_longer(cols = c(`hurricane force`, 
                        `sub-hurricane force`,
                        `total`),
               names_to = "tc_cat",
               values_to = "exposures") %>%
  mutate(person_exposures = exposures * 
           age_65_plus)

# grouping by 2-month periods


exp_person_summary_month_groups <- test_bayesian_rowwise_95_cis %>%
  dplyr::select(-impact, -impact_summary) %>%
  unnest(data) %>%
  dplyr::select(gridid, storm_id, id, age_65_plus,
                date_time_max_wind, vmax_sust) %>%
  filter(age_65_plus >= 100) %>%
  mutate(hurricane = vmax_sust >= 32.9) %>%
  mutate(month = lubridate::month(date_time_max_wind),
  #mutate(month = as.factor(
  #  as.numeric(
  #    gsub((str_extract(storm_id, "-\\d{1,2}-")),
  #         pattern = "-", 
  #         replacement = ""))), 
    month = factor(month,
                   levels = c(6,7,8,9,10,11))
    
  ) %>% 
  mutate(month_group = case_when(month %in% c(6,7) ~ "Early",
                                 month %in% c(8,9) ~ "Middle",
                                 month %in% c(10,11) ~ "Late")) %>%
  group_by(gridid, hurricane, month_group) %>%
  add_count(name = 'exposures') %>%
  mutate(person_exposures = exposures * age_65_plus) %>%
  dplyr::select(gridid, age_65_plus, hurricane,
                month, month_group, exposures, person_exposures) %>%
  unique() %>%
  ungroup() %>%
  mutate(most_impacted = person_exposures >= quantile(person_exposures,
                                                      probs = c(0.995)),
         most_populated = age_65_plus >= quantile(age_65_plus,
                                                      probs = c(0.995)))


#
  # remember 'default' is aggregated over full time period
study_regions <- exp_person_summary %>%
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>%
  dplyr::select(gridid,
                age_65_plus, 
                exposures,
                person_exposures, 
                tc_cat,
                #most_impacted,
                #most_populated,
                NAME,
                state, 
                region, geometry) %>%
  unique()

study_regions_months <- exp_person_summary_months %>%
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>%
  dplyr::select(gridid,
                age_65_plus, 
                exposures,
                person_exposures, 
                tc_cat,
                month,
                month_group,
                #most_impacted,
                #most_populated,
                NAME,
                state, 
                region, geometry) %>%
  unique()


# doing it this way so  I get whole geometry boundaries
  # temporary object, though, I think

# weird! For some reason `unique()` no longer works for the us_counties object
  # does for some derivatives


## Plot of Person-Exposures Per Decade
  # lets do total, as well as supplemental figure

exp_person_summary_months

################################33

    ### GEOGRAPHY OBJECTS

###############################

# loading states separately

us_states <- get_acs(
  geography = "state",
  variables = "B06012_001",
  year = 2020,
  geometry = TRUE
) %>% 
  dplyr::select(GEOID, NAME, geometry)

#tidycensus::state_laea #object included in `tidycensus`
 # states (borders)

# States; using same CRS
study_regions_states <- study_regions %>%
  ungroup() %>%
  dplyr::select(state, region) %>% 
  unique() %>%
  left_join(us_states, by = c('state' = 'NAME'))  %>%
  st_as_sf() %>%
  st_transform(crs = "WGS84")

# NCEI regions
study_regions_ncei <- study_regions_states %>%
  group_by(region) %>%
  mutate(geometry = st_union(geometry)) %>%
  dplyr::select(-state) %>%
  unique()

# for blank counties;

study_regions_states_counties <- us_counties_wgs84 %>% 
  dplyr::select(-NAME, -GEOID, -variable, -estimate, -moe) %>%
  filter(state %in% study_regions$state) 

# let's save object(some take a while to run)


save(study_regions_states_counties, file = "study_regions_states_counties.rda")
save(study_regions_ncei, file = "study_regions_ncei.rda")
save(study_regions_states, file = "study_regions_states.rda")
save(us_states, file = "us_states.rda")
save(study_regions, file = "study_regions.rda")
save(exp_person_summary_month_groups, file = "exp_person_summary_month_groups.rda")
save(exp_person_summary_months, file = "exp_person_summary_months.rda")
save(exp_person_summary, file = "exp_person_summary.rda")
##########################################################3
##############################################################
### Quick plot demontrating storm path & calculations

# setting up demonstration versions of storm geomtery
  # so I can remove the full objects (take up a lot of memory)

# Hopefully just an ~administrative issue, but
  # looks like the `hurricane` version of the data
  # scrambled storm ID names
    # this is basically an ID column, so shouldn't impact
    # data integrity, but still something to note

demo_storm_track <- storm_2k_all_obs_na_proc_ggw %>%
  filter(storm_id == "0-0903-8-4") %>%
  #mutate(longitude = longitude - 360) %>%
  st_as_sf(coords = c('longitude', 'latitude'), 
           crs = "WGS84")

demo_storm_exposures <- storm_10k_all_obs_na_proc_hurricane_ggw %>%
  filter(!is.na(date_time_max_wind)) %>%
  filter(storm_id == "0-0903-8-4") %>%
  left_join(study_regions %>% dplyr::select(gridid, geometry),
            by = "gridid")

save(demo_storm_exposures, file = "demo_storm_exposures.rda")
save(demo_storm_track, file = "demo_storm_track")

wind_calc_plot <- ggplot() +
  aes(geometry = geometry) +
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = 'white', 
          color = 'black',
          data = study_regions_states) +
  #geom_sf(data = study_regions) +
  geom_sf(data = demo_storm_exposures, 
          aes(fill = vmax_sust, 
              alpha = vmax_sust >= 17.4)) +
  geom_sf(data = demo_storm_track,
                  aes(color = wind)) +
    theme_void() +
  scale_color_viridis_c(option = "plasma") +
  scale_fill_viridis_c(option = "cividis") +
  labs(color = "Instantaneous Windspeed (m/s)",
       alpha = "Tropical Storm Inclusion Threshold (17.4 m/s)",
      fill = "Modeled County-Level Windspeed",
      title = "Tropcial Cyclone Exposure Estimation Example")
 
  
   ggsave(wind_calc_plot,
          filename = "Fig_S_n_exposure_estimates_CORRECT.png",
         scale = 4.5)



##########################################################
   
   
##########################################################

# combined plot attempt
  # showing individual blank counties looks nice, but don't think it's adding any new info
    # and would guess it brings up render time a fair amount
combined_pop_region_plot <- ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region, alpha = 0.2),
          fill = NA,
          alpha = 0.2,
          linewidth = 4,
          data = study_regions_ncei) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = 'white', 
          color = 'black',
          data = study_regions_states) +
  
  
  geom_sf(aes(geometry = geometry,
              fill = age_65_plus),
          linewidth = 0.5, color = NA, 
          data = study_regions) +


  
  
  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 0.5,
          data = study_regions_ncei)+ 
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5, 
          color = 'black',
          fill = NA, 
          data = study_regions_states) +

  theme_void() +
  scale_fill_viridis_c(option = "turbo", trans = "log") +
  scale_color_manual(values = c("#b9c85eff", "#83a4a3ff",
                                "#efdf8bff", "#dbb35bff")) +
  labs(color = "NCEI Region",
       fill = "County Population (65+);\n (Log Scale)")
#c("#44809cff", "#716a3aff","#ba62e7ff", "#f69a94ff"))
# OK, don't think this is going to work as one map; going to make a small
  # "key" map to insert; might keep to bkg 'shading', though.

   
   
   
   
####################################
####################################   
   ### Fig 1: Population Map
####################################   
###################################

# population map:

combined_pop_region_main <-  ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region, alpha = 0.2),
          fill = NA,
          alpha = 0.2,
          linewidth = 4,
          data = study_regions_ncei) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = 'white', 
          color = 'black',
          data = study_regions_states) +
  
  geom_sf(aes(geometry = geometry,
              fill = age_65_plus),
          linewidth = 0.5, color = NA, 
          data = study_regions) +
  
  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 0.5,
          data = study_regions_ncei)+ 
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5, 
          color = 'black',
          fill = NA, 
          data = study_regions_states) +
  
  theme_void() +
  scale_fill_viridis_c(option = "turbo", trans = "log") +
  scale_color_viridis_d(option = "plasma") +
  labs(color = "NCEI Region",
       fill = "County Population (65+);\n (Log Scale)")

   
# exposures -- should be decades
   
combined_exp_region_main <-  ggplot() + 
     
     geom_sf(aes(geometry = geometry,
                 color = region, alpha = 0.2),
             fill = NA,
             alpha = 0.2,
             linewidth = 4,
             data = study_regions_ncei) +
     
     geom_sf(aes(geometry = geometry),
             linewidth = 0.5,
             fill = 'white', 
             color = 'black',
             data = study_regions_states) +
     
     geom_sf(aes(geometry = geometry,
                 fill = exposures),
             linewidth = 0.5, color = NA, 
             data = study_regions) +
     
     geom_sf(aes(geometry = geometry,
                 color = region),
             fill = NA,
             linewidth = 0.5,
             data = study_regions_ncei)+ 
     geom_sf(aes(geometry = geometry),
             linewidth = 0.5, 
             color = 'black',
             fill = NA, 
             data = study_regions_states) +
     
     theme_void() +
     scale_fill_viridis_c(option = "turbo") +
     scale_color_viridis_d(option = "plasma") +
     labs(color = "NCEI Region",
          fill = "Exposures per Decade (2000-year simulation)") + 
  facet_wrap(. ~ tc_cat, ncol = 2)
   
# person-exp


combined_exp_region_main <-  ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region, alpha = 0.2),
          fill = NA,
          alpha = 0.2,
          linewidth = 4,
          data = study_regions_ncei) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = 'white', 
          color = 'black',
          data = study_regions_states) +
  
  geom_sf(aes(geometry = geometry,
              fill = person_exposures),
          linewidth = 0.5, color = NA, 
          data = study_regions) +
  
  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 0.5,
          data = study_regions_ncei)+ 
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5, 
          color = 'black',
          fill = NA, 
          data = study_regions_states) +
  
  theme_void() +
  scale_fill_viridis_c(option = "turbo", trans = 'log') +
  scale_color_viridis_d(option = "plasma") +
  labs(color = "NCEI Region",
       fill = "Person-Exposures per Decade (2000-year simulation)") + 
  facet_wrap(. ~ tc_cat, ncol = 2)


# 'key' to inset

combined_pop_region_key <- ggplot() + 

  
  geom_sf(aes(geometry = geometry, 
              fill = region),
          linewidth = 0.5,
          data = study_regions_states) +

  theme_void() +
  #scale_fill_manual(values = c("#b9c85eff", "#83a4a3ff",
  #                             "#efdf8bff", "#dbb35bff")) +
  scale_fill_viridis_d(option = "plasma") +
  guides(fill="none")


library(patchwork)
# can polish, maybe manually rearrange, but pretty much OK

#combined_pop_region_key +  combined_pop_region_main

  # address redundant keys (maybe render as separate object?, titles)
  # feel like I remember there's a way to get 'clean' color scale #s

  # Ok, as long as I don't have a complete overlap, 
  #also opens some more latitude for colors

#3 Different Version

ggplot() + 
  
  
  geom_sf(aes(geometry = geometry, 
              fill = region),
          linewidth = 0.5,
          data = study_regions_states) +
  
  theme_void() +
  #scale_fill_manual(values = c("#b9c85eff", "#83a4a3ff",
  #                             "#efdf8bff", "#dbb35bff")) +
  scale_fill_viridis_d(option = "plasma") +
  geom_sf(aes(geometry = geometry,
              fill = NA),
          linewidth = 0.25, color = 'grey', 
          data = study_regions) + geom_sf(aes(geometry = geometry, 
                                              fill = NA),color = "black",
                                          linewidth = 0.5,
                                          data = study_regions_states) + 
  geom_sf(aes(geometry = geometry),
          size = 0.2,color = "white",
          data = (stormwindmodel::county_points %>%
                    st_as_sf(coords = c("glon", "glat"),
                             crs = st_crs(study_regions_states)) )) + 
  labs (fill = "NCEI Region")

##############################################################
#            Fig S2?: Exposures by Category              #
##############################################################

## NOTE: Need to remove indiv. storm data; exposures/ etc. 
  # is based on totals

# I'm thinking of 'promoting' this to a main results, and 
  # dropping the comibned metric; doesn't seem like it adds much info
exposures_map <- ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region, alpha = 0.2),
          fill = NA,
          alpha = 0.2,
          linewidth = 4,
          data = study_regions_ncei) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = 'white', 
          color = 'black',
          data = study_regions_states) +
  
  geom_sf(aes(geometry = geometry,
              fill = exposures/1000),
          linewidth = 0.5, color = NA, 
          data = study_regions) +
  

  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 0.5,
          data = study_regions_ncei)+ 

    geom_sf(aes(geometry = geometry),
          linewidth = 0.5, 
          color = 'black',
          fill = NA, 
          data = study_regions_states) +
  facet_wrap(. ~ tc_cat, ncol = 1) +
  
  theme_void() +
  scale_fill_viridis_c(option = "turbo") +

  scale_color_viridis_d(option = "plasma") +
  labs(color = "NCEI Region",
       fill = "TC Exposures per Decade")

 
ggsave(exposures_map, file = "FIG_exposures_map_vert.svg", scale = 5)  
ggsave(exposures_map, file = "FIG_exposures_map_vert.png", scale = 9)  

## person-exposures
#############################################
person_exposures_map <- ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region, alpha = 0.2),
          fill = NA,
          alpha = 0.2,
          linewidth = 4,
          data = study_regions_ncei) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = 'white', 
          color = 'black',
          data = study_regions_states) +
  
  geom_sf(aes(geometry = geometry,
              fill = person_exposures/1000),
          linewidth = 0.5, color = NA, 
          data = study_regions) +
  
  
  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 0.5,
          data = study_regions_ncei)+ 
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5, 
          color = 'black',
          fill = NA, 
          data = study_regions_states) +
  facet_wrap(. ~ tc_cat, nrow = 1) +
  
  theme_void() +
  scale_fill_viridis_c(option = "turbo") +
  
  scale_color_viridis_d(option = "plasma") +
  labs(color = "NCEI Region",
       fill = "TC Exposures per Decade")

###########################################3


# let's make a per-month version (supplemental)

exposures_map_months <- ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region, alpha = 0.2),
          fill = NA,
          alpha = 0.2,
          linewidth = 4,
          data = study_regions_ncei) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = 'white', 
          color = 'black',
          data = study_regions_states) +
  
  geom_sf(aes(geometry = geometry,
              fill = exposures/1000),
          linewidth = 0.5, color = NA, 
          data = study_regions_months) +
  
  
  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 0.5,
          data = study_regions_ncei)+ 
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5, 
          color = 'black',
          fill = NA, 
          data = study_regions_states) +
  facet_wrap(. ~ tc_cat + month, ncol = 6) +
  
  theme_void() +
  scale_fill_viridis_c(option = "turbo") +
  
  scale_color_viridis_d(option = "plasma") +
  labs(color = "NCEI Region",
       fill = "TC Exposures per Decade")

ggsave(exposures_map_months, file = "FIG_exposures_map_months.png", scale = 9) 

## Yeah, think I'm dropping person-exposures as at least a main result
  # just seems kind of muddled 


##############################################################
      ###  Fig 3: Map of (50th percentile) Impacts
#############################################################

##############################################################
#            Fig 2: Person-Exposures by Category              #
##############################################################
  
# OK, want to double-check how/whether the impacts are categorized
  #also remember I might want to look into months; derived variable

exp_person_summary2 <- exp_person_summary %>%
  dplyr::select(-person_exposures) %>%
  pivot_wider(names_from = hurricane,
              values_from = exposures) %>%
  mutate(total = sum(`TRUE`, `FALSE`, na.rm = TRUE)) %>%
  rename(tropical_storm = `FALSE`,
         hurricane = `TRUE`) %>%
  pivot_longer(cols = c(tropical_storm,
                        hurricane, total),
              names_to = "tc_type",
              values_to = "exposures") %>%
  mutate(person_exposures = age_65_plus * exposures)

exp_person_summary2_months <- exp_person_summary_months %>%
  dplyr::select(-person_exposures) %>%
  pivot_wider(names_from = hurricane,
              values_from = exposures) %>%
  mutate(total = sum(`TRUE`, `FALSE`, na.rm = TRUE)) %>%
  rename(tropical_storm = `FALSE`,
         hurricane = `TRUE`) %>%
  pivot_longer(cols = c(tropical_storm,
                        hurricane, total),
               names_to = "tc_type",
               values_to = "exposures") %>%
  mutate(person_exposures = age_65_plus * exposures)

# then mapping



#
exp_person_plot <- ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 4,
          data = study_regions_ncei) +
  
  geom_sf(aes(geometry = geometry),
          color = "grey40", 
          fill = 'white',
          data = study_regions_states_counties) +
  
  
  geom_sf(aes(geometry = geometry,
              fill = person_exposures),
          linewidth = 0.5, color = NA, 
          data = exp_person_summary2 %>%
            left_join(us_counties_wgs84,
                      by = c("gridid" = "GEOID"))) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = NA, data = study_regions_states) +
  
  facet_wrap(. ~ tc_type) +
  
  theme_void() +
  scale_fill_viridis_c(option = "turbo", trans = "log") +
  scale_color_manual(values = c("#b9c85eff", "#83a4a3ff",
                                "#efdf8bff", "#dbb35bff")) +
  labs(fill = "Person-Exposures (Log Scale)")

ggsave(exp_person_plot, file = "exp_person_plot_RD_1k.png", scale = 5)
  

exp_person_plot_months <- ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 4,
          data = study_regions_ncei) +
  
  geom_sf(aes(geometry = geometry),
          color = "grey40", 
          fill = 'white',
          data = study_regions_states_counties) +
  
  
  geom_sf(aes(geometry = geometry,
              fill = person_exposures),
          linewidth = 0.5, color = NA, 
          data = exp_person_summary2_months %>%
            left_join(us_counties_wgs84,
                      by = c("gridid" = "GEOID"))) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = NA, data = study_regions_states) +
  
  facet_wrap(. ~ tc_type + month, ncol = 6) +
  
  theme_void() +
  scale_fill_viridis_c(option = "turbo", trans = "log") +
  scale_color_manual(values = c("#b9c85eff", "#83a4a3ff",
                                "#efdf8bff", "#dbb35bff")) +
  labs(fill = "Person-Exposures (Log Scale)")

# just exposures


exp_plots_months <- ggplot() + 
  
  geom_sf(aes(geometry = geometry,
              color = region),
          fill = NA,
          linewidth = 4,
          data = study_regions_ncei) +
  
  geom_sf(aes(geometry = geometry),
          color = "grey40", 
          fill = 'white',
          data = study_regions_states_counties) +
  
  
  geom_sf(aes(geometry = geometry,
              fill = exposures/100), # manually putting in # years
          linewidth = 0.5, color = NA, 
          data = exp_person_summary2_months %>%
            left_join(us_counties_wgs84,
                      by = c("gridid" = "GEOID"))) +
  
  geom_sf(aes(geometry = geometry),
          linewidth = 0.5,
          fill = NA, data = study_regions_states) +
  
  facet_wrap(. ~ tc_type + months, ncol = 6) +
  
  theme_void() +
  scale_fill_viridis_c(option = "mako") +
  scale_color_manual(values = c("#b9c85eff", "#83a4a3ff",
                                "#efdf8bff", "#dbb35bff")) +
  labs(fill = "Exposures")

  #oh, could use as input for above plots (just need population and gridid)
    #fewer derived objects/steps


#####################################################################
##          Fig 3: Impacts (Map and Boxplots)    ##
#####################################################################

# need a different setup

exp_person_summary_OLD <- test_bayesian_rowwise_95_cis %>%
  dplyr::select(-impact_summary) %>%
  unnest(data) %>%
  dplyr::select(gridid, storm_id, id, age_65_plus,
                date_time_max_wind, vmax_sust) %>%
  filter(age_65_plus >= 100) %>%
  mutate(hurricane = vmax_sust >= 32.9) %>%
  group_by(gridid, hurricane, age_65_plus) %>%
  summarise(exposures = n()) %>%
  mutate(person_exposures = exposures * age_65_plus)


impact_summary_months <- test_bayesian_rowwise_95_cis %>%
  dplyr::select(-impact_summary) %>%
  unnest(data) %>%
  dplyr::select(gridid, storm_id, id, age_65_plus,
                date_time_max_wind, vmax_sust, impact) %>%
  filter(age_65_plus >= 100) %>%
  mutate(hurricane = vmax_sust >= 32.9) %>%
  mutate(month = as.numeric(
    gsub((str_extract(storm_id, "-\\d{1,2}-")),
         pattern = "-", 
         replacement = "")), 
    month = factor(month,
                   levels = c(6,7,8,9,10,11))
  ) %>%
  group_by(gridid, hurricane, month) %>%
  add_count() %>%
  mutate(impact_summary = 
           purrr::map(.x = impact,
                      .f = ~data.frame(
                        impact_estimate = quantile(.x,
                                                   probs = c(0.025, 0.5, 0.975)),
                        percentile = c("2.5_pct", "50_pct", "97.5_pct") )))

# I feel like adding percentiles seems valid?

# "average" impacts by state?
impact_summary_months_state <- test_bayesian_rowwise_95_cis %>%
  dplyr::select(-impact_summary) %>%
  unnest(data) %>%
  dplyr::select(gridid, storm_id, id, age_65_plus,
                date_time_max_wind, vmax_sust, impact) %>%
  filter(age_65_plus >= 100) %>%
  mutate(hurricane = vmax_sust >= 32.9) %>%
  mutate(month = as.numeric(
    gsub((str_extract(storm_id, "-\\d{1,2}-")),
         pattern = "-", 
         replacement = "")), 
    month = factor(month,
                   levels = c(6,7,8,9,10,11))
  ) %>%
  group_by(gridid, hurricane, month) %>%
  add_count() %>%
  mutate(impact_summary = 
           purrr::map(.x = impact,
                      .f = ~data.frame(
                        impact_estimate = quantile(.x,
                                                   probs = c(0.025, 0.5, 0.975)),
                        percentile = c("2.5_pct", "50_pct", "97.5_pct") )))


# region

impact_summary_months_region <- test_bayesian_rowwise_95_cis %>%
  dplyr::select(-impact_summary) %>%
  unnest(data) %>%
  mutate(month = lubridate::month(date_time_max_wind)) %>%
  mutate(month = factor(month,
                 levels = c(6,7,8,9,10,11))
  ) %>%
  dplyr::select(gridid, storm_id, id, age_65_plus,
                month, vmax_sust, impact) %>%
  left_join((study_regions %>%  ungroup() %>%
               dplyr::select(gridid, state, region,
                             exposures, person_exposures)),
            by = "gridid") %>%
  filter(age_65_plus >= 100) %>%
  mutate(hurricane = vmax_sust >= 32.9) %>%

  group_by(gridid, hurricane, month, region) %>%
  add_count() %>%
  mutate(impact_summary = 
           purrr::map(.x = impact,
                      .f = ~data.frame(
                        impact_estimate = quantile(.x,
                                                   probs = c(0.025, 0.5, 0.975)),
                        percentile = c("2.5_pct", "50_pct", "97.5_pct") )))


##################33
        ## Making a version w/ 'total' TC category for net impact


impact_summary_months_region <- test_bayesian_rowwise_95_cis %>%
  dplyr::select(-impact_summary) %>%
  unnest(data) %>%
  dplyr::select(gridid, storm_id, id, age_65_plus,
                date_time_max_wind, vmax_sust, impact) %>%
  left_join((study_regions %>%  ungroup() %>%
               dplyr::select(gridid, state, region,
                             exposures, person_exposures)),
            by = "gridid") %>%
  filter(age_65_plus >= 100) %>%
  mutate(hurricane = vmax_sust >= 32.9) %>%
  mutate(month = as.numeric(
    gsub((str_extract(storm_id, "-\\d{1,2}-")),
         pattern = "-", 
         replacement = "")), 
    month = factor(month,
                   levels = c(6,7,8,9,10,11))
  ) %>%
  group_by(gridid, hurricane, month, region) %>%
  add_count() %>%
  mutate(hurricane = factor(hurricane),
         hurricane = fct_recode(hurricane,
                               'hurricane_strength' = 'TRUE',
                               'sub-hurricane strength' = 'FALSE')) %>%
  mutate(impact_summary = 
           purrr::map(.x = impact,
                      .f = ~data.frame(
                        impact_estimate = quantile(.x,
                                                   probs = c(0.025, 0.5, 0.975)),
                        percentile = c("2.5_pct", "50_pct", "97.5_pct") )))


#  Liking this version: impacts by month and state
  # giving statewide totals, # of storms per century
impact_summary_months %>% 
  left_join(us_counties_wgs84, 
            by = c("gridid" ="GEOID")) %>% 
#  mutate(state = factor(state, levels = states_order)) %>%
  dplyr::select(gridid, age_65_plus, hurricane,
                month, impact_summary, state, 
                region, geometry) %>%
  group_by(gridid, month, hurricane) %>%
  add_count(name = "exposures") %>%
  unnest(impact_summary) %>% 
  group_by(state, hurricane, month, percentile) %>%
  add_count(name = "exposures_state") %>% 
  mutate(impact_total = sum(impact_estimate * (age_65_plus/100000)),
         total_population_65_plus = sum(age_65_plus)) %>%
  dplyr::select(-gridid, -age_65_plus, 
                -impact_estimate, -geometry) %>%
  unique() %>% 
  pivot_wider(names_from = percentile, 
              values_from = impact_total) %>%
  dplyr::select(-exposures) %>% 
  unique() %>%
  ggplot(aes(x = month, y = `50_pct`, ymin = `2.5_pct`,
             ymax = `97.5_pct`, color = hurricane)) + 
  geom_errorbar(position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6))+ 
  geom_point(aes(size = exposures_state), 
             alpha = 0.3,
             position = position_dodge(width = 0.6)) +
  facet_wrap(. ~ region + state) +
  theme_classic() + 
  geom_hline(yintercept = 0) +
  labs(x = "Month",
       y = "Attributable Excess Deaths (Totals)",
       size = "Storms per Decade",
       color = "Hurricane Status\n(>=32.9 m/s Exposures)", 
       title = "Simulated Tropical Cyclone Attributable Deaths by State per Century")

# this gives county-level impacts
  # remember "impact" is excess deaths per 100k 
  # so could get attr. deaths by county

###############

impact_summary_months_region %>% 
  #left_join(us_counties_wgs84, 
  #          by = c("gridid" ="GEOID")) %>% 
  #  mutate(state = factor(state, levels = states_order)) %>%
  dplyr::select(gridid, age_65_plus, hurricane,
                month, impact_summary, 
                #region, geometry) %>%
                region) %>%
  group_by(gridid, month, hurricane) %>%
  add_count(name = "exposures") %>%
  mutate(exposures = exposures/n_decades) %>%
  unnest(impact_summary) %>% 
  group_by(region, hurricane, month, percentile) %>%
  add_count(name = "exposures_region") %>% 
  mutate(impact_total = sum(impact_estimate * (age_65_plus/100000)),
         total_population_65_plus = sum(age_65_plus)) %>%
  dplyr::select(-gridid, -age_65_plus, 
                -impact_estimate) %>%
  unique() %>% 
  pivot_wider(names_from = percentile, 
              values_from = impact_total) %>%
  dplyr::select(-exposures) %>% 
  unique() %>%
  ggplot(aes(x = month, y = `50_pct`/200, ymin = `2.5_pct`,
             ymax = `97.5_pct`, color = hurricane)) + 
  geom_errorbar(position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6))+ 
  geom_point(aes(size = exposures_region/200), 
             alpha = 0.3,
             position = position_dodge(width = 0.6)) +
  facet_wrap(. ~ region) +
  theme_classic() + 
  geom_hline(yintercept = 0) +
  labs(x = "Month",
       y = "Attributable Excess Deaths (Totals)",
       size = "Storms per Decade",
       color = "Hurricane-Strength Exposure", 
       title = "Simulated Tropical Cyclone Attributable Deaths by NCEI Region per Decade") +
  scale_color_manual(values = c("gold4", "grey"))


#####################################################################
#####################################################################
#####################################################################






##########################
study_pops_and_demographics <- test_bayesian_rowwise_95_cis %>% 
  unnest(data) %>% 
  group_by(gridid) %>% 
  dplyr::select(poverty_prop:population_density) %>% 
  unique()  %>%
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = age_65_plus) +
  geom_sf() + 
  scale_fill_viridis_c(trans = 'log') + 
  theme_void()



# OK, merged NCEI data into the WS84 version of the counties 

# filtering to 1000 years to hopefully slow working times
  # wow, already >8 Gb just loading things, though!
#test_bayesian_rowwise_95_cis_working <- test_bayesian_rowwise_95_cis %>%
#  filter(str_detect(storm_id, "^1"))
#rm(test_bayesian_rowwise_95_cis)



# Have various exploratory plots up, but aiming to set up formal 'figures'

# adding saffir-simpson scale to categorize risks; I guess can think through how to generalize

saffir_simpson <- c(0, 74, 96, 111, 131) # maybe?


# ~supplementary plot showing explict geographic buffer (vs 'bounding box' approach


tc_attr_deaths_summary <- test_bayesian_rowwise_95_cis_working %>% 
  unnest(impact_summary) %>%
  ungroup() %>% 
  unnest(data) %>% 
  dplyr::select(gridid, date_time_max_wind, mean, vmax_sust) %>% 
  mutate(year = lubridate::year(date_time_max_wind)) %>%
  dplyr::select(-date_time_max_wind) %>% 
  unique() %>% 
  mutate(hurricane = as.factor(vmax_sust >= 32.9),
         hurricane = fct_recode(hurricane, "tc" = "FALSE", "hurricane" ="TRUE")) %>% 
  group_by(year, gridid, hurricane) %>%
  pivot_wider(names_from = hurricane, values_from = mean) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(cols = c(tc, hurricane), names_to = "tc_status", 
               values_to = "mean_attr_deaths") %>% 
 left_join(us_counties_wgs84, by = c('gridid' = 'GEOID'))



tc_attr_deaths_summary %>%
  group_by(year, gridid, tc_status) %>%
  mutate(mean_attr_deaths = sum(mean_attr_deaths)) %>%
  dplyr::select(-vmax_sust) %>%
  unique() %>%
  ggplot() +
  aes(x = region,
      y = mean_attr_deaths,
      color = tc_status) + 
  geom_violin(draw_quantiles = c(0.5)) + 
  theme_minimal()



# wrong section 
ggplot() +
  geom_sf(
    aes(geometry = geometry,
        fill = mean_attr_deaths),
    data = tc_attr_deaths_summary
  ) +
  geom_sf(
    aes(geometry = geometry,
        color = region)
  ) +
  scale_fill_viridis_c(option = "turbo") +
  theme_void()


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

## Comparing TC windspeeds

ggplot() +
  geom_histogram(data = storm_hist_proc_hurricane,
                 aes(x = wind,
                     y = after_stat(count / sum(count))),
                 fill = 'black',
                 alpha = 0.5) +
  geom_histogram(data = storm_2k_obs_na_proc, 
                 aes(x = wind,
                     y = after_stat(count / sum(count))),
                 fill = 'cyan',
                 alpha = 0.5) +
  theme_minimal()
 


## Oh, have been keeping this too much in-console, but here are some plots by region

# was originally doing states with "^\\d{2}", but realized I could merge in coastlines
  ## !!!: Good point--should count years with no >=Cat 1 storms as a '0', not an NA
      # how to approach that?
      # doing a pretty basic approach (coloring based on indicator var))
        # so need to add a calculation before the plotting step


test_bayesian_rowwise_95_cis_working %>% 
  unnest(impact_summary) %>%
  ungroup() %>% 
  unnest(data) %>% 
  #mutate(state = str_extract(gridid, "^\\d{2}")) %>%
  left_join(coastlines, by = c('gridid' = 'state_county_fips')) %>% 
  ggplot() +
  aes(x = coastline_region, y = mean,
      color = as.factor(vmax_sust >= 32.9)) + 
  geom_violin(draw_quantiles = c(0.5)) + 
  theme_minimal()

# NCEI regions version

test_bayesian_rowwise_95_cis_working %>% 
  unnest(impact_summary) %>%
  ungroup() %>% 
  unnest(data) %>% 
  left_join(us_counties_wgs84, by = c('gridid' = 'GEOID')) %>% 
  ggplot() +
  aes(x = region, y = mean,
      color = as.factor(vmax_sust >= 32.9)) + 
  geom_violin(draw_quantiles = c(0.5)) + 
  theme_minimal()

# map

test_bayesian_rowwise_95_cis_working %>% 
  unnest(impact_summary) %>%
  ungroup() %>% 
  unnest(data) %>% 
  left_join(us_counties_wgs84, by = c('gridid' = 'GEOID')) %>%
  ggplot() + 
  aes(geometry = geometry,
      fill = age_65_plus) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c(option = 'turbo', trans = 'log')

# adding region boundaries

ggplot() +
  geom_sf(aes(geometry = geometry,
              fill = age_65_plus),
          data = 
            (test_bayesian_rowwise_95_cis_working %>% 
               unnest(impact_summary) %>%
               ungroup() %>% 
               unnest(data) %>% 
               left_join(us_counties_wgs84, by = c('gridid' = 'GEOID')))
            ) +
  theme_void() +
  scale_fill_viridis_c(option = 'turbo', trans = 'log')


# numeric <-- still need to make better use of confidence intervals

test_bayesian_rowwise_95_cis_working %>% 
  unnest(impact_summary) %>%
  ungroup() %>% 
  unnest(data) %>% 
  left_join(us_counties_wgs84, by = c('gridid' = 'GEOID')) %>%
  mutate(hurricane = vmax_sust >= 32.9) %>%
  group_by(region, hurricane) %>%
  mutate(mean_attr_deaths = mean(mean),
         total_attr_deaths = sum(mean)/1000) %>%
  dplyr::select(region, hurricane, mean_attr_deaths,
                total_attr_deaths) %>%
  unique() %>% 
  arrange(region)
    # Why does Ohio vVlley have such weird numbers? (43 mean deaths/hurricane, but <1 net attr. death)
      # doesn't seem to match the violin plots
        # mean should be 50ish, but don't see any <0 values


###########3


  #why are there `NA` regions showing up here?
    # OK, FIPS is 11001 -- Washington DC, so not technically a state
    # seems like you'd want to fold it into the SE?


# NCEI regions version

# states -- need some way to organize other than alphabetical 
  # maybe shoudl make more explicit, but `us_counties_ws84` has the NCEI regions\

test_bayesian_rowwise_95_cis_working %>% 
  unnest(impact_summary) %>%
  ungroup() %>% 
  unnest(data) %>% 
  left_join(us_counties_wgs84, by = c('gridid' = 'GEOID')) %>% 
  ggplot() +
  aes(x = state, y = mean,
      color = as.factor(vmax_sust >= 32.9)) + 
  geom_violin(draw_quantiles = c(0.5)) + 
  theme_minimal()


# Oh, still need some way to associate written states w/ the FIPS codes
  # OK, us_counties/etc. should still have it

# trying for composite 'person-exposure' metric

# first pass, at least; double-check math, but should be same general operations

sim_person_exposures <- test_bayesian_rowwise_95_cis_working %>% 
  unnest(data) %>% 
  unnest(impact_summary) %>% 
  mutate(hurricane = as.factor(vmax_sust >= 32.9)) %>% 
  ungroup() %>% 
  dplyr::select(gridid, vmax_sust,
                hurricane, age_65_plus, mean) %>% 
  group_by(gridid, hurricane) %>% 
  mutate(annual_deaths = sum(mean)/1000, exposures = n()) %>% 
  dplyr::select(gridid, hurricane,
                age_65_plus, annual_deaths, exposures) %>% 
  unique() %>% 
  mutate(person_exposures_yr = age_65_plus * exposures/1000) 

sim_person_exposures %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot()+
  aes(geometry = geometry, fill = person_exposures_yr) + geom_sf() +
  scale_fill_viridis_c(option = 'turbo') +
  facet_wrap(. ~ hurricane) +
  theme_void()

sim_person_exposures %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot()+
  aes(geometry = geometry, fill = age_65_plus) + geom_sf() +
  scale_fill_viridis_c(option = 'turbo', trans = 'log')


sim_person_exposures %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot()+
  aes(geometry = geometry, fill = person_exposures_yr) +
  geom_sf() + 
  scale_fill_viridis_c() +
  facet_wrap(. ~ hurricane)


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


# Oh, jsut realized I haven't been "saving" the code on exploratory plots, etc.;

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

# Oh, let's see if I can ~automatically generate Saphir-simpson categories:

#Mean annual category 1 deaths
test_bayesian_rowwise_hist95_cis %>%
  unnest(data) %>%
  mutate(saphir_simpson = as.numeric(cut(vmax_sust, c(0,74,96,111,130,157,200)*0.44704,
                              right = FALSE, label = FALSE))-1) %>%
  unnest(impact_summary) %>% 
  group_by(gridid, saphir_simpson) %>%
  mutate(deaths_yr = sum(mean)/30) %>%
  dplyr::select(gridid, saphir_simpson, deaths_yr) %>%
  unique() %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID"))  %>% 
  ggplot() + aes(geometry = geometry, fill = deaths_yr) +
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void() +
  facet_wrap(. ~ saphir_simpson)

test_bayesian_rowwise_95_cis_working %>%
  unnest(data) %>%
  mutate(saphir_simpson = as.numeric(cut(vmax_sust, c(0,74,96,111,130,157,200)*0.44704,
                                         right = FALSE, label = FALSE))-1) %>%
  unnest(impact_summary) %>% 
  group_by(gridid, saphir_simpson) %>%
  mutate(deaths_yr = sum(mean)/1000) %>%
  dplyr::select(gridid, saphir_simpson, deaths_yr) %>%
  unique() %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID"))  %>% 
  ggplot() + aes(geometry = geometry, fill = deaths_yr) +
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void() +
  facet_wrap(. ~ saphir_simpson)

  # could also not divide by years, but then not compatible with historical vs simualted data

# Seeing if I can rearrange to get the whole map as a backdrop
saphir_simpson_plot_data_sim <- test_bayesian_rowwise_95_cis_working %>%
  unnest(data) %>%
  mutate(saphir_simpson = as.numeric(cut(vmax_sust, c(0,74,96,111,130,157,200)*0.44704,
                                         right = FALSE, label = FALSE))-1) %>%
  unnest(impact_summary) %>% 
  group_by(gridid, saphir_simpson) %>%
  mutate(deaths_yr = sum(mean)/1000) %>%
  dplyr::select(gridid, saphir_simpson, deaths_yr) %>%
  unique() %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID"))

ggplot() + 
  geom_sf(aes(geometry = geometry), fill = 'grey', color = 'grey80', 
          data = (us_counties_wgs84 %>%
            filter(GEOID %in% 
                     unique(c(unique(test_bayesian_rowwise_95_cis_working$gridid)),
                              unique(test_bayesian_rowwise_hist95_cis$gridid)) )
            ))+
  geom_sf(
    aes(geometry = geometry, fill = deaths_yr),
    data = saphir_simpson_plot_data_sim,
    ) + 
  scale_fill_viridis_c() + 
  theme_void() +
  facet_wrap(. ~ saphir_simpson) + 
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', limits = c(-12, 12))
  
  
  # historical
  
  saphir_simpson_plot_data_hist <- test_bayesian_rowwise_hist95_cis %>%
    unnest(data) %>%
    mutate(saphir_simpson = as.numeric(cut(vmax_sust, c(0,74,96,111,130,157,200)*0.44704,
                                           right = FALSE, label = FALSE))-1) %>%
    unnest(impact_summary) %>% 
    group_by(gridid, saphir_simpson) %>%
    mutate(deaths_yr = sum(mean)/30) %>%
    dplyr::select(gridid, saphir_simpson, deaths_yr) %>%
    unique() %>% 
    left_join(us_counties_wgs84, by = c("gridid" = "GEOID"))
  
  ggplot() + 
    geom_sf(aes(geometry = geometry), fill = 'grey', color = 'grey80', 
            data = (us_counties_wgs84 %>%
                      filter(GEOID %in% 
                               unique(c(unique(test_bayesian_rowwise_95_cis_working$gridid)),
                                      unique(test_bayesian_rowwise_hist95_cis$gridid)) )
            ))+
    geom_sf(
      aes(geometry = geometry, fill = deaths_yr),
      data = saphir_simpson_plot_data_hist,
    ) + 
    scale_fill_viridis_c() + 
    theme_void() +
    facet_wrap(. ~ saphir_simpson) + 
    scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red')
  

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
               st_cast(., to = "LINESTRING")))
             
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



#   #   #   #   #   #   #   #   #   3   #   #   #

# trying to compare tracks between historical and 2k data



  
ggplot() +
  geom_sf(aes(geometry = geometry),
          fill = NA,
          color = 'black', 
          data = tc_exp_impact_counties) + 
  geom_sf(aes(
    geometry = geometry,
    color = wind,
    group = storm_id),
    data = storm_2k_obs_na_proc[1:8000,] %>% 
      mutate(longitude = longitude) %>%
      st_as_sf(coords = c("longitude",
                          "latitude"),
               crs = st_crs(us_counties_wgs84)) ) +
  geom_sf() +
  theme_void() + 
  scale_color_viridis_c()


ggplot() +
  geom_sf(aes(geometry = geometry),
          fill = NA,
          color = 'black', 
          data = tc_exp_impact_counties) + 
  geom_sf(aes(
    geometry = geometry,
    color = wind,
    group = storm_id),
    data = storm_hist_proc_hurricane %>% 
      mutate(longitude = longitude) %>%
      st_as_sf(coords = c("longitude",
                          "latitude"),
               crs = st_crs(us_counties_wgs84)) ) +
  geom_sf() +
  theme_void() + 
  scale_color_viridis_c()


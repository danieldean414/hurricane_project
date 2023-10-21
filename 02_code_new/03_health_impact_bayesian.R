
# trying to work out which of these are ~functional/final steps

# Trying to get 95% CI assuming a normal distirubiton

get_95_ci <- function(data_in){
  s_d = sd(data_in, na.rm = TRUE)
  error = qnorm(0.975)*s_d/sqrt(length(data_in))
  mean = mean(data_in)
  
  sd_obj = list(
    mean = mean,
    lower = mean - error,
    upper = mean + error
  )
  return(sd_obj)
}


#################################################################33
  # !: Want to make sure it follows same filtering criteria

# Using historical data

load(file = "01_data/storm_hist_proc_hurricane_ggw.rda")
  # remember, this is the one filtered to storms reaching at least Cat 1

# trying to track down some NA's
    # OK, already there are 13918 NA's for date/time/etc. at this point
    # Are there in the raw historical data?
test_bayesian_data_hist <- (storm_hist_proc_hurricane_ggw %>%  
                         #rownames_to_column("storm_id") %>%
                          #mutate(storm_id = str_remove_all(storm_id, pattern = "\\.\\d*")) %>%
                           filter(!is.na(date_time_max_wind))  %>%
                           left_join(county_acs_vars_bayesian, 
                                     by = c("gridid" = "GEOID")) %>%
                           mutate(exposure = replace_na(exposure, 1)) %>%
                           filter(!is.na(median_house_value)))

# why are there NAs in the historic data??
  # OK, I think it checks against every county each time,
    # and NAs are when there isn't a detectable exposure


#Sys.time()
#test_bayesian_hist <- predict(modobj, newdata = (test_bayesian_data_hist) )
# OK, no structure as-is (just a long 1-by-x vector); could either use something like `rbind`, or apply rowwise
# ohh, still NAs for things like median income, etc.; I remember this is a concern
#Sys.time()

##########

# replacing w/ some of the locally-processed STORM data:
  # huh for some reason appears to have retained the  

load(file = "01_data/storm_10k_all_obs_na_proc_hurricane_ggw.rda")

test_bayesian_data_in <- storm_10k_all_obs_na_proc_hurricane_ggw %>%
  #rownames_to_column("storm_id") %>%
  #mutate(storm_id = str_extract(storm_id, pattern = "\\d\\-\\d{4}\\-\\d")) %>%
  filter(!is.na(date_time_max_wind))  %>%
  left_join(county_acs_vars_bayesian, 
            by = c("gridid" = "GEOID")) %>%
  mutate(exposure = replace_na(exposure, 1)) %>%
  filter(!is.na(median_house_value))
      # the removed counties are Kenedy and Loving again (I guess rejoined w/ geography?)
          # only median house value appears to be missing, though; wonder if e.g. interpolating would be a good alternative?
        # plus Bedford City County, VA, which was folded into a larger county in 2015


#Sys.time()
#test_bayesian <- predict(modobj, newdata = (test_bayesian_data_in) )
# OK, no structure as-is (just a long 1-by-x vector); could either use something like `rbind`, or apply rowwise
# ohh, still NAs for things like median income, etc.; I remember this is a concern
#Sys.time()

# Well, on the bright side, runs pretty much instantaneously with 100 rows;
  # output is 47000 numbers; I guess 470 per row? seems like an odd 'default' number
  

test_bayesian_single <- predict(modobj, newdata = (test_bayesian_data_in[13,]))
  # 1000 ~rows (trying to remember how to handle--do you do summary statistics?)
    # so rowwise application might make sense

# OK, this could work:



test_bayesian_rowwise <- test_bayesian_data_in %>%
  filter(vmax_sust >= 17.4) %>% # too extreme?
  group_by(gridid, storm_id) %>%
  nest() %>%
  mutate(impact = purrr::map(.x = data, .f = ~predict(modobj, newdata = .x)) )

test_bayesian_rowwise_hist <- test_bayesian_data_hist %>%
  filter(vmax_sust >= 17.4) %>%
  group_by(gridid, storm_id) %>%
  nest() %>%
  mutate(impact = purrr::map(.x = data, .f = ~predict(modobj, newdata = .x)) )#,
         #impact_summary = purrr::map(.x = impact, .f = ~summary(unlist(.x))))

# OK, a little risky, but I *think* Na/zero-row results are just where the storm doesn't hit that county
  # Would it follow that each storm will have all ~3000ish counties, then?

  # no--everywhere from 1-2 to hundreds of counties:
test_bayesian_rowwise %>%
  ungroup() %>%
  group_by(storm_id) %>%
  tally() %>%
  summary()

## Huh, so then what *do* NA's mean here?
  # maybe something w/ filtering to min. windspeeds on the basis of exposures?

#test_bayesian_rowwise_summarized_hist <- test_bayesian_data_hist %>%
#  filter(vmax_sust >= 17.4) %>%
#  group_by(gridid, storm_id) %>%
#  nest() %>%
#  mutate(impact = purrr::map(.x = data, .f = ~predict(modobj, newdata = .x)),
#         impact_lower = purrr::map(.x = impact, .f = ~quantile(.x, probs = c(0.025))),
#         impact_median = purrr::map(.x = impact, .f = ~quantile(.x, probs = c(0.5))),
#         impact_upper = purrr::map(.x = impact, .f = ~quantile(.x, probs = c(0.975))))


#test_bayesian_rowwise_summarized <- test_bayesian_data_in %>%
#  filter(vmax_sust >= 17.4) %>%
# group_by(gridid, storm_id) %>%
#  nest() %>%
#  mutate(impact = purrr::map(.x = data, .f = ~predict(modobj, newdata = .x)),
#         impact_lower = purrr::map(.x = impact, .f = ~quantile(.x, probs = c(0.025))),
#         impact_median = purrr::map(.x = impact, .f = ~quantile(.x, probs = c(0.5))),
#         impact_upper = purrr::map(.x = impact, .f = ~quantile(.x, probs = c(0.975))))


test_bayesian_rowwise_95_cis <- test_bayesian_rowwise %>%
  #unnest(data)
  mutate(impact_summary = purrr::map(.x = impact, .f = ~as.data.frame(get_95_ci((.x)))))



test_bayesian_rowwise_hist95_cis <- test_bayesian_rowwise_hist %>%
  #unnest(data)
  mutate(impact_summary = purrr::map(.x = impact, .f = ~as.data.frame(get_95_ci((.x)))))

save(test_bayesian_rowwise_95_cis, file = "01_data/test_bayesian_rowwise_95_cis.rda")
  # why did this crash the first time? (See notes for exact storm/gridid)
save(test_bayesian_rowwise_hist95_cis, file = "01_data/test_bayesian_rowwise_hist95_cis.rda")
####### Quick plot


test_bayesian_rowwise_95_cis %>% 
  #mutate(deaths_base = purrr::map(.x = data,
  #                                .f = ~(((.x$deaths / .x$population) * .x$age_65_plus))) ) %>%
  #mutate(deaths_attr = purrr::map2(.x = deaths, .y = impact_summary,
  #                                 .f = ~((as.numeric(.x)/100000) * as.numeric(.y)))) %>% 
  mutate(deaths_attr = purrr::map2(.x = data, .y = impact_summary,
                                   .f = ~(((.x$age_65_plus)/100000) * (.y)))) %>% 
  unnest(deaths_attr) %>%
  #mutate(deaths_attr_95_ci = rep(c("deaths_central", "deaths_lower", "deaths_upper"))) %>%
  #pivot_wider(values_from = deaths_attr, names_from = deaths_attr_95_ci) %>%
  group_by(gridid) %>%
  dplyr::summarize(across(where(is.numeric), sum)) %>%
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = mean / 10000) + # for the 1000 years in this set
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void() +
  labs(title = "Estimated annual TC-atributable deaths (10000 synthetic years)")

# might be more useful as a rate?

## ?? Is "mean" a misnomer here? 

test_bayesian_rowwise_95_cis %>% 
  #mutate(deaths_base = purrr::map(.x = data,
  #                                .f = ~(((.x$deaths / .x$population) * .x$age_65_plus))) ) %>%
  #mutate(deaths_attr = purrr::map2(.x = deaths, .y = impact_summary,
  #                                 .f = ~((as.numeric(.x)/100000) * as.numeric(.y)))) %>% 
  #mutate(deaths_attr = purrr::map2(.x = data, .y = impact_summary,
  #                                 .f = ~(((.x$age_65_plus)/100000) * (.y)))) %>% 
  #unnest(deaths_attr) %>%
  #mutate(deaths_attr_95_ci = rep(c("deaths_central", "deaths_lower", "deaths_upper"))) %>%
  #pivot_wider(values_from = deaths_attr, names_from = deaths_attr_95_ci) %>%
  unnest(impact_summary) %>%
  group_by(gridid) %>%
  dplyr::summarize(across(where(is.numeric), median)) %>%
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = mean) + # for the 1000 years in this set
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void() +
  labs(title = "Estimated Mean TC-atributable Deaths per 100k (10000 synthetic years)")

#######################################################3

############# Setting up versions with only >30ms *winds* (not just storms)
#########################################3

  # Oh, this is could be a good illustration of the 'rare but hihg-impact' subset
    # historical record is pretty spotty, while sim. is much more of coast

test_bayesian_data_in_cat_1_p <- storm_10k_obs_na_proc_hurricane_ggw %>%
  filter(vmax_sust >= 32.9) %>% # maybe should filter in the next function upstream?
  rownames_to_column("storm_id") %>%
  mutate(storm_id = str_extract(storm_id, pattern = "\\d\\-\\d{4}\\-\\d")) %>%
  filter(!is.na(date_time_max_wind))  %>%
  left_join(county_acs_vars_bayesian, 
            by = c("gridid" = "GEOID")) %>%
  mutate(exposure = replace_na(exposure, 1)) %>%
  filter(!is.na(median_house_value))
  # only 7178 rows; 608 storms; is that low?

test_bayesian_data_hist_cat_1_p <- (storm_hist_proc_hurricane_ggw %>%  
                                      filter(vmax_sust >= 32.9) %>% # maybe should filter in the next function upstream?
                                      rownames_to_column("storm_id") %>%
                                      mutate(storm_id = str_remove_all(storm_id, pattern = "\\.\\d*")) %>%
                                      filter(!is.na(date_time_max_wind))  %>%
                                      left_join(county_acs_vars_bayesian, 
                                                by = c("gridid" = "GEOID")) %>%
                                      mutate(exposure = replace_na(exposure, 1)) %>%
                                      filter(!is.na(median_house_value)))




test_bayesian_rowwise_c1_p <- test_bayesian_data_in_cat_1_p %>%
  filter(vmax_sust >= 17.4) %>%
  group_by(gridid, storm_id) %>%
  nest() %>%
  mutate(impact = purrr::map(.x = data, .f = ~predict(modobj, newdata = .x)) )

test_bayesian_rowwise_hist_c1_p <- test_bayesian_data_hist_cat_1_p %>%
  filter(vmax_sust >= 17.4) %>%
  group_by(gridid, storm_id) %>%
  nest() %>%
  mutate(impact = purrr::map(.x = data, .f = ~predict(modobj, newdata = .x)) )

# 


test_bayesian_rowwise_c1_p_95_cis <- test_bayesian_rowwise_c1_p %>%
  #unnest(data)
  mutate(impact_summary = purrr::map(.x = impact, .f = ~as.data.frame(get_95_ci((.x)))))



test_bayesian_rowwise_hist_c1_p_95_cis <- test_bayesian_rowwise_hist_c1_p %>%
  #unnest(data)
  mutate(impact_summary = purrr::map(.x = impact, .f = ~as.data.frame(get_95_ci((.x)))))




# yeah, still 'spotty' geo. coverage even w/ pre filtering; not sure what I'd be expecting 


############################


############################################################3
#  trying to work out why there are NAs
ggplot() +
  geom_sf(aes(fill = impact_median,
              geometry = geometry),
          data = test_bayesian_rowwise_summarized %>%
            unnest(impact_median) %>% 
            filter(storm_id == "0-0002-0") %>%
            left_join(us_counties_wgs84, by = c("gridid" = "GEOID"))) +
  geom_sf(aes(  geometry = geometry, color = wind), 
          data = (storm_10k_obs_na_proc_split$`0-0002-0` %>%
                    mutate(longitude = longitude - 360) %>%
                    st_as_sf(coords = c('longitude', 'latitude'),
                             crs = "WGS84"))) +
  scale_color_viridis_c(option = 'plasma') +
  scale_fill_viridis_c(option = "mako") + 
  theme_void()

# exploratory plot on why there are NA's in the Bayesian output:
ggplot() +
  geom_sf(aes(fill = vmax_sust,
              alpha = vmax_sust >= 17.4,
              geometry = geometry),
          data = test_bayesian_data_in %>%
            filter(storm_id == "0-0001-0") %>%
            left_join(us_counties_wgs84, by = c("gridid" = "GEOID"))) +
  geom_sf(aes(  geometry = geometry, color = wind), 
          data = (storm_10k_obs_na_proc %>% filter(storm_id == '0-0001-0') %>%
                    mutate(longitude = longitude - 360) %>%
                    st_as_sf(coords = c('longitude', 'latitude'),
                             crs = "WGS84"))) +
  scale_color_viridis_c(option = 'plasma', na.value = 'magenta') +
  scale_fill_viridis_c(option = "mako") + 
  theme_void()  +
  geom_sf(aes(color = impact_median,
              geometry = geometry), fill = NA,
          data = test_bayesian_rowwise_summarized %>%
            unnest(impact_median) %>% 
            filter(storm_id == "0-0001-0") %>%
            left_join(us_counties_wgs84, by = c("gridid" = "GEOID")))

  # not too enlightening; not sure why those ones are NA's vs ~0 like the other peripheral counties

storm_ids_na <- test_bayesian_rowwise_summarized %>%
  dplyr::select(-impact, -data) %>% 
  unnest(impact_lower:impact_upper) %>%
  filter(is.na(impact_median)) %>%
  dplyr::select(storm_id)

# 545  unique storms with"NA" values:
storm_ids_na %>% ungroup() %>% dplyr::select(storm_id) %>% unique() %>% dim()
# for some reason apparently only 471 matching?? 
storm_10k_obs_na_proc %>% filter(storm_id %in% storm_ids_na$storm_id) %>%
  dplyr::select(storm_id) %>% unique() %>% dim()

###############################################################

  # huh, no storm_id in my version
  # oh, adding a summary step seems to make the runtime a lot longer...

test_df <- test_bayesian_rowwise_summarized %>%
  unnest(c(impact_lower, impact_median, impact_upper)) %>%
  ungroup() %>%
  group_by(gridid) %>% 
  dplyr::summarize(impact_median_median = median(impact_median, na.rm = T),
                   impact_lower_median = median(impact_lower, na.rm = T), 
                   impact_upper_median = median(impact_upper, na.rm = T))


test_df %>% 
  filter(!is.na(impact_median_median)) %>%
  left_join(county_acs_vars_bayesian, by = c("gridid" = "GEOID")) %>%
  ggplot() +
  aes(geometry =geometry)


  # Oh, just realized this doesn't have the storm categories here; I guess treat as one unit?

test_bayesian_rowwise %>%
  ungroup() %>%
  dplyr::select(fips, storm_id, impact, - storm_id) %>%
  group_by(fips) %>%
  mutate(impact = sum(impact))
  
  
mutate(impact_summary = purrr::map(.x = impact, .f = ~get_95_ci(data_in = unlist(.x))))

## plots
  # OK, revisiting it, I *think* excess events is per population, not per deaths

test_bayesian_rowwise_c1_p_95_cis %>% 
  #mutate(deaths_base = purrr::map(.x = data,
  #                                .f = ~(((.x$deaths / .x$population) * .x$age_65_plus))) ) %>%
  #mutate(deaths_attr = purrr::map2(.x = deaths, .y = impact_summary,
  #                                 .f = ~((as.numeric(.x)/100000) * as.numeric(.y)))) %>% 
  mutate(deaths_attr = purrr::map2(.x = data, .y = impact_summary,
                                   .f = ~(((.x$age_65_plus)/100000) * (.y)))) %>% 
  unnest(deaths_attr) %>%
  #mutate(deaths_attr_95_ci = rep(c("deaths_central", "deaths_lower", "deaths_upper"))) %>%
  #pivot_wider(values_from = deaths_attr, names_from = deaths_attr_95_ci) %>%
  group_by(gridid) %>%
  dplyr::summarize(across(where(is.numeric), sum)) %>%
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = mean / 1000) + # for the 1000 years in this set
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void() +
  labs(title = "Estimated annual TC-atributable deaths (1000 synthetic years); Category 1 and Higher")

# Weird; just grabbing means only takes a few seconds, but something like `summary()` runs indefinitely
  
## Esitamtes are seeming suspiciously low for historical storms;
  # could see Katrina being an outlier (post-storm management compounded), but seems low in general

# Michael (2018): ~59 US deaths (per Wikipedia) vs ~6.7 (>= 32.9m/s) or -14 (full dataset)
 # etc.


# Yeah, at least trying to replicate methods papers' figures,
  # looking a lot more like it's excess events per 100k *residents*, not deaths
  # Katrina estimate of ~470, vs ~16 by deaths; figure has center somewhere around 500


test_bayesian_rowwise_hist_c1_p_95_cis %>% 
 # mutate(deaths_base = purrr::map(.x = data,
  #                                .f = ~(((.x$deaths / .x$population) * .x$age_65_plus))) ) %>%
  mutate(deaths_attr = purrr::map2(.x = data, .y = impact_summary,
                                   .f = ~((as.numeric(.x$age_65_plus)/100000) * as.numeric(.y)))) %>% 
  unnest(deaths_attr) %>%
  mutate(deaths_attr_95_ci = rep(c("deaths_central", "deaths_lower", "deaths_upper"))) %>%
  pivot_wider(values_from = deaths_attr, names_from = deaths_attr_95_ci) %>% unnest(data) %>% 
  filter(str_detect(storm_id, "Katrina")) %>%
  group_by(gridid) %>%
  dplyr::summarize(across(where(is.numeric), sum)) %>% 
  dplyr::select(deaths_central) %>% 
  sum()


test_bayesian_rowwise_95_cis <- test_bayesian_rowwise %>%
  #unnest(data)
  mutate(impact_summary = purrr::map(.x = impact, .f = ~get_95_ci(unlist(.x))))
  
test_bayesian_rowwise_hist_95_cis <- test_bayesian_rowwise_hist %>%
  #unnest(data)
  mutate(impact_summary = purrr::map(.x = impact, .f = ~get_95_ci(unlist(.x))))

## Getting in a more useful format
test_bayesian_rowwise_hist_95_cis %>% unnest(impact_summary) %>% 
  unnest(impact_summary) %>%
  mutate(intervals = rep(c("central", "upper", "lower") )) %>%
  pivot_wider(values_from = impact_summary, names_from = intervals)

test_bayesian_rowwise_95_cis %>% unnest(impact_summary) %>% 
  unnest(impact_summary) %>%
  mutate(intervals = rep(c("central", "upper", "lower") )) %>%
  pivot_wider(values_from = impact_summary, names_from = intervals)

# estimating attributable deaths (fixed populations/rates)

test_bayesian_rowwise_95_cis %>% 
  mutate(deaths_base = purrr::map(.x = data,
                                  .f = ~(((.x$deaths / .x$population) * .x$age_65_plus))) ) %>%
  mutate(deaths_attr = purrr::map2(.x = deaths_base, .y = impact_summary,
                                   .f = ~((as.numeric(.x)/100000) * as.numeric(.y)))) %>% 
  unnest(deaths_attr) %>%
  mutate(deaths_attr_95_ci = rep(c("deaths_central", "deahts_upper", "deaths_lower"))) %>%
  pivot_wider(values_from = deaths_attr, names_from = deaths_attr_95_ci) 

# plot

test_bayesian_rowwise_95_cis %>% 
  mutate(deaths_base = purrr::map(.x = data,
                                  .f = ~(((.x$deaths / .x$population) * .x$age_65_plus))) ) %>%
  mutate(deaths_attr = purrr::map2(.x = deaths_base, .y = impact_summary,
                                   .f = ~((as.numeric(.x)/100000) * as.numeric(.y)))) %>% 
  unnest(deaths_attr) %>%
  mutate(deaths_attr_95_ci = rep(c("deaths_central", "deaths_upper", "deaths_lower"))) %>%
  pivot_wider(values_from = deaths_attr, names_from = deaths_attr_95_ci) %>%
  group_by(gridid) %>%
  dplyr::summarize(across(where(is.numeric), sum)) %>%
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = deaths_central / 1000) +
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void() +
  labs(title = "estimated annual TC-atributable deaths (1000 synthetic years)")


test_bayesian_rowwise_95_cis %>% 
  mutate(deaths_base = purrr::map(.x = data,
                                  .f = ~(((.x$deaths / .x$population) * .x$age_65_plus))) ) %>%
  mutate(deaths_attr = purrr::map2(.x = deaths_base, .y = impact_summary,
                                   .f = ~((as.numeric(.x)/100000) * as.numeric(.y)))) %>% 
  unnest(deaths_attr) %>%
  mutate(deaths_attr_95_ci = rep(c("deaths_central",
                                   "deaths_upper",
                                   "deaths_lower"))) %>%
  pivot_wider(values_from = deaths_attr, names_from = deaths_attr_95_ci) %>%
  group_by(gridid) %>%
  dplyr::summarize(across(where(is.numeric), sum)) %>%
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = deaths_central / 1000) +
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void() +
  labs(title = "estimated annual TC-atributable deaths (1000 synthetic years)")


test_bayesian_rowwise_hist_95_cis %>% 
  mutate(deaths_base = purrr::map(.x = data,
                                  .f = ~(((.x$deaths / .x$population) * .x$age_65_plus))) ) %>%
  mutate(deaths_attr = purrr::map2(.x = deaths_base, .y = impact_summary,
                                   .f = ~((as.numeric(.x)/100000) * as.numeric(.y)))) %>% 
  unnest(deaths_attr) %>%
  mutate(deaths_attr_95_ci = rep(c("deaths_central",
                                   "deaths_upper",
                                   "deaths_lower"))) %>%
  pivot_wider(values_from = deaths_attr, names_from = deaths_attr_95_ci) %>%
  group_by(gridid) %>%
  dplyr::summarize(across(where(is.numeric), sum)) %>%
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = deaths_central / 30) +
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void() +
  labs(title = "estimated annual TC-atributable deaths (1988-2018)")

  #3 trying a plot:

test_bayesian_rowwise_95_cis %>% 
  unnest(impact_summary) %>% 
  unnest(impact_summary) %>%
  mutate(intervals = rep(c("central", "upper", "lower") )) %>%
  pivot_wider(values_from = impact_summary, names_from = intervals) %>% 
  dplyr::select(-impact, - data) %>% 
  group_by(gridid) %>% 
  dplyr::summarise(., across(where(is.numeric), median)) %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = central) +
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void()

test_bayesian_rowwise_hist_95_cis %>% 
  unnest(impact_summary) %>% 
  unnest(impact_summary) %>%
  mutate(intervals = rep(c("central", "upper", "lower") )) %>%
  pivot_wider(values_from = impact_summary, names_from = intervals) %>% 
  dplyr::select(-impact, - data) %>% 
  group_by(gridid) %>% 
  dplyr::summarise(., across(where(is.numeric), median)) %>% 
  left_join(us_counties_wgs84, by = c("gridid" = "GEOID")) %>% 
  ggplot() + 
  aes(geometry = geometry, fill = central) +
  geom_sf() + 
  scale_fill_viridis_c() + 
  theme_void()


# Population map



# huh--still seeing more negatives than not; I guess double check inclusion criteria?
  # oh, I'm filtering per-storm instead of per-timestep, so it's storms that reach at least that 32.9 m/s
    # feels like that makes sense, though; otherwise would only be including snippets

# Trying to write a 95% CI script
# https://www.cyclismo.org/tutorial/R/confidence.html#:~:text=We%20use%20a%2095%25%20confidence%20level%20and%20wish,%2B%20error%20%3E%20left%204.063971%20%3E%20right%205.936029 from 

#sample mean is 5, the standard deviation is 2, and the sample size is 20 

a <- 5
 s <- 2
 n <- 20
 error <- qnorm(0.975)*s/sqrt(n)
 left <- a-error
 right <- a+error
 left
#[1] 4.123477
 right
#[1] 5.876523

####
 
 # comparing populations:
 
 test_bayesian_rowwise_95_cis %>% 
   unnest(impact_summary) %>% 
   unnest(impact_summary) %>%
   mutate(intervals = rep(c("central", "upper", "lower") )) %>%
   pivot_wider(values_from = impact_summary, names_from = intervals) %>% 
   dplyr::select(-impact,) %>%
   group_by(gridid) %>% 
   unnest(data) %>%
   dplyr::summarise(., across(where(is.numeric), median)) %>% 
   dplyr::select(age_65_plus, population) %>%
   summary()
 
 
 test_bayesian_rowwise_95_cis %>% 
   unnest(impact_summary) %>% 
   unnest(impact_summary) %>%
   mutate(intervals = rep(c("central", "upper", "lower") )) %>%
   pivot_wider(values_from = impact_summary, names_from = intervals) %>% 
   dplyr::select(-impact,) %>%
   group_by(gridid) %>% 
   unnest(data) %>%
   dplyr::summarise(., across(where(is.numeric), median)) %>% 
   mutate(mort_rate = deaths / population,
          deaths = mort_rate * age_65_plus,
          deaths_attr = (deaths / 100000) * central) # any more efficient way to get  all 3 estimates? <- maybe unnest before pivot_wider?
 
 
 

############################################################################################################
  # OK, think everything below this can be skipped, but holding off on deleting for now...
############################################################################################################

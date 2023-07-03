# Have various exploratory plots up, but aiming to set up formal 'figures'

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
  filter(GEOID %in% storm_counties) %>%
  left_join(counties_mort_65, by = c("GEOID" = "county_code")) %>%
  ggplot() +
  aes(
    geometry = geometry,
    fill = age_65_plus
  ) +
  geom_sf() +
  theme_void()

county_acs_vars_bayesian %>%
  left_join(us_counties_wgs84) %>%
  filter(GEOID %in% storm_counties) %>%
  left_join(counties_mort_65, by = c("GEOID" = "county_code")) %>%
  ggplot() +
  aes(
    geometry = geometry,
    fill = deaths / population #age_65_plus
  ) +
  geom_sf() +
  theme_void()

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
    # appears to be off by some 


# making an equation, and later function, from Nethery et al data:

####### Libraries

library(tidyverse)
library(tigris)
  # I think this covers `sf`, etc. <- still should note specific packages
library(ggplot2)
library(janitor)

library(hurricaneexposuredata)
  # Loading data for storm events and properties

####################### Data

wind_mort_data <- read_csv("exposure_response_death.csv")
  ## Note: this only deals w/ events >= 17 

counties_mort_65 <- read_tsv("all_cause_mortality_65_plus_county.txt")
  # From CDC Wonder with:
    # Age set to 65 and above
    # years 2018, 2019 (? <-- verify; last 2 available)
      # suspect a wider range of years might be needed to counteract censoring
    # No subsetting by age/sex/ethnicity/etc.
    # all mortality
    # * At this stage not separating by county pop. -- should consider downstream

us_counties <- tigris::counties(cb = TRUE)


########################## Setting up function

  # Not sure how to account for CIs in writing a model
    # look up, but use central estimate for now....

  # Just predictive, so don't need to track ~precise relationship, I think...

library(splines)

wind_mort_fn_ns <- glm(excess_death_rate ~ ns(windspeed, 4), data = wind_mort_data)
  # Just using AIC; 4 is markedly better than 3 (~164k vs ~136k)
    # And increases again @ 5 knots -- 4 seems to be optimal
  # Adding CIs manually as separate models:

wind_mort_fn_ns_ci_low <- glm(lower_95ci ~ ns(windspeed, 4), data = wind_mort_data)
wind_mort_fn_ns_ci_high <- glm(upper_95ci ~ ns(windspeed, 4), data = wind_mort_data)

# Just had a thought--might be able to address CIs if I use est + u/l CI values?

wind_mort_data_long <- wind_mort_data %>%
  pivot_longer(cols = c(excess_death_rate,
                        lower_95ci,
                        upper_95ci),
               names_to = "est_type",
               values_to= "excess_death_rate")


wind_mort_fn_ns <- glm(excess_death_rate ~ ns(windspeed, 4),
                       data = wind_mort_data)

wind_mort_long_fn_ns <- glm(excess_death_rate ~ ns(windspeed, 4), 
                            data = wind_mort_data_long)


# Central estimate
wind_mort_fn_ns_pred <- data.frame(
  windspeed = wind_mort_data$windspeed,
  excess_death_est = predict(wind_mort_fn_ns, wind_mort_data)
  )

wind_mort_long_fn_ns_pred <- data.frame(
  windspeed = wind_mort_data$windspeed,
  excess_death_est = predict(wind_mort_long_fn_ns,
                             wind_mort_data)
)

# Not sure if/how this captures CIs

# Does lose some fidelity at the lower end (~17-25 m/s) 
  # unit is "excess rates per 100,000 of mortality"
    # So is that referring to # that die, or # population?
      # i.e. would "10" mean an extra 10 deaths per 100k in county, OR
      # an extra 10 deaths per 100,000 baseline deaths?
    # Reading further up, I'm leaning towards *population*

# dataset:

wind_mort_pred <- storm_winds %>%
  filter(vmax_sust > 17) %>%
  mutate(storm_year = as.numeric(
    str_extract(storm_id, "[0-9]{4}"))) %>%
  mutate(storm_years = max(storm_year) - min(storm_year)) %>% 
  left_join((counties_mort_65 %>%
              clean_names()),
            by = c('fips' = 'county_code')) %>%
  mutate(est_excess_death_rate = predict(wind_mort_long_fn_ns,
                                     newdata = data.frame( windspeed = vmax_sust)),
         est_excess_deaths_yr = est_excess_death_rate *
           population / 100000 / storm_years) %>%
  left_join(us_counties %>%
              mutate(fips = paste0(STATEFP, COUNTYFP)))


wind_mort_pred %>%
  group_by(fips) %>%
  nest() %>%
  mutate(est_excess_deaths_sum = 
           purrr::map(.x = data,
                      .f = ~sum(.x$est_excess_deaths_yr))) %>%
  unnest(cols = c(data, est_excess_deaths_sum)) %>%
  data.frame %>%
  ggplot() +
  aes(geometry = geometry,
      fill = est_excess_deaths_sum) +
  geom_sf() +
  scale_fill_viridis_c(trans = "log")

# Oh, just realized I'm using the default ~high-res county shapefiles
  


# OK, just realized an issue w/ ~continuous approach:
  # If I can't stratify and bin event count by exposure, need to change approach
    # Because currently (I think) have 1 entry per storm/county
    # For mapping, maybe can just take sum per county?
  # Looks like the excess death rate *only* takes population into account
    # Does that seem right?
    # The other reading (extra deaths per 100k mortalities) would be lower; seems semi-plausible?

# Might make more sense to (for now) use 3 separate functions (one for each CI)



# function format for nethery et al framework

# Let's leave out data ~importing for now; the function needs to:
  # match e.g. FIPS code to # storm events
  # convert to average events/yr <- maybe add paramater for timeframe?
  # get matching populations, etc.
  

wind_mort_long_fn_ns <- glm(excess_death_rate ~ ns(windspeed, 4), 
                            data = wind_mort_data_long)
  # Oh, this is a bad idea, I think -- trying to represent each estimate and interval as "samples"

# does it work with an external model?
  # Pulling in ones from above:

wind_mort_fn_ns <- glm(excess_death_rate ~ ns(windspeed, 4), data = wind_mort_data)
# Just using AIC; 4 is markedly better than 3 (~164k vs ~136k)
# And increases again @ 5 knots -- 4 seems to be optimal
# Adding CIs manually as separate models:

wind_mort_fn_ns_ci_low <- glm(lower_95ci ~ ns(windspeed, 4), data = wind_mort_data)
wind_mort_fn_ns_ci_high <- glm(upper_95ci ~ ns(windspeed, 4), data = wind_mort_data)


# `fips` here is a vector of FIPS (*as characters*)
county_mortality_pred <- function(fips, pred_model = wind_mort_long_fn_ns) {
  #wind_mort_long_fn_ns <- glm(excess_death_rate ~ ns(windspeed, 4), 
  #                            data = wind_mort_data_long)
  
  estimates <- counties_mort_65 %>%
    clean_names() %>%
    filter(county_code %in% fips) %>%
    left_join(
  storm_winds %>%
    filter(vmax_sust > 17) %>%
    mutate(storm_year = as.numeric(
      str_extract(storm_id, "[0-9]{4}"))) %>%
    mutate(storm_years = max(storm_year) - min(storm_year)),
  by = c("county_code" = "fips")
    ) %>%
    mutate(est_excess_death_rate = predict(pred_model,
                                           newdata = data.frame( windspeed = vmax_sust)),
           est_excess_deaths_yr = est_excess_death_rate *
             population / 100000 / storm_years)
  return(estimates)
    
}


county_mortality_pred(fips = "22057")

county_mortality_pred(fips = "22057", wind_mort_long_fn_ns)

  
# Let's get a sense of total variation?
  # It seems like simple addition should hold up...

# Center

county_mortality_pred(fips = counties_mort_65$`County Code`,
                      wind_mort_fn_ns) %>%
  select(est_excess_deaths_yr) %>%
  sum(na.rm = T)


# Upper:

county_mortality_pred(fips = counties_mort_65$`County Code`,
                      wind_mort_fn_ns_ci_high) %>%
  select(est_excess_deaths_yr) %>%
  sum(na.rm = T)

# Lower:


county_mortality_pred(fips = counties_mort_65$`County Code`,
                      wind_mort_fn_ns_ci_low) %>%
  select(est_excess_deaths_yr) %>%
  sum(na.rm = T)


### Trying one state:

county_mortality_pred(fips = (counties_mort_65 %>%
                                filter(str_detect(`County Code`,
                                                  "^09")))$`County Code`,
                      wind_mort_fn_ns_ci_high) %>%
  select(est_excess_deaths_yr) %>%
  sum(na.rm = T)


(counties_mort_65 %>% filter(str_detect(`County Code`,
                                        "^01")))$`County Code`

county_mortality_pred(fips = (counties_mort_65 %>%
                                filter(str_detect(`County Code`,
                                                  "^12")))$`County Code`,
                      wind_mort_fn_ns) %>%
  #select(est_excess_deaths_yr) %>%
  #sum(na.rm = T) %>%
  group_by(county_code) %>%
  nest() %>%
  mutate(sum_excess_deaths = purrr::map(.x = data,
                                        .f = ~sum(.$est_excess_deaths_yr))) %>%
  unnest(cols = c(data, sum_excess_deaths)) %>%
  mutate(sum_exces_deaths = sum(est_excess_deaths_yr, na.rm = T)) %>%
  mutate(fips = county_code) %>%
  left_join(us_counties %>%
                                 mutate(fips = paste0(STATEFP, COUNTYFP))) %>%
  ggplot() +
  aes(fill = sum_exces_deaths, geometry = geometry) %>%
  geom_sf()

county_mortality_pred(fips = (counties_mort_65 %>%
                                filter(str_detect(`County Code`,
                                                  "^09")))$`County Code`,
                      wind_mort_fn_ns) %>%
  select(est_excess_deaths_yr) %>%
  sum(na.rm = T)

## Adding coastal county list 

url <- "ftp://ftp.coast.noaa.gov/pub/MSP/ORT/CoastalCounties.zip"
destfile <- "coastal_fips_noa.zip"
download.file(url, destfile)

  # Argh, it's a set of shape files!

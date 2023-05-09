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

us_counties <- tigris::counties()


########################## Setting up function

  # Not sure how to account for CIs in writing a model
    # look up, but use central estimate for now....

  # Just predictive, so don't need to track ~precise relationship, I think...

library(splines)

wind_mort_fn_ns <- glm(excess_death_rate ~ ns(windspeed, 4), data = wind_mort_data)
  # Just using AIC; 4 is markedly better than 3 (~164k vs ~136k)
    # And increases again @ 5 knots -- 4 seems to be optimal


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



wind_mort_fn_ns_pred <- data.frame(
  windspeed = wind_mort_data$windspeed,
  excess_death_est = predict(wind_mort_fn_ns, wind_mort_data)
  )

wind_mort_long_fn_ns_pred <- data.frame(
  windspeed = wind_mort_data$windspeed,
  excess_death_est = predict(wind_mort_long_fn_ns,
                             wind_mort_data)
)


# Does lose some fidelity at the lower end (~17-25 m/s) 
  # unit is "excess rates per 100,000 of mortality"
    # So is that referring to # that die, or # population?
      # i.e. would "10" mean an extra 10 deaths per 100k in county, OR
      # an extra 10 deaths per 100,000 baseline deaths?
    # Reading further up, I'm leaning towards *population*

# dataset:

storm_winds %>%
  filter(vmax_sust > 17) %>%
  left_join(us_counties %>%
              mutate(fips = paste0(STATEFP, COUNTYFP))) %>%
  left_join((counties_mort_65 %>%
              clean_names()),
            by = c('fips' = 'county_code')) %>%
  mutate(est_excess_deaths = predict(wind_mort_long_fn_ns,
                                     newdata = data.frame( windspeed = vmax_sust)))



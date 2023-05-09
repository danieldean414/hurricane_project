# loading WONDER mortality data

library(tidyverse)

mortality <- read_tsv("../../Downloads/mortality_compressed_by_census_state_county.txt")
# so seems to consolidate death; need to download causes specifically?


multiple_cause_mortality <- read_tsv("../../Downloads/multiple_cause_of_death.txt")
#Argh--still aggregates; I guess need to download each cause separately?

accident_mortality <- read_tsv("../../Downloads/compressed_mortality_accident_maybe.txt")

#using (roughly) https://wonder.cdc.gov/controller/datarequest/D140

#Forgot to set separate names for circulatory (~cardovascular?) and respiratory mortality; using these for now
mortality_1 <- read_tsv("../../Downloads/Compressed Mortality, 1999-2016.txt")
mortality_2 <- read_tsv("../../Downloads/Compressed Mortality, 1999-2016 (1).txt")

circulatory_mortality <- read_tsv("../../Downloads/compressed_mortality_circulatory.txt")
respiratory_mortality <- read_tsv("../../Downloads/mortality_compressed_respiratory (1).txt")

  #Argh, just realized these don't seem to have dates attached-are these mortality over the whole ~17yr stretch?

#So, 
# all-cause (International Classification of Diseases, Ninth Revision (ICD-9), 001–999),
#accidental (ICD-9, 800–999),
#cardiovascular (ICD-9 codes (390–448),
#and respiratory (ICD-9 codes 480–486, 490–497, or 507).

#have all-cause, I suppose; need others
#390–448), and respiratory (ICD-9 codes 480–486, 490–497, or 507)


# ~2008 paper doesn't have much in the way of supplemental material, etc.

#Let's try the R package?

library(hurricaneexposure)
library(hurricaneexposuredata)
library(stormwindmodel)

hurricaneexposuredata::ext_tracks_wind

library(hurricaneexposuredata)


head(hurricaneexposuredata::storm_winds)

# Let's try populations?

county_pop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv") %>%
  clean_names()

#21 m/s ~ 21/2.23693629

hurricaneexposuredata::ext_tracks_wind %>%
  filter(vmax_sust >= 21/2.23693629) # 21 m/s


# Rvest
library("rvest")
library(janitor)

fips_to_county <- read_html("https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697") %>%
  html_nodes("table.data") %>%
  html_table() %>%
  data.frame() %>%
  clean_names()

# Populations?

ext_tracks_wind_with_county <- ext_tracks_wind %>%
  left_join(fips_to_county %>% 
              mutate(fips = as.character(fips),
                     fips = str_replace(fips,
                                        pattern = "^([0-9]{4})$",
                                        replacement = "0\\1")),
            by = "fips")


ext_tracks_wind_with_county %>%
  left_join((county_pop %>%
               select(ctyname, popestimate2019,
                      deaths2019, netmig2019)),
            by = c("name" = "ctyname"))



circulatory_mortality %>%
  clean_names() %>%
  left_join(ext_tracks_wind_with_county, by = c("county_code" = "fips",
                                                "county" = "name"))


# Example with circulatory deaths:
  #1.03 RR over the 10-day period

ext_tracks_wind_with_county %>%
  filter(vmax_sust >= 21/2.23693629) %>%
  group_by(name) %>%
  nest() %>%
  mutate(events = purrr::map(.x = data, .f = ~nrow(.x)),
         events_yr = purrr::map(.x = data, .f = ~nrow(.x) / 17)) %>% # 1988 - 2005 = 17yr
  unnest(cols = c(data, events, events_yr)) %>%
  left_join((circulatory_mortality %>%
               clean_names()),
            by = c("fips"= "county_code")) %>%
  select(name, state, fips, events, events_yr, icd_sub_chapter:crude_rate) %>%
  group_by(name) %>%
  nest() %>%
  mutate(total_deaths = purrr::map(.x = data, .f = ~sum(.x$deaths))) %>%
  unnest(cols = c(data, total_deaths)) %>%
  mutate(total_deaths_avg_day = (total_deaths / 17) / 365,
         death_risk_avg_day = total_deaths_avg_day / population) %>% # I'm assuming 17yr between 1999-2016
  mutate(event_days_yr = events_yr * 10,
         elev_death_risk = death_risk_avg_day * 1.03 * event_days_yr,
         elev_death = elev_death_risk * population) %>%
  ggplot() +
  aes(
    x = events,
    y = (elev_death)
  ) +
  geom_jitter()



# Respiratory Deaths:
  # RR = 1.05

ext_tracks_wind_with_county %>%
  #filter(vmax_sust >= 21/2.23693629) %>%
  group_by(name) %>%
  nest() %>%
  mutate(events = purrr::map(.x = data, .f = ~nrow(.x)),
         events_yr = purrr::map(.x = data, .f = ~nrow(.x) / 17)) %>% # 1988 - 2005 = 17yr
  unnest(cols = c(data, events, events_yr)) %>%
  left_join((respiratory_mortality %>%
               clean_names()),
            by = c("fips"= "county_code")) %>%
  select(name, state, fips, events, events_yr, county:crude_rate, population, vmax_sust) %>%
  filter(population >= 400000) %>%
  #group_by(name) %>%
  #nest() %>%
  #mutate(total_deaths = purrr::map(.x = data, .f = ~sum(.x$deaths))) %>%
  #unnest(cols = c(data, total_deaths)) %>%
  mutate(exp_group = case_when(
    vmax_sust >= 21/2.23693629 ~ "exp_21_ms",
    vmax_sust >= 18/2.23693629 ~ "exp_18_ms",
    vmax_sust >= 15/2.23693629 ~ "exp_15_ms",
    vmax_sust >= 12/2.23693629 ~ "exp_12_ms",
    vmax_sust < 12/2.23693629 ~ "exp_null"
        ),
    exp_rr = case_when(
      exp_group == 'exp_21_ms' ~ 1.05,
      exp_group == 'exp_18_ms' ~ 1.02,
      exp_group == 'exp_15_ms' ~ 0.99,
      exp_group == 'exp_12_ms' ~ 1.01,
      exp_group == 'exp_null' ~ 1.00,
    ),
         total_deaths_avg_day = (deaths / 17) / 365,
         death_risk_avg_day = total_deaths_avg_day / population) %>% # I'm assuming 17yr between 1999-2016
  mutate(event_days_yr = events_yr * 10,
         elev_death_risk = death_risk_avg_day * exp_rr * event_days_yr,
         elev_death = elev_death_risk * population) %>%
  ggplot() +
  aes(
    x = events,
    y = elev_death,
    color = exp_group
  ) +
  geom_point()

# Huh--the minimum already appears to exceed 21m/s (assuming I have the units right)
  # Is it pre-filtered somehow?

# All-cause mortality; RR = 1.07

ext_tracks_wind_with_county %>%
  filter(vmax_sust >= 21/2.23693629) %>%
  group_by(name) %>%
  nest() %>%
  mutate(events = purrr::map(.x = data, .f = ~nrow(.x)),
         events_yr = purrr::map(.x = data, .f = ~nrow(.x) / 17)) %>% # 1988 - 2005 = 17yr
  unnest(cols = c(data, events, events_yr)) %>%
  left_join((multiple_cause_mortality %>%
               clean_names()),
            by = c("fips"= "county_code")) %>%
  select(name, state, fips, events, events_yr, county:crude_rate) %>%
  #group_by(name) %>%
  #nest() %>%
  #mutate(total_deaths = purrr::map(.x = data, .f = ~sum(.x$deaths))) %>%
  #unnest(cols = c(data, total_deaths)) %>%
  mutate(total_deaths_avg_day = (deaths / 17) / 365,
         death_risk_avg_day = total_deaths_avg_day / population) %>% # I'm assuming 17yr between 1999-2016
  mutate(event_days_yr = events_yr * 10,
         elev_death_risk = death_risk_avg_day * 1.07 * event_days_yr,
         elev_death = elev_death_risk * population) %>%
  ggplot() +
  aes(
    x = events,
    y = elev_death,
    color = population
  ) +
  geom_point()

# Accident mortality; RR = 2.42


ext_tracks_wind_with_county %>%
  filter(vmax_sust >= 21/2.23693629) %>%
  group_by(name) %>%
  nest() %>%
  mutate(events = purrr::map(.x = data, .f = ~nrow(.x)),
         events_yr = purrr::map(.x = data, .f = ~nrow(.x) / 17)) %>% # 1988 - 2005 = 17yr
  unnest(cols = c(data, events, events_yr)) %>%
  left_join((accident_mortality %>%
               clean_names()),
            by = c("fips"= "county_code")) %>%
  select(name, state, fips, events, events_yr, icd_sub_chapter:crude_rate) %>%
  group_by(name) %>%
  nest() %>%
  mutate(total_deaths = purrr::map(.x = data, .f = ~sum(.x$deaths))) %>%
  unnest(cols = c(data, total_deaths)) %>%
  mutate(total_deaths_avg_day = (total_deaths / 17) / 365,
         death_risk_avg_day = total_deaths_avg_day / population) %>% # I'm assuming 17yr between 1999-2016
  mutate(event_days_yr = events_yr * 10,
         elev_death_risk = death_risk_avg_day * 2.42 * event_days_yr,
         elev_death = elev_death_risk * population) %>%
  ggplot() +
  aes(
    x = event_days_yr,
    y = (elev_death)
  ) +
  geom_point()


# Merged

mortality_merged <- multiple_cause_mortality %>%
  select(-`Crude Rate`) %>%
  left_join(
    respiratory_mortality %>%
      rename("Deaths_Respiratory" = Deaths) %>%
      select(County, Deaths_Respiratory) %>%
      unique) %>%
  left_join(
    circulatory_mortality %>%
      select(-`Crude Rate`) %>%
      group_by(County) %>%
      mutate(Deaths_Circulatory = purrr::map(
        .x = Deaths, .f = ~sum(.x) )) %>%
      unnest(Deaths_Circulatory) %>%
      select(County, Deaths_Circulatory) %>%
      unique()) %>%
  left_join(
    accident_mortality %>%
      select(-`Crude Rate`) %>%
      group_by(County) %>%
      mutate(Deaths_Accident = purrr::map(
        .x = Deaths, .f = ~sum(.x) )) %>%
      unnest(Deaths_Accident) %>%
      select(County, Deaths_Accident) %>%
      unique()) %>%
  clean_names() 


mortality_merged %>%
  select(county, county_code, population, deaths,
         deaths_circulatory, deaths_respiratory,
         deaths_accident) %>%
  unique()

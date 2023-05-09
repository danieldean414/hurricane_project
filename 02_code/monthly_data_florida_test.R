# mortalirt data monthly

mortality_all_fl_monthly_2018 <- read_tsv("../../Downloads/all_cause_mortality_2018_monthly_FL.txt")
#Oh, so monthly data doesn't have population data attached--I guess that's fair
mortality_all_fl_2018 <- read_tsv("../../Downloads/all_cause_mortality_2018_FL_GA.txt")
  #Note that this is a considerable assumption; `NA` for a reason in monthly data

mortality_all_fl_monthly_2018_with_yr_pop <- mortality_all_fl_monthly_2018 %>%
  select(-Population, -`Crude Rate`) %>%
  left_join((mortality_all_fl_2018 %>%
               select(County, Population)), by = "County") %>%
  clean_names()

mortality_all_fl_monthly_2018_with_yr_pop %>%
  filter(!is.na(month)) %>%
  #filter(population >= 400000) %>%
  ggplot() +
  aes(
    x = str_extract(month, pattern = "[A-Za-z]{3}"),
    y = deaths/population,
    group = county,
    color = population,
    alpha = population >= 400000
  ) +
  geom_line()

# peaks in winter (Jan)

# remember to use the other hurricane data

hurricaneexposuredata::storm_winds

storm_winds_with_county <- storm_winds %>%
  filter()
  left_join(fips_to_county %>% 
              mutate(fips = as.character(fips),
                     fips = str_replace(fips,
                                        pattern = "^([0-9]{4})$",
                                        replacement = "0\\1")),
            by = "fips")


##


##


storm_winds_with_county  %>%
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
  geom_point()
  

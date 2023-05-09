
# merging

test_bayesian_data_conus_split_ids <- test_bayesian_data_conus_split

for(storm_scenario in 1:length(test_bayesian_data_conus_split)){
  tmp = cbind(test_bayesian_data_conus_split[[storm_scenario]],
              "bayesian_est" = test_bayesian_conus_processed_lapply[[storm_scenario]]) 
  
  test_bayesian_data_conus_split_ids[[storm_scenario]] <- tmp
}

  # missing last ~4 storms; for some reason row count on one of last few doesn't align

test_bayesian_data_conus_split_ids_merge <- do.call("rbind", test_bayesian_data_conus_split_ids) %>%
  filter(!is.na(median_house_value) &  !is.na(population_density))


test_bayesian_data_conus_split_ids_merge_geom <- test_bayesian_data_conus_split_ids_merge %>%
  left_join(us_counties, by = c("gridid" = "GEOID")) 



test_bayesian_data_conus_split_ids_merge_summary <- test_bayesian_data_conus_split_ids_merge %>%
  group_by(gridid, scenario) %>%
  summarize(bayesian_est_mean = mean(bayesian_est, na.rm = T))






test_bayesian_data_conus_split_ids_merge_summary %>%
  left_join(us_counties, by = c("gridid" = "GEOID"))  %>%
  ggplot() +
  aes(
    geometry = geometry,
    fill = bayesian_est_mean
  ) +
  geom_sf()




##

load("01_data/test_bayesian_conus_prefiltered.rda")

test_bayesian_data_conus_2 <- amo_merged_250_clean_get_grid %>%
  mutate(population_density = population_density * 2,589,988.11) %>%
                                 filter(vmax_sust >= 17) %>%
                               left_join((county_acs_vars_bayesian %>% 
                                            filter(!is.na(median_house_value))),
                                          by = c("gridid" = "GEOID"))


test_bayesian_data_conus_2 %>%
  filter(!is.na(population_density))


rbind((test_bayesian_data_conus_2 %>%
         filter(!is.na(population_density)) %>% select(gridid, storm_id, scenario)),
      rowMeans(test_bayesian_conus))

######################33


load("01_data/test_bayesian_conus_prefiltered_unit_conversion.rda")


test_bayesian_data_conus <- amo_merged_250_clean_get_grid %>%
  filter(vmax_sust >= 17) %>%
  left_join((county_acs_vars_bayesian %>%
               mutate(population_density = population_density * 2,589,988.11) %>%
               na.omit()),
            by = c("gridid" = "GEOID")) %>%
  na.omit() 
                   

test_bayesian_conus_with_preds <- cbind(test_bayesian_data_conus, rowMeans(test_bayesian_conus))

png("bayesian_est_means.png", width = 2000, height = 1500, units = "px")
test_bayesian_conus_with_preds %>%
  clean_names() %>%
  group_by(gridid, scenario) %>%
  mutate(mean_bayesian_est = mean(x23)) %>%
  select(gridid, scenario, mean_bayesian_est, poverty_prop:population_density) %>%
  unique() %>%
  left_join(us_counties, by = c("gridid" = "GEOID")) %>%
  ggplot() +
  aes(
    geometry = geometry,
    fill = mean_bayesian_est
  ) +
  geom_sf(color = NA)  + facet_wrap(. ~ scenario) + scale_fill_viridis_c() + theme_void()
dev.off()

ggsave(filename = "bayesian_est_means.png")

counties_mort_65 <- read_tsv("01_data/all_cause_mortality_65_plus_county.txt") %>%
  filter(!is.na(State))

png("bayesian_est_means_mortality_rate.png", width = 2000, height = 1500, units = "px")
test_bayesian_conus_with_preds %>%
  clean_names() %>%
  group_by(gridid, scenario) %>%
  mutate(mean_bayesian_est = mean(x23)) %>%
  left_join((counties_mort_65 %>% clean_names()), by = c("gridid" = "county_code")) %>%
  select(gridid, scenario, mean_bayesian_est,
         poverty_prop:population_density,
         deaths:crude_rate_upper_95_percent_confidence_interval) %>%
  unique() %>%
  left_join(us_counties, by = c("gridid" = "GEOID")) %>%
  ggplot() +
  aes(
    geometry = geometry,
    fill = (mean_bayesian_est/100 * (deaths/population)) 
  ) +
  geom_sf(color = NA)  + facet_wrap(. ~ scenario) + scale_fill_viridis_c() + theme_void()
dev.off()

ggsave(filename = "bayesian_est_means_mortality_rate.png")

#png("test_png.png", width = 2000, height = 1500, units = "px")
test_bayesian_conus_with_preds %>%
  clean_names() %>%
  group_by(gridid, scenario) %>%
  mutate(mean_bayesian_est = mean(x23)) %>%
  left_join((counties_mort_65 %>% clean_names()), by = c("gridid" = "county_code")) %>%
  select(gridid, scenario, mean_bayesian_est,
         poverty_prop:population_density,
         deaths:crude_rate_upper_95_percent_confidence_interval) %>%
  unique() %>%
  left_join(us_counties, by = c("gridid" = "GEOID")) %>%
  ggplot() +
  aes(
    geometry = geometry,
    fill = (mean_bayesian_est/100 * (deaths/(population / 100000)) * population) 
  ) +
  geom_sf(color = NA)  + facet_wrap(. ~ scenario) + scale_fill_viridis_c() + theme_void()
#dev.off()

#ggsave(filename = "bayesian_est_means_mortalies.png")

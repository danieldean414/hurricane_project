

# Importing STORM data
library(tidyverse)
library(janitor)
library(parallel)
library(stormwindmodel)

storm_sim_col_names <- c('Year',        'Month',        'Storm number', 'Timestep (3-hourly)',  'Lat',  'Lon',  'Max wind (kt)','Min Pres (hPa)')

#amo_all_250 <- read_csv("All_250_years.csv", col_names = FALSE)
amo_all_250 <- read_csv("Synthetic_data/All_250_years_CONUS.csv", col_names = FALSE)

colnames(amo_all_250) <- storm_sim_col_names


# cleaning format


amo_all_250_clean <- amo_all_250 %>%
	filter(Year <= 20) %>%
  clean_names() %>%
  mutate(year = year + 1,
         storm_id = paste(storm_number,str_pad(year, width = 4,
                                  side = "left",
                                  pad = 1),sep = "-"),
       date = ( (lubridate::as_datetime(
         paste0(
         "1", str_pad(year, width = 3, side = 'left', pad = 0),
         "-",
         str_pad(month, width = 2, side = 'left', pad = 0),
         "-01"
         )
         )) + lubridate::hours(timestep_3_hourly * 3))
         ) %>%
mutate(date = as.character(date), 
date = str_sub(date, start = 0, end = 16) ) %>%
  rename( "latitude" = "lat",
    "longitude" = "lon",
    "wind" = "max_wind_kt") %>%
  select(storm_id, date, latitude, longitude, wind)

##
## Removing storms with less than 3 events recorded (~70)

amo_all_250_clean <- amo_all_250_clean %>%
  group_by(storm_id) %>%
  nest() %>%
  mutate(length_events = purrr::map(.x = data, .f = ~nrow(.x))) %>%
  unnest(length_events) %>%
  arrange(desc(length_events)) %>%
  unnest(data) %>%
  filter(length_events > 1) %>% 
  select(-length_events)



amo_all_250_clean %>% mutate(date_check = lubridate::ymd_hm(date)) %>% head()

amo_all_250_clean %>% mutate(date_check = lubridate::ymd_hm(date)) %>% filter(is.na(date_check)) %>% head()

write_csv(amo_all_250_clean, "amo_all_250_clean_input_test.csv")

## print test

amo_all_250_clean %>% head() %>% print()
 
# Processing for setup:

amo_all_250_clean_split <- split(amo_all_250_clean,
                                 f = amo_all_250_clean$storm_id)

# parallel processing

#all
start <- proc.time()
cl <- makeCluster(48) # 8 for ~full utilization
amo_all_250_clean_get_grid <- parLapply(cl,
                                        amo_all_250_clean_split,
                                        fun = get_grid_winds)
stopCluster(cl)
end <- proc.time()
print(end - start)

save(amo_all_250_clean_get_grid, file = "amo_all_250_clean_get_grid_alt_date_CONUS.rda")



# Plotting



amo_all_250_clean_get_grid_TEST_comb <- do.call("rbind", amo_all_250_clean_get_grid)

amo_all_250_clean_get_grid_TEST_comb <- amo_all_250_clean_get_grid_TEST_comb %>%
  rownames_to_column("storm_id") %>%
  mutate(storm_id = str_extract(storm_id,
                                "[0-9]-[0-9]*"))

write_csv(amo_all_250_clean_get_grid_TEST_comb, "amo_all_250_clean_get_grid_TEST_comb_1s_SHORT.csv")

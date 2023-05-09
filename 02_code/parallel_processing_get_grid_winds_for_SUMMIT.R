## Version of parallel `grid_winds` operation for each of the 250-year files:
  # will make one for `All`, `Pos`, `Neg`

    # Don't think the function has any external dependencies



# Importing STORM data
library(tidyverse)
library(janitor)
library(parallel)

storm_sim_col_names <- c('Year',	'Month',	'Storm number',	'Timestep (3-hourly)',	'Lat',	'Lon',	'Max wind (kt)','Min Pres (hPa)')

amo_all_250 <- read_csv("All_250_years.csv", col_names = FALSE)

colnames(amo_all_250) <- storm_sim_col_names


# cleaning format


amo_all_250_clean <- amo_all_250 %>%
  clean_names() %>%
  mutate(year = year + 1,
         #hours_base = timestep_3_hourly * 3,
         hours_base = timestep_3_hourly * 3,
         hour = hours_base %% 24,
         day = ((hours_base - hour) / 24) + 1,
         storm_id = paste(storm_number,
                          str_pad(year, width = 4,
                                  side = "left",
                                  pad = 0),
                          sep = "-")
         #wind = wind * 1.94384449 # converting to knots
  ) %>% 
  mutate(date = 
           paste0(str_pad(year, width = 4, side = "left", pad = 0),
                  str_pad(month, width = 2, side = "left", pad = 0),
                  str_pad(day, width = 2, side = "left", pad = 0),
                  str_pad(hour, width = 2, side = "left", pad = 0),
                  "00"
                  
           )) %>%
  rename(
    "latitude" = "lat",
    "longitude" = "lon",
    "wind" = "max_wind_kt"
  ) %>%
  select(storm_id, date, latitude, longitude, wind)

##

# Processing for setup:

amo_all_250_clean_split <- split(amo_all_250_clean,
                                 f = amo_all_250_clean$storm_id)

# parallel processing

#all
start <- proc.time()
cl <- makeCluster(7) # 8 for ~full utilization
amo_all_250_clean_get_grid <- parLapply(cl,
                                            amo_all_250_clean_split,
                                            get_grid_winds)
stopCluster(cl)
end <- proc.time()
print(end - start) 

save(amo_all_250_clean_get_grid_5yr, file = "amo_all_250_clean_get_grid_5yr.rda")


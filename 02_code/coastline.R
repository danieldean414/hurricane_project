library(openxlsx)
library(tidyverse)
library(ggplot)

coastlines <- read.xlsx("01_data/coastline-counties-list.xlsx", colNames = TRUE, startRow = 3) %>% clean_names()
  
  
  
  amo_all_250_clean_get_grid_TEST_comb %>%
    left_join(clean_names(coastlines), by = c("gridid" = "state_county_fips")) %>%
    left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
    ggplot() +
    aes(fill = coastline_region, geometry = geometry, color = NULL) +
    geom_sf() +
    theme_minimal()

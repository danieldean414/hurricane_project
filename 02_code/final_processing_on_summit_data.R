# Trying last bit of preprocessing CONUS data here (typo had crashed at last stage)


load("01_data/amo_all_250_clean_get_grid_alt_date_CONUS.rda")
load("01_data/amo_neg_250_clean_get_grid_alt_date_CONUS.rda")
load("01_data/amo_pos_250_clean_get_grid_alt_date_CONUS.rda")

# corresponds to amo_<>_250_clean_get_grid, I think

amo_all_250_clean_get_grid

# copying in last few lines of code after crash (should be fixed now, but seemed easier to rerun here)

# all



amo_all_250_clean_get_grid_TEST_comb <- do.call("rbind", amo_all_250_clean_get_grid)

amo_all_250_clean_get_grid_TEST_comb <- amo_all_250_clean_get_grid_TEST_comb %>%
  rownames_to_column("storm_id") %>%
  mutate(storm_id = str_extract(storm_id,
                                "[0-9]-[0-9]*"))


# negative 
amo_neg_250_clean_get_grid_TEST_comb <- do.call("rbind", amo_neg_250_clean_get_grid)

amo_neg_250_clean_get_grid_TEST_comb <- amo_neg_250_clean_get_grid_TEST_comb %>%
  rownames_to_column("storm_id") %>%
  mutate(storm_id = str_extract(storm_id,
                                "[0-9]-[0-9]*"))


# positive 
amo_pos_250_clean_get_grid_TEST_comb <- do.call("rbind", amo_pos_250_clean_get_grid)

amo_pos_250_clean_get_grid_TEST_comb <- amo_pos_250_clean_get_grid_TEST_comb %>%
  rownames_to_column("storm_id") %>%
  mutate(storm_id = str_extract(storm_id,
                                "[0-9]-[0-9]*"))


((amo_neg_250_clean %>% filter(wind >= 20))$storm_id %in% amo_neg_250_clean_get_grid_TEST_comb$storm_id) %>% summary()
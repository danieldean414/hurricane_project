## Seeing if `foreach` parallel backend is a viable alternative to parLapply

library(foreach)

#load("01_data/storm_hist_proc_hurricane_64_knots_plus.rda")
load(file = "01_data/storm_10k_obs_na_proc_hurricane_CORRECTED.rda")
load(file = "01_data/storm_hist_proc_hurricane_CORRECTED.rda")
# filtered w/ same conditions (minus landfall) as simualted data
# called "storm_hist_proc_hurricane"

#oh, check which windspeed filtering this is using
# I guess keep the names downstream the same?
storm_10k_all_obs_na_proc_hurricane_split <- split(storm_10k_obs_na_proc_hurricane,
                                                   f = storm_10k_obs_na_proc_hurricane$storm_id)


start <- Sys.time()#proc.time()

my.cluster <- parallel::makeCluster(
  7#, 
  #  type = "PSOCK"  # not sure if necessary
)

storm_10k_all_obs_na_proc_hurricane_split_ggw <- foreach(
  i = storm_10k_all_obs_na_proc_hurricane_split,
  .errorhandling='pass'
) %dopar% {
  stormwindmodel::get_grid_winds(i)
}


parallel::stopCluster(cl = my.cluster)
end <- Sys.time() #proc.time()
print(end - start) 


Sys.time()
storm_10k_all_obs_na_proc_hurricane_ggw <- do.call("bind_rows", c(storm_10k_all_obs_na_proc_hurricane_split_ggw, .id = "id"))
Sys.time()

## OK, doesn't seem to preserve nested dataframe names like rbind, so need to merge back in;

storm_ids_10_df <- data.frame(storm_id = names(storm_10k_all_obs_na_proc_hurricane_split),
                              id = as.character(seq(1:length(storm_10k_all_obs_na_proc_hurricane_split))))

storm_10k_all_obs_na_proc_hurricane_ggw <- storm_10k_all_obs_na_proc_hurricane_ggw %>%
  left_join(storm_ids_10_df, by = "id")

## historical 


storm_hist_proc_hurricane_split <- split(storm_hist_proc_hurricane,
                                                   f = storm_hist_proc_hurricane$storm_id)


start <- Sys.time()#proc.time()

my.cluster <- parallel::makeCluster(
  7#, 
  #  type = "PSOCK"  # not sure if necessary
)

storm_hist_proc_hurricane_split_ggw <- foreach(
  i = storm_hist_proc_hurricane_split,
  .errorhandling='pass'
) %dopar% {
  stormwindmodel::get_grid_winds(i)
}


parallel::stopCluster(cl = my.cluster)
end <- Sys.time() #proc.time()
print(end - start) 


Sys.time()
storm_hist_proc_hurricane_ggw <- do.call("bind_rows", c(storm_hist_proc_hurricane_split_ggw, .id = "id"))
Sys.time()

# re-adding storm IDs


storm_ids_hist_df <- data.frame(storm_id = names(storm_hist_proc_hurricane_split),
                              id = as.character(seq(1:length(storm_hist_proc_hurricane_split))))

storm_hist_proc_hurricane_ggw <- storm_hist_proc_hurricane_ggw %>%
  left_join(storm_ids_hist_df, by = "id")

############3

save(storm_10k_all_obs_na_proc_hurricane_ggw, file = "01_data/storm_10k_all_obs_na_proc_hurricane_ggw.rda")
save(storm_hist_proc_hurricane_ggw, file = "01_data/storm_hist_proc_hurricane_ggw.rda")

#####################################################################################################
###########################################################################################3

# not sure where to put this, but finally worked out how to pin down the error(s)
  # there was only one!!! <-- I guess at least should generalize
    # and presumably at least one <3-timestep storm since filtering fixed the error earlier

tibble(data = storm_10k_all_obs_na_proc_hurricane_split_ggw ) %>%
  rownames_to_column("number") %>% 
  mutate(class = purrr::map(.x = data, .f = ~paste0(class(.x)))) %>%
  unnest(class) %>% 
  filter(str_detect(class, "error"))

# trying to remmber if/how storm IDs are stored; sometimes `row_names` works?
  # 1301 anyway; I *think* that should just carry over to indexing on the input?
    # Oh, storm IDs are preserved at least to this point

storm_10k_all_obs_na_proc_hurricane_split[1301]
  # wow, actually 238 timesteps, so not a matter of length (assuming it's the right one)

storm_10k_all_obs_na_proc_hurricane_split[1301][[1]] %>%
  ggplot() +
  aes(y = latitude,
      x = longitude) +
  geom_point()
  # pretty convoluted path, but nothing jumps out 

# Ok, let's double-check that it doesn't work...

stormwindmodel::get_grid_winds(
  storm_10k_all_obs_na_proc_hurricane_split[1301][[1]]
)
  # OK, this *is* the one with the '"to" must be a finite number' error;
  


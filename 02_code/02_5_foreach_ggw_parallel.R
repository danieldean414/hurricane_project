## Seeing if `foreach` parallel backend is a viable alternative to parLapply

library(foreach)

my.cluster <- parallel::makeCluster(
  7#, 
#  type = "PSOCK"  # not sure if necessary
)
doParallel::registerDoParallel(cl = my.cluster)

#foreach::getDoParRegistered() # checking if it works ['true']

x <- foreach(
  i = 1:10, 
  .combine = 'c'
) %dopar% {
  sqrt(i)
}
x

parallel::stopCluster(cl = my.cluster)



#####################################3
# OK, I guess might as well jump in and try the GGW data (use a subset)

storm_10k_all_obs_na_proc_hurricane_TMP <- storm_10k_all_obs_na_proc_hurricane %>%
  filter(str_detect(storm_id, "-9$"))
# I guess keep the names downstream the same?
storm_10k_all_obs_na_proc_hurricane_split <- split(storm_10k_all_obs_na_proc_hurricane_TMP,
                                                   f = storm_10k_all_obs_na_proc_hurricane_TMP$storm_id)


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
storm_10k_all_obs_na_proc_hurricane_ggw <- do.call("rbind", storm_10k_all_obs_na_proc_hurricane_split_ggw)
Sys.time()

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
  



# trying to work out which of these are ~functional/final steps
#################################################################33

# Using historical data
test_bayesian_data <- (hurricaneexposuredata::storm_winds %>% 
                         left_join(county_acs_vars_bayesian, 
                                   by = c("fips" = "GEOID")))
##########

## replacing w/ some of the locally-processed STORM data:
test_bayesian_data <- storm_10k_obs_na_proc_ggw %>%
  rownames_to_column("storm_id") %>%
  mutate(storm_id = str_extract(storm_id, pattern = "\\d\\-\\d{4}\\-\\d")) %>%
  filter(!is.na(date_time_max_wind))  %>%
  left_join(county_acs_vars_bayesian, 
            by = c("gridid" = "GEOID"))

# testing 1:100 for a sense of timing; coincidentally no NA's but need to addres longer-term
  # missing the `storm_id` present in the example data
  # Oh, I think there's a way 

Sys.time()
test_bayesian <- predict(modobj, newdata = (test_bayesian_data[1:100,]) )
  # OK, no structure as-is (just a long 1-by-x vector); could either use something like `rbind`, or apply rowwise
  # ohh, still NAs for things like median income, etc.; I remember this is a concern
Sys.time()

# Well, on the bright side, runs pretty much instantaneously with 100 rows;
  # output is 47000 numbers; I guess 470 per row? seems like an odd 'default' number
  

test_bayesian_single <- predict(modobj, newdata = (test_bayesian_data[13,]))
  # 1000 ~rows (trying to remember how to handle--do you do summary statistics?)
    # so rowwise application might make sense

# OK, this could work:

test_bayesian_rowwise <- test_bayesian_data %>%
  filter(vmax_sust >= 17.4) %>%
  group_by(gridid, storm_id) %>%
  nest() %>%
  mutate(impact = purrr::map(.x = data, .f = ~predict(modobj, newdata = .x)) )#,
         #impact_summary = purrr::map(.x = impact, .f = ~summary(unlist(.x))))

# OK, a little risky, but I *think* Na/zero-row results are just where the storm doesn't hit that county
  # Would it follow that each storm will have all ~3000ish counties, then?

  # no--everywhere from 1-2 to hundreds of counties:
test_bayesian_rowwise %>%
  ungroup() %>%
  group_by(storm_id) %>%
  tally() %>%
  summary()

## Huh, so then what *do* NA's mean here?
  # maybe something w/ filtering to min. windspeeds on the basis of exposures?

test_bayesian_rowwise_summarized <- test_bayesian_data %>%
  filter(vmax_sust >= 17.4) %>%
  group_by(gridid, storm_id) %>%
  nest() %>%
  mutate(impact = purrr::map(.x = data, .f = ~predict(modobj, newdata = .x)),
         impact_lower = purrr::map(.x = impact, .f = ~quantile(.x, probs = c(0.025))),
         impact_median = purrr::map(.x = impact, .f = ~quantile(.x, probs = c(0.5))),
         impact_upper = purrr::map(.x = impact, .f = ~quantile(.x, probs = c(0.975))))

############################################################3
#  trying to work out why there are NAs
ggplot() +
  geom_sf(aes(fill = impact_median,
              geometry = geometry),
          data = test_bayesian_rowwise_summarized %>%
            unnest(impact_median) %>% 
            filter(storm_id == "0-0002-0") %>%
            left_join(us_counties_wgs84, by = c("gridid" = "GEOID"))) +
  geom_sf(aes(  geometry = geometry, color = wind), 
          data = (storm_10k_obs_na_proc_split$`0-0002-0` %>%
                    mutate(longitude = longitude - 360) %>%
                    st_as_sf(coords = c('longitude', 'latitude'),
                             crs = "WGS84"))) +
  scale_color_viridis_c(option = 'plasma') +
  scale_fill_viridis_c(option = "mako") + 
  theme_void()

  # not too enlightening; not sure why those ones are NA's vs ~0 like the other peripheral counties

storm_ids_na <- test_bayesian_rowwise_summarized %>%
  dplyr::select(-impact, -data) %>% 
  unnest(impact_lower:impact_upper) %>%
  filter(is.na(impact_median)) %>%
  dplyr::select(storm_id)

# 545  unique storms with"NA" values:
storm_ids_na %>% ungroup() %>% dplyr::select(storm_id) %>% unique() %>% dim()
# for some reason apparently only 471 matching?? 
storm_10k_obs_na_proc %>% filter(storm_id %in% storm_ids_na$storm_id) %>%
  dplyr::select(storm_id) %>% unique() %>% dim()

###############################################################

  # huh, no storm_id in my version
  # oh, adding a summary step seems to make the runtime a lot longer...

test_df <- test_bayesian_rowwise_summarized %>%
  unnest(c(impact_lower, impact_median, impact_upper)) %>%
  ungroup() %>%
  group_by(gridid) %>% 
  dplyr::summarize(impact_median_median = median(impact_median, na.rm = T),
                   impact_lower_median = median(impact_lower, na.rm = T), 
                   impact_upper_median = median(impact_upper, na.rm = T))


test_df %>% 
  filter(!is.na(impact_median_median)) %>%
  left_join(county_acs_vars_bayesian, by = c("gridid" = "GEOID")) %>%
  ggplot() +
  aes(geometry =geometry)


  # Oh, just realized this doesn't have the storm categories here; I guess treat as one unit?

test_bayesian_rowwise %>%
  ungroup() %>%
  dplyr::select(fips, storm_id, impact, - storm_id) %>%
  group_by(fips) %>%
  mutate(impact = sum(impact))
  
  
mutate(impact_summary = purrr::map(.x = impact, .f = ~get_95_ci(data_in = unlist(.x))))



# Weird; just grabbing means only takes a few seconds, but something like `summary()` runs indefinitely
  
# Trying to get 95% CI assuming a normal distirubiton

get_95_ci <- function(data_in){
  s_d = sd(data_in, na.rm = TRUE)
  error = qnorm(0.975)*s_d/sqrt(length(data_in))
  mean = mean(data_in)
  
  sd_obj = list(
    mean = mean,
    lower = mean - error,
    upper = mean + error
  )
  return(sd_obj)
}

test_bayesian_rowwise %>%
  unnest(data)
  mutate(impact_summary = purrr::map(.x = impact, .f = ~get_95_ci(unlist(.x))))
  
 

# Trying to write a 95% CI script
# https://www.cyclismo.org/tutorial/R/confidence.html#:~:text=We%20use%20a%2095%25%20confidence%20level%20and%20wish,%2B%20error%20%3E%20left%204.063971%20%3E%20right%205.936029 from 

#sample mean is 5, the standard deviation is 2, and the sample size is 20 

a <- 5
 s <- 2
 n <- 20
 error <- qnorm(0.975)*s/sqrt(n)
 left <- a-error
 right <- a+error
 left
#[1] 4.123477
 right
#[1] 5.876523

####

############################################################################################################
  # OK, think everything below this can be skipped, but holding off on deleting for now...
############################################################################################################

#test_bayesian_data_conus <- (amo_merged_250_clean_get_grid %>% 
#                               left_join(county_acs_vars_bayesian, 
#                                         by = c("gridid" = "GEOID")))

test_bayesian_data_conus <- tc_merged_data

test_bayesian_conus <- predict(modobj, newdata = (test_bayesian_data_conus %>% 
                                                    filter(!is.na(median_house_value))))

# OK, keep running into error  ('cannot allocate vector of size 1014.2 Mb <-- seems reasonable!)
  # Error in UseMethod("predict") : 
      # no applicable method for 'predict' applied to an object of class "tcExcessMort"

# Don't seem to have at least made any comments on this, so maybe there's some auxiliary function I'm missing?

#### OK, filtering for rep dropped RAM load, but still too large; let's try a loop and/or parallelization?
# If so, maybe try filtering by county? (Trying to anticipate ~nonlinear responses)

# Processing for setup:

test_bayesian_data_conus_split <- split(test_bayesian_data_conus,
                                        f = paste(test_bayesian_data_conus$storm_id,
                                                  test_bayesian_data_conus$scenario,
                                                  sep = "_"))


test_bayesian_conus_list <- list()

for(storm_id_TMP in test_bayesian_data_conus_split){
  test_bayesian_data_conus_TMP <- storm_id_TMP %>% filter(!is.na(median_house_value))
  test_bayesian_conus_TMP <- predict(modobj, newdata = test_bayesian_data_conus_TMP) %>%
    data.frame()
  test_bayesian_conus_list <- append(test_bayesian_conus_list, test_bayesian_conus_TMP)
}


  # Oh, interesting--looks like it crashed in the notes, but worked this one
#### Nope--still not enough memory (same figure of 28.7 Gb as well); let's try parallel processing?

# parallel processing

# It isn't handling "external" objects well

# Setting up function?

modobj_predict <- function(x){
  A <- predict(modobj, newdata = x)
  return(A)
}

# Another method?

result_list <- llply(test_bayesian_data_conus_split, function(x) {
  # do the replacing
  return(x)
})


#library(parallel)
#all
start <- proc.time()
#cl <- makeCluster(7, "modobj") # 8 for ~full utilization
cl <- makeCluster(5) # 8 for ~full utilization
clusterEvalQ(cl, source('02_code/pred_function.R'))
#clusterEvalQ(cl, library(Hmisc))
test_bayesian_data_conus_split_par <- parLapply(cl,
                                                test_bayesian_data_conus_split,
                                                modobj_predict
)
stopCluster(cl)
end <- proc.time()
print(end - start) 

  #~1-2 seconds, or ~16 'elapsed'

# trying to figure out how it lines up with FIPS; seems to drop rows w/ missing data
test_bayesian_10 <- predict(modobj, newdata = test_bayesian_data[1:10,])
# So I think every row is a county (w/ 1000 predictions?); I have to assume in order provided
# However, full dataset is shorter than provided list; presumably dropping relevant NAs
# could create ~cascading mismatches since it just outputs a numeric dataframe
# so need to pre-filter carefully (trying that above--let's see if it matches)

# OK, removing the 342 counties without median house value data accounts for difference in length
  # looks like I'm using a fixed year ; I don't *think* the function uses any year-to-year trends?

test_bayesian_fixed_year <- predict(modobj, newdata = (test_bayesian_data %>% mutate(year = 2015)))

county_mortality_pred_bayesian <- function(fips = counties_mort_65$`County Code`,
                                           storm_data = (hurricaneexposuredata::storm_winds %>% 
                                                           left_join(county_acs_vars_bayesian, 
                                                                     by = c("fips" = "GEOID"))),
                                           pred_model = modobj,
                                           counties = counties_mort_65,
                                           windspeed_min = 0,
                                           windspeed_max = Inf,
                                           storm_ids = ".*") {
  estimates <- counties %>%
    clean_names() %>%
    filter(county_code %in% fips) %>%
    inner_join(
      storm_data %>%
        filter(vmax_sust > windspeed_min &
                 vmax_sust <= windspeed_max) %>%
        filter(str_detect(storm_id, storm_ids)) %>% 
        mutate(storm_year = as.numeric(
          str_extract(storm_id, "[0-9]{4}"))) %>%
        mutate(storm_years = length(unique(storm_year))),
      mutate(
        storm_years = max(storm_year - min(storm_year)) + 1
      ),
      by = c("county_code" = "fips")
    )
  
  if(nrow(estimates > 0)){
    estimates <- estimates %>%
      mutate(est_excess_death_rate = 
               #predict(pred_model, newdata = data.frame( windspeed = vmax_sust)),
               predict(pred_model, newdata = storm_data),
             est_excess_deaths = (est_excess_death_rate *  population) / 100000)
    return(estimates %>%
             select(county_code, state, county, vmax_sust, storm_id, storm_year, est_excess_death_rate, est_excess_deaths, population, deaths, crude_rate, storm_years))
  }
  
  else{
    return("No data meeting parameters")
  }
  
}

county_mortality_pred_bayesian(pred_model = predict.tcExcessMort)



predict.tcExcessMort(newdata = (hurricaneexposuredata::storm_winds %>% 
                                  left_join(county_acs_vars_bayesian, 
                                            by = c("fips" = "GEOID"))))

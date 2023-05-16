
# trying to work out which of these are ~functional/final steps
#################################################################33

# Using historical data
test_bayesian_data <- (hurricaneexposuredata::storm_winds %>% 
                         left_join(county_acs_vars_bayesian, 
                                   by = c("fips" = "GEOID")))

test_bayesian <- predict(modobj, newdata = (test_bayesian_data %>% filter(!is.na(median_house_value))))

####

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


library(parallel)
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



# trying to figure out how it lines up with FIPS; seems to drop rows w/ missing data
test_bayesian_10 <- predict(modobj, newdata = test_bayesian_data[1:10,])
# So I think every row is a county (w/ 1000 predictions?); I have to assume in order provided
# However, full dataset is shorter than provided list; presumably dropping relevant NAs
# could create ~cascading mismatches since it jsut outputs a numeric dataframe
# so need to prefilter carefully (trying that above--let's see if it matches)

# OK, removing the 342 counties without median house value data accounts for difference in length


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
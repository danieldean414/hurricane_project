###################################################################
# Example code for:
#   "Tropical cyclone exposures and risks of emergency Medicare hospital 
#    admission for cardiorespiratory diseases in 175 United States 
#    counties, 1999–2010"
###################################################################


################### Storm exposure assessment #####################

# County-level exposures to Altantic-basin tropical cyclones can be 
# conducted using an open-source R package "hurricaneexposure" by
# Dr. Brooke Anderson. Here is a tutorial:
# https://cran.r-project.org/web/packages/hurricaneexposure/vignettes/hurricaneexposure.html

# Assume we have generated an indicator variable denoting county-level 
# storm exposure based on a certain exposure metric (i.e., a variable that
# is "1" on the day a storm is closest to a county for all storms that
# cause exposures for that county and a "0" on all other days. (See the
# "hurricaneexposure" tutorial for examples of creating this type of 
# exposure data for US counties). 

# A hypothetical data ("dat") is attached with this code file

# PLEASE NOTE: The hypothetical data set is given to show the example
# format for data to input to the code used for this analysis. It isn't
# real data, and so the results of this code will not produce the 
# results shown in the main paper. Instead, it is shared here to provide
# example code that could be used to replication the methods of the 
# paper with other datasets.


########################### Data format ###############################

# We have provided a dataset with hypothetical data (note: not the real data
# used in the paper) under the file "dat.csv". This file is provided to 
# show the required format of data to input to this code. If you convert
# other health datasets to this format, you should be able to input them
# into this code to run the same model as used in the main text of this paper.

# dat: a time series data for seven counties from 1999 to 2010, with rows 
#      representing days and columns showing variables, which include

# This data should have the following columns: 

## "fips": county's fips
## "date": date of each day
## "year": year for each day
## "doy": day of year, used to find similar non-storm days in other years
## "dow": day of week
## "storm": an indicator variable for tropical cyclone exposure, "storm" is 1 
##          if a day is exposed to storm and 0 for non-storm exposed days (or
##          other binary exposure of interest)
## "storm_id": the name and year of storm (or other disaster identifier)
## "cardio": daily hospitalizations of cardiovascular disease (or other health outcome)
## "denom": daily total number of Medicare beneficiaries residing in a county 
##          who were not already hospitalized on a given day (or other relevant denominator
##          for the health outcome of interest)

dat <- read.csv("dat.csv")
dat$date <- as.Date(dat$date)


##### Match storm-exposed days to compariable unexposed days ######

set.seed(123)
fips_list <- unique(dat$fips)

for(i in 1:length(fips_list)){
  df <- subset(dat, fips == fips_list[i])
  
  # exclude the 3 days within any other storm
  df$time <- 1:nrow(df)
  cand_control <- unique(c(which(df$storm == 1) , which(df$storm == 1) + 1,
                           which(df$storm == 1) - 1))
  df$cand_control <- TRUE
  df$cand_control[cand_control] <- FALSE
  # exclude the two weeks following 2001-9-11
  two_week_911 <- seq(as.Date("2001-09-11"),  as.Date("2001-09-11") + 14, by = 1)
  df$excu_911 <- ifelse(df$date %in% two_week_911, FALSE, TRUE)
  
  case_dates <- subset(df, storm == 1)
  control_dates <- subset(df, storm == 0)
  
  for(j in 1:nrow(case_dates)){
    # choose lags (lagged -2 to lagged 7)
    lag_dates <- case_dates[j, ]$date + -2:7
    lag_case <- subset(df, date %in% lag_dates)
    
    # choose 10 compariable unexposed days for each storm-exposed day
    control_range <- case_dates[j, ]$doy + -3:3
    control_subset <- subset(control_dates,
                             control_dates$year != case_dates[j, ]$year &
                             doy %in% control_range &
                             cand_control & excu_911)
        controls <- dplyr::sample_n(control_subset, 10)
        
        # choose lagged days for selected unexposed days
        la_con <- c(-2:-1, 1:7)
        for(p in 1:length(la_con)){
          lag_control_dates <- controls$date + la_con[j]
          lag_control_each <- subset(df, date %in% lag_control_dates)
          
          if(p == 1){
            lag_control <- lag_control_each
          }else{
            lag_control <- rbind(lag_control, lag_control_each)
          }
        }
  j_stratum <- rbind(lag_case, controls, lag_control)
  stratum <- paste("stratum", j, sep = ".")
  j_stratum$stratum <- stratum
  status <- c(rep("case", length(-2:7)), rep("control", 10*(length(-2:7))))
  j_stratum$status <- status
  lag <- c(-2:7, rep(0, 10), rep(c(-2:-1, 1:7), each = 10))
  j_stratum$lag <- lag
  
  if(j == 1){
    new_df <- j_stratum
  }else{
    new_df <- rbind(new_df, j_stratum)
  }
}
   if(i == 1){
     matched_df <- new_df
   }else{
     matched_df <- rbind(matched_df, new_df)
   }
}
# matched_df is a matched multi-county data set


######### Estimate the effects storms on hospitalizations #########

# We use "dlnm" package to generate the distributed lag function for "storm" 
library(lme4); library(dlnm); library(dplyr)

for(i in 1:length(fips_list)){
  orig_dat <- subset(dat, fips == fips_list[i])
  match_dat <- subset(matched_df, fips == fips_list[i])
  
  orig_cb <- dlnm::crossbasis(orig_dat$storm, lag = c(-2, 7),
                              argvar = list(fun = "lin"),
                              arglag = list(fun = "integer"))
  obs_n <- nrow(orig_dat)
  orig_cb_matr <- as.data.frame(subset(orig_cb, nrow = obs_n))
  orig_cb_matr$date <- orig_dat$date
  matched_date <- match_dat %>% dplyr::select(date)
  matched_cb_matr <- orig_cb_matr %>%
    dplyr::right_join(matched_date, by = "date") %>%
    dplyr::select(-date) %>% as.matrix()
  
  if(i == 1){
    matched_cb_matrix <- matched_cb_matr
  }else{
    matched_cb_matrix <- rbind(matched_cb_matrix, matched_cb_matr)
  }
  # add attributes
  matched_dim <- dim(matched_cb_matrix)
  attr <- attributes(orig_cb)
  attr$dim <- matched_dim
  matched_cb <- matched_cb_matrix
  attributes(matched_cb) <- attr
}

# mixed-effect model
fit <- lme4::glmer(cardio ~ matched_cb + factor(year) + factor(dow) + (1|fips),
                   data = matched_df, offset = log(denom),
                   family = poisson(link = "log"),
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)))
pred <- dlnm::crosspred(matched_cb, fit, at = 1)

# calculate overdispersion parameter
blmeco::dispersion_glmer(fit)

# estimates of distributed relative risks（RR）for cardiovascular hospitalizations 
# on storm-exposed days compared with matched unexposed days 
# for all storms and across the seven counties
pred$matRRfit

# lower and upper confidence intervals for distriuted RRs
pred$matRRlow; pred$matRRhigh

# estimate of overall RR for the entire storm-exposure period (i.e., ten days)
over_rr <- sum(pred$matRRfit)/10

# we used "delta method" to calculate confidence interval for the overall 
# RR for the entire storm period
library(msm)
estvar <- pred$vcov
estmean <- c(pred$coefficients)

over_rr_se <- msm::deltamethod(~ (exp(x1) + exp(x2) + exp(x3) + exp(x4) + 
                                  exp(x5) + exp(x6) + exp(x7) + 
                                  exp(x8) + exp(x9) + exp(x10))/10, 
                               estmean, estvar)
over_rr_low <- over_rr / exp(1.96*over_rr_se)
over_rr_high <- over_rr * exp(1.96*over_rr_se)


################# Sensitivity analyses ###################
# alternative model 1
fit_am1 <- glm(cardio ~ matched_cb + factor(dow) + factor(year) + factor(fips),
                      data = matched_df, offset = log(denom), 
               family = poisson(link = "log"))
pred_am1 <- dlnm::crosspred(matched_cb, fit_am1, at = 1)

# alternative model 2
matched_df$year <- as.numeric(matched_df$year)
matched_df$fips <- factor(matched_df$fips)
fit_am2 <- glm(cardio ~ matched_cb + factor(dow) + year * fips,
           data = matched_df, offset = log(denom), 
           family = poisson(link = "log"))
pred_am2 <- dlnm::crosspred(matched_cb, fit_am2, at = 1)

# alternative model 3
matched_df$year <- as.numeric(matched_df$year)
fit_am3 <- lme4::glmer(cardio ~ matched_cb + factor(dow) + (1 + year|fips) + year,
                   data = matched_df, offset = log(denom), family = poisson(link = "log"), 
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
pred_am3 <- dlnm::crosspred(matched_cb, fit_am3, at = 1)

# alternative model 4: conditional Poisson model
fit_am4 <- gnm::gnm(cardio ~ matched_cb + factor(dow) + factor(year),
                eliminate = factor(fips), family = poisson(link = "log"),
                data = matched_df, offset = log(denom))
pred_am4 <- dlnm::crosspred(matched_cb, fit_am4, at = 1)

# alternative model 5: conditional quasiPoisson model
fit_am5 <- gnm::gnm(cardio ~ matched_cb + factor(dow) + factor(year),
                eliminate = factor(fips), family = quasipoisson(link = "log"),
                data = matched_df, offset = log(denom))
pred_am5 <- dlnm::crosspred(matched_cb, fit_am5, at = 1)

# alternative 6: 
matched_df$obs <- factor(1:nrow(matched_df))
fit_am6 <- glm(cardio ~ matched_cb + factor(year) + factor(dow) + (1|fips) + (1|obs),
               data = matched_df, offset = log(denom), 
               family = poisson(link = "log"),
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
pred_am6 <- dlnm::crosspred(matched_cb, fit_am6, at = 1)

# alternative 7
fit_am7 <- MASS::glmmPQL(cardio ~ matched_cb + factor(year) + factor(dow) + offset(log(denom)),
                         random = ~ 1|fips, data = matched_df, 
                         family = quasipoisson(link = "log"),
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
pred_am7 <- dlnm::crosspred(matched_cb, fit_am7, at = 1)



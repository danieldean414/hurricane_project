## load the model output ##
load('02_code/modobj.RData')
library(Hmisc)

## function to predict (should work with predict() in R)
predict.tcExcessMort<-function(modobj=modobj, newdata){
  ##  newdata should be a data frame with county/storm events in the rows and the following variables in the columns
  ##  note that all variables should be un-transformed-- any necessary transformations will be performed within the function
  ##  all variables should be numeric, with the coastal counties variable coded as binary 0/1
  
  ##  vmax_sust: modeled maximum sustained wind speed at the population centroid of the county during the TC in m/s (from hurricaneexposuredata package)
  ##  sust_dur: duration of sustained wind speeds above 20 m/s at the population centroid of the county during the TC (from hurricaneexposuredata package)
  ##  exposure: total number of TC exposures experienced by the county during 1999-2015 (derived from hurricaneexposuredata package)
  ##  poverty_prop: proportion of residents in poverty          
  ##  white_prop: proportion of residents identifying as white
  ##  owner_occupied_prop: proportion of homes that are owner-occupied
  ##  age_pct_65_plus_prop: proportion of residents age 65+
  ##  median_age: county median age
  ##  population_density: county population density
  ##  median_house_value: county median house value
  ##  no_grad_prop: proportion of residents without a high school diploma
  ##  year: year the TC occurred
  ##  coastal: whether the county is coastal (0/1)
  
  newdata$population_density<-(newdata$population_density-modobj$popdens_scale[1])/modobj$popdens_scale[2]
  newdata$median_house_value<-(newdata$median_house_value-modobj$medhouseval_scale[1])/modobj$medhouseval_scale[2]
  newdata$year<-(newdata$year-modobj$year_scale[1])/modobj$year_scale[2]
  
  ## transform covariates as needed for the model ##
  X<-model.matrix(~rcspline.eval(vmax_sust,inclx = T,knots=modobj$vmaxsust_knots)+
                    sust_dur+exposure+poverty_prop+white_prop+owner_occupied_prop+
                    age_pct_65_plus_prop+median_age+population_density+median_house_value+
                    no_grad_prop+rcspline.eval(year,inclx = T,knots=modobj$year_knots)+coastal,data=newdata)
  
  ## add appropriate names to line up with those in the matrix of posterior samples ##
  vnm_proc<-c('intercept','vmax_sust_s1','vmax_sust_s2','vmax_sust_s3',"sust_dur","exposure","poverty_prop","white_prop","owner_occupied_prop","age_pct_65_plus_prop",
              "median_age","population_density","median_house_value","no_grad_prop","year_s1","year_s2","coastal")
  
  colnames(X)<-vnm_proc
  
  ## get posterior samples of the predictions for each TC/county in newdata ##
  post_pred<-X%*%t(modobj$beta_post)
  
  return(post_pred)
}


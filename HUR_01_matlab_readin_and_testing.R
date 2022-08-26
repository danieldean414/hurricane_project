# Trying to consolidate tests with matlab output; hoping one of these is a good substitute 
  # can compare to rendered plots for ~accuracy

library(R.matlab)
library(ggplot2)
library(tidyverse)

#test_matlab <- readMat("TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/ibtracs.NA_since-9999_dx1.mat")
test_matlab <- readMat("TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/simulated.NA10years_dx1.mat")

test_matlab 
  # Huh, doesn't seem to have a ~storm identifier or anything like that
      # Huh, I think columns might be the storms?
        # Yeah, 76 x 241 
      
  # Does seem like there's enough to work with (cooridantes, vmax, etc.)
    # not sure what format time is in, though
    # Do we need anything but vmax? Need to review ~full model
  
  # Good way to convert a matrix to a list?
      # if it were a d.f., could do `pivot_longer`

test_matlab_df <- data.frame(
  storm_id = "placeholder",
  time = -999,
  lat = -999,
  lon = -999,
  vmax = -999
)

for(storm in 1:nrow(test_matlab$time)){
  storm_id <- paste(storm)
  time <- test_matlab$time[storm,]
  lat <- test_matlab$lat[storm,]
  lon <- test_matlab$lon[storm,]
  vmax <- test_matlab$vmax[storm,]
  
  test_matlab_df <- rbind(test_matlab_df,
                          data.frame(storm_id, time, lat, lon, vmax))
}

test_matlab_df <- test_matlab_df %>%
  filter(storm_id != "placeholder")

library(sf)
# Still trying to get time format worked out...
  # `as_date` doesn't work--e.g .694480.8 comes up as "3871-06-04"
    # Probably something like hours since some reference point -- generated at the default 3-hr timestep
        # Huh, so maximum time involved would be 723 hours (241*3), I think, or about a month; seems plausible
        # So, each column in is 0.1-unit increments; if I assume each of those is 3 hours, 1 unit is 30 hours [seems odd]
          # so to grab a random time, 694592.2 = 20837766 hours, or 124034.3 weeks, ~ 2385.275 years
              # Can't remember exactly what range this was, but if it was e.g. year 2000, reference time would be ~385 BC <-- seems arbitrary
    # Alright, based on the code/comments, it's set up to have the start/reference date be 1970-01-01 (presumably midnight?)
      # So I need to figure out ~700000 of *what* from 1970-01-01 puts me at a reasonable date.
        # Supposing it's ~30ish years, maybe it's something like seconds or minutes? (e.g. 525600 minutes in a year)
            # Oh, that's way too many; maybe hours? (8760/yr) <- would put it at something like 80 years, or ~2050
          # But then seems odd to have 0.1-unit changes
    # OK, also remember I don't care about the "date/year" per se, but do want to know what season they're in (approximately)
      # Oh, so actually these might be over the course of a single "year" after all, in which case minutes might make more sense

    # alright, so minimum time is 694480, maximum is 697971, or difference of 3491

    # Yeah, confirmed that there were 76 hurricanes generated
    # Maybe 694480 is somewhere early in "1970", and reflects hours(?) since some starting point
      # Still seems like the 0.1-unit increments should reflect 3 hours based on the code setup
        # I guess it could be being interpolated?
  # Looking like unix time is the best fit so far; lubridate automatic reading is taking this all as a few hours, though...
      # should be more like months?
    
# OK, at least historical data has same time format as far as I can tell
  # Not positive what to do with this information, though; I guess I know the range of inputs, but not really a "key"

library(lubridate)


test_matlab_sf <- test_matlab_df %>%
  #filter(storm_id != "placeholder") %>%
  filter(!is.na(lon)) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = "WGS84")

  # Do I want to keep the coordinates for anything?

  # end up with NAs; different lengths of storms (makes sense), so pretty sure I can cut rows with NAs now
library(viridis)
library(mapview)
library(sf)


test_matlab_df %>% filter(!is.na(lat)) %>%
  st_as_sf(coords = c('lon', 'lat')) %>%
  ggplot() +
  aes(
    y = lat,
    x = lon,
    color = vmax
  ) +
  geom_point() +
  theme_void() +
  scale_color_viridis(option = "turbo")

  
# Trying layering [I think this works in ggplot, too]
  
  test_matlab_df_storms_layer <- test_matlab_df %>%
    filter(storm_id != "placeholder") %>%
    filter(!is.na(lat)) %>%
    st_as_sf(coords = c('lon', 'lat'))
  
  library(tmap)
  
  
  tm_shape(us_counties) +
    tm_borders() +
  tm_shape(test_matlab_df_storms_layer) +
    tm_dots()
  
  
  # Looks reasonable to me, I think <-- should have checked how many sotrms were generated <-- should still be on ext. drive

# Seeing if historical equivalent gives any insight
  #think this is the right one:
  
test_matlab_hist <- readMat("TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/ibtracs.NA_since-9999_dx1.mat")
# also reading in netcdf 

library(ncdf4)
library(ncdf4.helpers)

test_netcdf <- nc_open("C:/Users/Admin/Documents/hurricane_project/tcwise/TCWiSE_toolbox_v1.0.79/02_application/data/ibtracs_2000.nc")

# Check netcdf metadata for input files

require(lubridate)
ncdims <- names(test_netcdf$dim) #get netcdf dimensions
timevar <- ncdims[which(ncdims %in% c("date_time"))[1]] #find time variable
times <- ncvar_get(test_netcdf, timevar)


####### OK, next, need to match this with counties [so figure out CRS situation], and then plug into model
  # Might be a good situation for parallel processing

# Alright, so turns out Matlab has a kind of unusual format where it reports seconds since year 0000, rather than 1970; 
  # found a script to convert to a more standard format

#https://lukemiller.org/index.php/2011/02/converting-matlab-and-r-date-and-time-values/#:~:text=Inside%20R%2C%20converting%20from%20the%20MATLAB%20%E2%80%98%20datenum,are%20in%20a%20time%20zone%20other%20than%20UTC.

# Filename: matlab_time.R
# Convert between MATLAB datenum values and R POSIXt time values.
# 
# Author: Luke Miller   Feb 20, 2011
###############################################################################

#Convert a numeric  MATLAB datenum (days since 0000-1-1 00:00) to seconds in 
#the Unix epoch (seconds since 1970-1-1 00:00). Specify a time zone if the 
#input datenum is anything other than the GMT/UTC time zone. 
matlab2POS = function(x, timez = "UTC") {
  days = x - 719529 	# 719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 # 86400 seconds in a day
  # This next string of functions is a complete disaster, but it works.
  # It tries to outsmart R by converting the secs value to a POSIXct value
  # in the UTC time zone, then converts that to a time/date string that 
  # should lose the time zone, and then it performs a second as.POSIXct()
  # conversion on the time/date string to get a POSIXct value in the user's 
  # specified timezone. Time zones are a goddamned nightmare.
  return(as.POSIXct(strftime(as.POSIXct(secs, origin = '1970-1-1', 
                                        tz = 'UTC'), format = '%Y-%m-%d %H:%M', 
                             tz = 'UTC', usetz = FALSE), tz = timez))
}



test_matlab$time %>% matlab2POS() %>% summary()

  # OK, well that all seems reasonable now (1901-1910; June-December; latter seems a bit late but not impossible)
  # Where was I getting 1851 from then?

  # weird--for some reason need to reinstall; maybe non-CRAN pacakges don't carry over w/ updates?
devtools::install_github("geanders/stormwindmodel", build_vignettes = TRUE)

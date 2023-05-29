# reading in .netcdf4 files from CLIMADA
  # generates a single file per storm; think I remember that's the opposite issue we had

# trying to read in netcdf

library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
library(tidyverse)

# just want to subset it, so hopefully nothing too involved

test_output_file <- paste0("../../../Admin/climada_python/output/2019141N28291.nc") # just one for now
# maybe some way to iterate for all ~dozen or so files
  # think I remember there's a way to merge files in commandline; not sure of pros/cons
  
# example on https://stackoverflow.com/questions/19654502/opening-and-reading-multiple-netcdf-files-with-rnetcdf
  # how to read in multiple single files in loop, extract relevant variables, merge  into a df
  # so here, what are the relevant datapoints?
  
  # latitude and longitude
  # windspeed <0- max sustained? Review inputs here
  # "season" indicator <-- output files have year as first 4 characters
  # some storm ID <- also included in filename; not positive what the parts mean
    # has "_gen#" suffix for each storm
  
# OK, reviewing `get_grid_winds` inputs

?get_grid_winds()

# Pulling in example read-in
  # Also separately generated a storm id column; could use filenames here
  # yeah, so I think do just need some ID (maybe just a convenience), 

# 

  # ok, this is the downstream input from parallel processing to get windspeeds

#storm_winds_25yr3h_grid_comb %>%
#  group_by(gridid) %>%
#  dplyr::summarize(mean_windspeed = mean(vmax_sust)) %>%
#  left_join(us_counties, by = c("gridid" = 'GEOID')) %>%
#  ggplot() +
#  aes(fill = mean_windspeed, geometry = geometry) +
#  geom_sf() +
#  theme_minimal() + 
#  scale_fill_viridis_c(option = 'magma') +
#  labs(fill = "Windspeed (m/s)",
#       title = "Mean Sustained Windspeed (m/s) in 25y Simulation")

#OK, inputs are:
  # storm_id [think this is arbitrary], date/time (specific format), latitude, longitude, wind
  # think I can get these all without too much trouble
###########

test_climada_nc <- nc_open(test_output_file)
# well, that worked at any rate






nc_close(test_output_file)
#!!: Apparently *not* closing it after nc_open 'risks data loss' [hopefully not in ~parent file!]

test_climada_nc %>% str()
# can review variables
  #time_step, radius_max_wind, radius_oci, max_sustained_wind, central pressure, 
  # environmental pressure, basin, lon, lat, on_land, distance_since_lf

  # those are the "selectable" ones, but looks like there are quite a few more 

  # ** Notably, no analog to "season", so would have to add or ~infer from file/context
    # so that may be nonstarter, since we're wanting to consider seasons, vs single storms 

# quick exploratory plot
ggplot() +
  aes(x = ncvar_get(test_climada_nc, "lat"), y = ncvar_get(test_climada_nc, "lon"), 
      color = ncvar_get(test_climada_nc, "max_sustained_wind"), 
      shape = as.factor(ncvar_get(test_climada_nc, "on_land"))) +
  geom_point() +
  scale_color_viridis_c(option = "turbo") +
  theme_classic()

######################################33

# Trying as a loop through all 400(?) files

climada_netcdf4_files <- list.files("../../../Admin/climada_python/output/", pattern = "*.nc",
                                    full.names = TRUE)

# still trying to extract date information
  # I guess could get year/date from title, add timesteps?
    # Yeah, they have [year:4][month:1-2][day:1-2] <- there's apparently a day 0?

# at least can *plot* for now to get a sense of spatial distribution

latitude <- c()
longitude <- c()
storm_id <- c()
windspeed <- c()

for(i in seq_along(climada_netcdf4_files)) {
  nc = nc_open(climada_netcdf4_files[i])
 # latitude, longitude, and wind are easy to get
  lat =  ncvar_get(nc,'lat') #var.get.nc(nc,'lat')
  lon = ncvar_get(nc,'lon') #var.get.nc(nc,'lon')
  ws = ncvar_get(nc, "max_sustained_wind")
  id <- rep(i, length(lat))
  
  latitude <- c(latitude, lat)
  longitude <- c(longitude, lon)
  storm_id <- c(storm_id, id)
  windspeed <- c(windspeed, ws)
  
  #windsp
  # wait--I need specific times/dates for the model; trying to think through that
    # so there are `timesteps` in 1-hr increments
    # trying to find where/if they're stored, though
}

# arbitrary IDs 

climada_df <- data.frame(
  storm_id,
  latitude,
  longitude,
  windspeed
)

climada_df %>% ggplot() +
  aes(y = latitude,
      x = longitude,
      color = windspeed,
      group = storm_id) +
  geom_path() +
  theme_classic() +
  scale_color_viridis_c(option = "turbo")

# seems to be stored as simple integers 1-?; presumably years since 1840?

# quick test for plotting

ncvar_get(ibtracs_all_nc, "season")
# oh, here it's presented ~naturally

# now, how to subset by year?
# should be able to use something like `which`, but not sure where to fit in w/o bausing misalignment



############## years by status (per Alex's notes)



##################

# here's a loose analog
# I guess see if this work? looks like it's focusing on one value

#tas_time <- nc.get.time.series(ibtracs_all_nc, v = "TREFHT", time.dim.name = "date_time")
tas_time <- nc.get.time.series(ibtracs_all_nc, v = "season", time.dim.name = "time")

time_index <- which(format(tas_time, "%Y-%m-%d") == "2002-09-16")
# not sure what this operation is accomplishing; seems like it's picking this specific date in 2075?
# substituting arbitrary date 

# nc.get.var.subset.by.axes(climate_output, "tas",
#axis.indices = list(T = time_index))

nc.get.var.subset.by.axes(ibtracs_all_nc, "storm", axis.indices = 988)



################333

# Still not making much headway here.... 
# 'NCO Tools' looks like a command line ~toolset for working with netcdf data; still looks like time might be a begin:end deal?
# Oh, here's something that looks promising! ::

# install.packages('ncdf.tools') # Argh, looks like it's not on CRAN; presumably a personal github then...
# no, does show up as being on CRAN; let's try again...
# Huh, so it was removed from CRAN in summer 2020 when requests for update weren't replied to
# I wonder if there are any newer analogs and/or an archived version?

#install.packages("ncdf.tools")
#library(ncdf.tools)


# OK, sounds like that is largely wrappers for RNetCDF pacakge functions, so let's try that?

#install.packages("RNetCDF")
library(RNetCDF)


###################3333

# OK, on the NCO side, was able to if nothing else, install and get command to run w/o error; I used

## ncks -d date_time,0,1 tcwise/TCWiSE_toolbox_v1.0.79/02_application/data/IBTrACS.ALL.v04r00_download260520.nc test.nc
# I'm assuming that's by index if it works; ideally can get to the `season` parameter?
#Let's see where that gets us....
# trying to figure out if there's actually any less data...

test_nc_NCO <-nc_open("test.nc")
# OK, seems to have same ~metadata, at least

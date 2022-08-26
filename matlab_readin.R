# attempting loading matlab data

#install.packages("R.matlab")
library(R.matlab)

#test_matlab <- readMat("TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/ibtracs.NA_since-9999_dx1.mat")
test_matlab <- readMat("TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/simulated.NA10years_dx1.mat")

test_matlab 
#trying to figure out structure; some is self-evident, but not sure about others (e.g. 'time' units, 'tgenesis')
  
  # also trying to work out how to ~process numerically
    #because it's set up as a list; some elemets have continuous dimensions, but not all

## Plotting

library(ggplot2)

ggplot() + aes(x = test_matlab$lon, y = test_matlab$lat, color = test_matlab$vmax) + geom_point() + scale_color_viridis_c()


test_matlab_historical <- readMat("TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/ibtracs.NA_since-9999_dx1.mat")

ggplot() + aes(x = test_matlab_historical$lon, y = test_matlab_historical$lat,
               color = test_matlab_historical$vmax) +
  geom_point() + scale_color_viridis_c()

# could try integrating counties next (check coordinate agreement)


### Update: Finally got simulations working; seeing how format differs:

test_matlab_tc_001 <- readMat("TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/spw_simulated/TC_0001/tc_input.mat")
  # not sure what a .spw file is; hopefully not ~essential for analysis?

test_matlab_tc_001
  # huh, pretty complex arrangement

test_matlab_tc_001$trackinput

attr(test_matlab_tc_001$trackinput, 'track')

    # wondering if this is input data as the name suggests

# Looks like there might be some higher-level summary/output data?

test_matlab_tc_001 <- readMat("TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/spw_simulated/TC_0001/tc_input.mat")



## changes:

# try to change number of storms/years to simulate (e.g. drop to ~20-30 to allow faster runs while experimenting)
# try modifying seed years (El Nino, etc.) 
  # here do want to use full list (may need a minimum number of seed years)

############ Ok, trying to read in matlab output from newest simulation

  #TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/spw_simulated/TC_0001/tc_input.mat

test_matlab_cropped_years <- readMat("TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/simulated.NA10years_dx1.mat")

test_matlab_cropped_years <- readMat("/Users/Admin/Documents/hurricane_project/TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/maps_simulated.NA10years_dx1_test1.mat")


test_matlab_cropped_pdf <- readMat("TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/maps_simulated.NA10years_dx1_test1.mat")

test_matlab_cropped_pdf <- readMat("TCWiSE_toolbox_v1.0.79/TCWiSE_toolbox_v1.0.79/03_output/maps_simulated.NA10years_dx1.mat")

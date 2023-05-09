# Trying to set up ~framework model for wind -> []-> exccess mortality function...


# Just for fun, seeing if I can read in graph

#install.packages("SCVA")
#library(SCVA)

  # older package--anything more recent?

  # following https://www.r-bloggers.com/2019/06/extracting-the-data-from-static-images-of-graphs-with-magick/

#install.packages("magick")
library(magick)

wind_mort_test_plot <- image_read("neterhey_et_al_2020_ws_mort_plot.png")

# saturation data
wind_mort_test_plot_proc <- wind_mort_test_plot %>%
  image_channel("saturation")

wind_mort_test_plot_proc

# threshold 
  # this is to get rid of grey values 

wind_mort_test_plot_proc_thresh <- wind_mort_test_plot_proc %>%
  image_threshold("white", "30%")

wind_mort_test_plot_proc_thresh

# Oh, I see where this is going -- might need to crop to just the curve itself?
  # Also would have/want to remove the red baseline



#####



# If coordinates line up, `image_average()` might be useful for NDVI data


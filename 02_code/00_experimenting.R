install.packages('hurricaneexposure')
#install.packages('hurricaneexposuredata') # CRAN isn't finding this 
install.packages('hurricaneexposuredata',
                 repos='https://geanders.github.io/drat/', type='source')

install.packages('stormwindmodel')

library(hurricaneexposure)
library(hurricaneexposuredata)
library(stormwindmodel)

#########333

hurricaneexposuredata::ext_tracks_wind
stormwindmodel::floyd_tracks
stormwindmodel::county_points %>% ggplot() +
  aes(y = glat, x = glon) +
  geom_point()

hurr_tracks %>%
  ggplot() +
  aes(y = latitude, x = longitude, group = storm_id, alpha = wind) +
  geom_path(arrow = arrow(angle = 15))

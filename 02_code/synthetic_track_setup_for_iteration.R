# stormwinds test setup

# Remember that (as far as I can tell) it uses *knots* as the input instead of m/s
  # so probably need to convert back

# Goal being to run on SUMMIT in parallel 

library(tidyverse)
library(stormwindmodel)

# per internet, 1 m/s = 1.94384449 knots

synth_track_sample <- read_csv("STORM_DATA_IBTRACS_NA_5_YEARS_0.txt",
                               col_names = FALSE) %>%
  select(c(1, 2, 3,4,6,7,9)) %>%
  rename("year" = 1,
          "month" = 2,
          "storm_id" = 3,
         'timestep' = 4,
         'latitude' = 5,
         'longitude' = 6,
         'wind' = 7) %>%
    mutate(year = year + 1,
           hours_base = timestep * 3,
           hour = hours_base %% 24,
           day = ((hours_base - hour) / 24) + 1,
           wind = wind * 1.94384449) %>% # converting to knots
  mutate(date = 
           paste0(str_pad(year, width = 4, side = "left", pad = 0),
                  str_pad(month, width = 2, side = "left", pad = 0),
                  str_pad(day, width = 2, side = "left", pad = 0),
                  str_pad(hour, width = 2, side = "left", pad = 0),
                  "00"
         
         )) %>%
  select(storm_id, date, latitude, longitude, wind)

# Trying to think through most efficient setup
  # Each operation has to be on a single-storm system

  # so either generate a lot of files (one for each track) and work through
  # or do one reference object and some kind of parameter for selecting

synth_track_sample %>% 
  group_by(storm_id) %>%
  nest()
  # Let's try w/ shortest track?

synth_track_sample %>%
  filter(storm_id == 14) %>%
  select(-storm_id) %>%
  get_grid_winds(tint = 3) # using default county centers

# OK, that seems to work (at least mechanically)
  # AND, importantly doesn't need to be a separate file (not sure why I was assuming that)

# So, options:
  # .) I think stick with one import file (unless it's *huge* or generated continuously)
  # .) Reminder that I want to run *separate* processes, not just a loop, though
  # So, either have a nested import object (and point to storms 1-n in each pass)
    # or a single, simple column and filter to storm `n` as a preliminary step
    # I don't think it should matter speed-wise (again, unless it's a huge file)
      # So let's stick w/ filtering for more generalizability
  #But do need an initial list of storm ids up front (not just looping through)
    # need a set of ~procedurally-generated files

# So it would look like
  #a) have list of storm ids on hand
  #b) 


  # I think the use of knots in the example is just a sdemonstration of how you can convert
    # I'd leave as=is and double-check that the units make sense.

# OK, get_grid_winds works *per strorm*, so definitely need to split by storm id anyway
  # also, requires a date-time format (year, month, day, hour, minute)
    # So can use placeholders for year-month, and covert hours to days?

#e.g. `100` = 300hr = 12.5 days
  # In this context



# So, I think the simplest approach is:



#### [Reset `i` assignment programatically]
  # IF it's one long list of storms; otherwise add a second term to track scenario
    # might be a good idea anyway

#i <- #[NUMBER]

track_i <- synth_track_sample %>%
  filter(storm_id == i) %>%
  select(-storm_id) %>%
  get_grid_winds(tint = 3) # double-check that this is right interpretation




## Testing with 
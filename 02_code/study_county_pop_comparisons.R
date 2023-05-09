library(tidyverse)
library(tidycensus)
library(tigris)
library(kableExtra)
library(knitr)

# Load vector with FIPS for all Medicare counties
all_counties <- readRDS("Epidemiology_revision/additional_analyses/data/med_all_counties_fips.rds")
length(all_counties) # There are 222 counties total in the original data

# Load dataframe with information for each county in the eastern US
study_counties <- readRDS("Epidemiology_revision/additional_analyses/data/med_eastern_counties.rds")
nrow(study_counties) # There are 180 counties in the eastern states we consider study states

# List study states and get two-digit FIPS code for each (to later match with census
# data)
study_states <- tibble(state = c("Alabama", "Arkansas", "Connecticut", "Delaware",
                                 "District of Columbia", "Florida", "Georgia",
                                 "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                                 "Louisiana", "Maine", "Maryland", "Massachusetts",
                                 "Michigan", "Mississippi", "Missouri",
                                 "New Hampshire", "New Jersey", "New York", "North Carolina",
                                 "Ohio", "Oklahoma", "Pennsylvania", "Rhode Island",
                                 "South Carolina", "Tennessee", "Texas", "Vermont",
                                 "Virginia", "West Virginia", "Wisconsin"))
nrow(study_states) # There are 34 study states / districts (those in the eastern part of the US, with 
# some chance to be affected by Atlantic basin tropical cyclones)

# Read in dataframe with state names, FIPS codes, and abbreviations
state_fips <- read_csv("Epidemiology_revision/additional_analyses/data/us-state-ansi-fips.csv")
study_state_fips <- state_fips %>% 
  semi_join(study_states, by = c("stname" = "state"))

## Make a list of variable codes for Census variables

var_labels_2000 <- tribble(
  ~ id, ~ description, 
  "P001001", "total_pop", #
  "P005002", "urban_pop", 
  "P005005", "rural_pop",
  "P003003", "white_pop", #
  "P003004", "black_pop", #
  "P004002", "hispanic_pop", #
  "P004003", "non_hispanic_pop", #
  "P017001", "ave_household_size", 
  "P013001", "median_age", #
  "P012020", "age_sub_65_66_male", #
  "P012021", "age_sub_67_69_male", #
  "P012022", "age_sub_70_74_male", #
  "P012023", "age_sub_75_79_male", #
  "P012024", "age_sub_80_84_male", #
  "P012025", "age_sub_85_p_male", #
  "P012044", "age_sub_65_66_female", #
  "P012045", "age_sub_67_69_female", #
  "P012046", "age_sub_70_74_female", #
  "P012047", "age_sub_75_79_female", #
  "P012048", "age_sub_80_84_female", #
  "P012049", "age_sub_85_p_female" #
)

var_labels_2010 <- tribble(
  ~ id, ~ description, 
  "P001001", "total_pop",
  "P002002", "urban_pop", 
  "P002005", "rural_pop",
  "P003002", "white_pop", 
  "P003003", "black_pop",
  "P004003", "hispanic_pop",
  "P004002", "non_hispanic_pop",
  "P017001", "ave_household_size", 
  "P013001", "median_age", 
  "P012020", "age_sub_65_66_male", 
  "P012021", "age_sub_67_69_male", 
  "P012022", "age_sub_70_74_male", 
  "P012023", "age_sub_75_79_male", 
  "P012024", "age_sub_80_84_male", 
  "P012025", "age_sub_85_p_male", 
  "P012044", "age_sub_65_66_female", 
  "P012045", "age_sub_67_69_female", 
  "P012046", "age_sub_70_74_female", 
  "P012047", "age_sub_75_79_female", 
  "P012048", "age_sub_80_84_female", 
  "P012049", "age_sub_85_p_female"
)

## Get demographic data from the two decennial censuses (2000 and 2010)
from_dec_census_2000 <- get_decennial(geography = "county", year = 2000, 
                                      variable = c("P001001", 
                                                   "P003003", "P003004",
                                                   "P004003", "P004002", "P017001", 
                                                   paste0("P01204", c("4", "5", "6", "7", "8", "9")),
                                                   paste0("P01202", c("0", "1", "2", "3", "4", "5")),
                                                   "P013001")) %>% 
  left_join(var_labels_2000, by = c("variable" = "id")) %>% 
  select(-variable) %>% 
  pivot_wider(names_from = description, values_from = value) %>% 
  mutate(over_65_pop = age_sub_65_66_female + age_sub_67_69_female + 
           age_sub_70_74_female + age_sub_75_79_female + age_sub_80_84_female + 
           age_sub_85_p_female + age_sub_65_66_male + age_sub_67_69_male + 
           age_sub_70_74_male + age_sub_75_79_male + age_sub_80_84_male + 
           age_sub_85_p_male) %>% 
  select(GEOID:ave_household_size, median_age:over_65_pop) %>% 
  mutate(state_fips = str_sub(GEOID, 1, 2)) %>% 
  right_join(study_state_fips, by = c("state_fips" = "st")) %>%
  mutate(in_study = if_else(GEOID %in% study_counties$fips, 
                            "study_counties", "other_counties"),
         in_study = fct_relevel(in_study, "study_counties"))


from_dec_census_2010 <- get_decennial(geography = "county", year = 2010, 
                                      variable = c("P001001", 
                                                   "P003002", "P003003",
                                                   "P004002", "P004003", "P017001", 
                                                   paste0("P01204", c("4", "5", "6", "7", "8", "9")),
                                                   paste0("P01202", c("0", "1", "2", "3", "4", "5")),
                                                   "P013001")) %>% 
  left_join(var_labels_2010, by = c("variable" = "id")) %>% 
  select(-variable) %>% 
  pivot_wider(names_from = description, values_from = value) %>% 
  mutate(over_65_pop = age_sub_65_66_female + age_sub_67_69_female + 
           age_sub_70_74_female + age_sub_75_79_female + age_sub_80_84_female + 
           age_sub_85_p_female + age_sub_65_66_male + age_sub_67_69_male + 
           age_sub_70_74_male + age_sub_75_79_male + age_sub_80_84_male + 
           age_sub_85_p_male) %>% 
  select(GEOID:ave_household_size, median_age:over_65_pop) %>% 
  mutate(state_fips = str_sub(GEOID, 1, 2)) %>% 
  right_join(study_state_fips, by = c("state_fips" = "st")) %>%
  mutate(in_study = if_else(GEOID %in% study_counties$fips, 
                            "study_counties", "other_counties"),
         in_study = fct_relevel(in_study, "study_counties"))

## Make table for population sizes

for_tab_2000 <- from_dec_census_2000 %>% 
  group_by(in_study) %>% 
  summarize(n = n(), 
            total_pop = sum(total_pop),
            white_pop = sum(white_pop), 
            black_pop = sum(black_pop), 
            non_hispanic_pop = sum(non_hispanic_pop), 
            hispanic_pop = sum(hispanic_pop),
            older_pop = sum(over_65_pop),
            other_race = total_pop - (white_pop + black_pop),
            younger_pop = total_pop - older_pop) %>% 
  select(in_study, total_pop, 
         white_pop, black_pop, other_race, 
         hispanic_pop, non_hispanic_pop, 
         younger_pop, older_pop) %>%  
  pivot_longer(cols = total_pop:older_pop, names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = in_study, values_from = value) %>% 
  mutate(total = study_counties + other_counties) %>% 
  mutate_at(vars(2:4), ~ prettyNum(., big.mark = ","))

for_tab_2010 <- from_dec_census_2010 %>% 
  group_by(in_study) %>% 
  summarize(n = n(), 
            total_pop = sum(total_pop),
            white_pop = sum(white_pop), 
            black_pop = sum(black_pop), 
            non_hispanic_pop = sum(non_hispanic_pop), 
            hispanic_pop = sum(hispanic_pop),
            older_pop = sum(over_65_pop),
            other_race = total_pop - (white_pop + black_pop),
            younger_pop = total_pop - older_pop) %>% 
  select(in_study, total_pop, 
         white_pop, black_pop, other_race, 
         hispanic_pop, non_hispanic_pop, 
         younger_pop, older_pop) %>% 
  pivot_longer(cols = total_pop:older_pop, names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = in_study, values_from = value) %>% 
  mutate(total = study_counties + other_counties) %>% 
  mutate_at(vars(2:4), ~ prettyNum(., big.mark = ","))

for_tab_total <- for_tab_2000 %>% 
  bind_cols(for_tab_2010 %>% select(-1)) %>% 
  mutate(variable = c("Total population", 
                      "White", "Black/African-American", "Other", 
                      "Hispanic or Latino", "Not Hispanic or Latino", 
                      "Under 65 years", "65 years and over"))

# Write out table to file
pop_tab <- for_tab_total %>% 
  kable(format = "latex", booktabs = TRUE, 
        align = "lcccccc",
        escape = FALSE, 
        col.names = linebreak(c("", 
                                c("Study\ncounties\n(N = 180)", 
                                          paste0("Other counties\nin study states", 
                                                 footnote_marker_symbol(1, "latex"),
                                                 "\n(N = 2,217)"), 
                                          "Total in\ncounties in\nstudy states"),
                                c("Study\ncounties\n(N = 180)", 
                                      "Other counties\nin study states\n(N = 2,216)", 
                                      "Total in\ncounties in\nstudy states")
                                ), 
                              align = "c"), 
        caption = "Reproduction of Table S1 in the Supplemental Appendix of the revised
        manuscript. Caption from the text: \\textit{``\\textbf{Table S1:} Comparison of population sizes of counties included in this study
        compared to other counties in the 34 eastern US states/districts shown in 
        Figure 1 of the main text. Population sizes are given for both the overall
        population and for specific subpopulations by race, ethnicity, and age. 
        Population sizes are summed across all counties in the given group of counties.
        Values are based on the 2000 and 2010 US Decennial Censuses.''}", 
        label = "countypopcompare") %>% 
  kable_styling(latex_options = "hold_position") %>% 
  pack_rows("Overall", 1, 1) %>% 
  pack_rows("By race", 2, 4) %>% 
  pack_rows("By ethnicity", 5, 6) %>% 
  pack_rows("By age", 7, 8) %>% 
  add_header_above(c(" ", "2000 Census" = 3, "2010 Census" = 3)) %>% 
  landscape() %>% 
  footnote(
    symbol = "The 34 eastern US states/districts covered in this study are shown in Figure 1 of the main text.",
    title_format = c("italic")
  )

fileConn <- file("Epidemiology_revision/additional_analyses/tables/population.tex")
writeLines(pop_tab, fileConn)
close(fileConn)


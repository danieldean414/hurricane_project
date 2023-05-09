library(tidyverse)
library(tidycensus)
library(kableExtra)

study_counties <- readRDS("Epidemiology_revision/additional_analyses/data/med_eastern_counties.rds")
study_states <- tibble(state = c("Alabama", "Arkansas", "Connecticut", "Delaware",
                                 "District of Columbia", "Florida", "Georgia",
                                 "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                                 "Louisiana", "Maine", "Maryland", "Massachusetts",
                                 "Michigan", "Mississippi", "Missouri",
                                 "New Hampshire", "New Jersey", "New York", "North Carolina",
                                 "Ohio", "Oklahoma", "Pennsylvania", "Rhode Island",
                                 "South Carolina", "Tennessee", "Texas", "Vermont",
                                 "Virginia", "West Virginia", "Wisconsin"))
state_fips <- read_csv("Epidemiology_revision/additional_analyses/data/us-state-ansi-fips.csv")
study_state_fips <- state_fips %>% 
  semi_join(study_states, by = c("stname" = "state"))

ex_vars <- tribble(
  ~ variable, ~ var_name,
  "B01003_001", "Total population", 
  "B19013_001", "Median household income in the past 12 months",
  "B17001_002", "Pop below poverty level", 
  "B17001_001", "Denom for poverty level",
  
  "B17001_015", "M 65 to 74 bpl",
  "B17001_016", "M 75p bpl",
  "B17001_029", "F 65 to 74 bpl",
  "B17001_030", "F 75p bpl",
  "B17001_044", "M 65 to 74 apl",
  "B17001_045", "M 75p apl",
  "B17001_058", "F 65 to 74 apl",
  "B17001_059", "F 75p apl",
  
  "B01002_001", "Median age", 
  "B01001_020", "Male 65 to 66", 
  "B01001_021", "Male 67 to 69", 
  "B01001_022", "Male 70 to 74", 
  "B01001_023", "Male 75 to 79", 
  "B01001_024", "Male 80 to 84", 
  "B01001_025", "Male 85p",
  "B01001_044", "Female 65 to 66", 
  "B01001_045", "Female 67 to 69", 
  "B01001_046", "Female 70 to 74", 
  "B01001_047", "Female 75 to 79", 
  "B01001_048", "Female 80 to 84", 
  "B01001_049", "Female 85p", 
  "B01001_001", "Denom sex by age",
  "B06007_002", "Speak only English at home", 
  "B06007_001", "Denom speak only English", 
  "B01001A_001", "Pop white alone", 
  "B01001B_001", "Pop black alone", 
  "B01001I_001", "Pop hispanic", 
  "B18106_004", "M 5 to 17 scd",
  "B18106_007", "M 18 to 34 scd", 
  "B18106_010", "M 35 to 64 scd", 
  "B18106_013", "M 65 to 74 scd", 
  "B18106_016", "M 75 p scd", 
  "B18106_020", "F 5 to 17 scd",
  "B18106_023", "F 18 to 34 scd", 
  "B18106_026", "F 35 to 64 scd", 
  "B18106_029", "F 65 to 74 scd", 
  "B18106_032", "F 75 p scd", 
  "B18106_001", "Denom self-care difficulty", 
  "B18106_012", "Denom scd M 65 to 74", 
  "B18106_015", "Denom scd M 75p",
  "B18106_028", "Denom scd F 65 to 74", 
  "B18106_031", "Denom scd F 75p",
  "B18105_004", "M 5 to 17 ad",
  "B18105_007", "M 18 to 34 ad", 
  "B18105_010", "M 35 to 64 ad", 
  "B18105_013", "M 65 to 74 ad", 
  "B18105_016", "M 75 p ad", 
  "B18105_020", "F 5 to 17 ad",
  "B18105_023", "F 18 to 34 ad", 
  "B18105_026", "F 35 to 64 ad", 
  "B18105_029", "F 65 to 74 ad", 
  "B18105_032", "F 75 p ad", 
  "B18105_001", "Denom ambulatory difficulty", 
  "B18105_012", "Denom ad M 65 to 74", 
  "B18105_015", "Denom ad M 75p",
  "B18105_028", "Denom ad F 65 to 74", 
  "B18105_031", "Denom ad F 75p",
  "B18107_004", "M 18 to 34 id", 
  "B18107_007", "M 35 to 64 id", 
  "B18107_010", "M 65 to 74 id", 
  "B18107_013", "M 75 p id", 
  "B18107_017", "F 18 to 34 id", 
  "B18107_020", "F 35 to 64 id", 
  "B18107_023", "F 65 to 74 id", 
  "B18107_026", "F 75 p id", 
  "B18107_001", "Denom independent living difficulty", 
  "B18107_009", "Denom id M 65 to 74", 
  "B18107_012", "Denom id M 75p",
  "B18107_022", "Denom id F 65 to 74", 
  "B18107_025", "Denom id F 75p",
  "B18101_001", "Denom disability",
  "B18101_004", "M u5 d", 
  "B18101_007", "M 5 to 17 d",
  "B18101_010", "M 18 to 34 d",
  "B18101_013", "M 35 to 64 d", 
  "B18101_016", "M 65 to 74 d",
  "B18101_019", "M 75 p d",
  "B18101_023", "F u5 d", 
  "B18101_026", "F 5 to 17 d",
  "B18101_029", "F 18 to 34 d",
  "B18101_032", "F 35 to 64 d", 
  "B18101_035", "F 65 to 74 d",
  "B18101_038", "F 75 p d",
  "B18101_015", "Denom d M 65 to 74", 
  "B18101_018", "Denom d M 75p", 
  "B18101_034", "Denom d F 65 to 74", 
  "B18101_037", "Denom d F 75p", 
  "B19058_002", "with pa / snap", 
  "B19058_001", "Denom pa / snap"
)

ex_acs <- get_acs(geography = "county", 
                  variable = ex_vars$variable, 
                  year = 2012, # ACS for 2008--2012, centered on 2010
                  survey = "acs5") %>% 
  select(-moe) %>% 
  left_join(ex_vars, by = "variable") 

tab_3 <- ex_acs %>% 
  select(-variable) %>% 
  pivot_wider(names_from = var_name, values_from = estimate) %>% 
  # Poverty / Income related
  mutate(`Percent below poverty level` = 100 * `Pop below poverty level` / `Denom for poverty level`) %>% 
  select(-c(`Pop below poverty level`, `Denom for poverty level`)) %>% 
  mutate(`Percent 65 years and over below poverty level` = 100 * 
           (`M 65 to 74 bpl` + `M 75p bpl` + `F 65 to 74 bpl` + `F 75p bpl`) / 
           (`M 65 to 74 bpl` + `M 75p bpl` + `F 65 to 74 bpl` + `F 75p bpl` + 
              `M 65 to 74 apl` + `M 75p apl` + `F 65 to 74 apl` + `F 75p apl`)) %>% 
  select(-c(`M 65 to 74 bpl`, `M 75p bpl`, `F 65 to 74 bpl`, `F 75p bpl`, 
            `M 65 to 74 apl`, `M 75p apl`, `F 65 to 74 apl`, `F 75p apl`)) %>% 
  mutate(`Percent of households receiving public assistance incomes of food stamps / SNAP` = 
           100 * `with pa / snap` / `Denom pa / snap`) %>% 
  select(-c(`with pa / snap`, `Denom pa / snap`)) %>% 
  #
  mutate(`Population 65 years and over` = `Male 65 to 66` + `Male 67 to 69` + 
           `Male 70 to 74` + `Male 75 to 79` + `Male 80 to 84` + `Male 85p` + 
           `Female 65 to 66` + `Female 67 to 69` + `Female 70 to 74` + 
           `Female 75 to 79` + `Female 80 to 84` + `Female 85p`, 
         `Percent 65 years and over` = 100 * `Population 65 years and over` / 
           `Denom sex by age`) %>% 
  select(-c(`Male 65 to 66`, `Male 67 to 69`, `Male 70 to 74`, `Male 75 to 79`, `Male 80 to 84`,
            `Male 85p`, `Female 65 to 66`, `Female 67 to 69`, `Female 70 to 74`, 
            `Female 75 to 79`, `Female 80 to 84`, `Female 85p`, `Denom sex by age`)) %>%
  mutate(`Percent language other than English spoken at home` = 100 * (1 - `Speak only English at home` / 
           `Denom speak only English`)) %>% 
  select(-c(`Speak only English at home`, `Denom speak only English`)) %>% 
  mutate(`Percent White alone` = 100 * `Pop white alone` / `Total population`) %>% 
  select(- `Pop white alone`) %>% 
  mutate(`Percent Black / African American alone` = 100 * `Pop black alone` / `Total population`) %>% 
  select(- `Pop black alone`) %>% 
  mutate(`Percent Hispanic or Latino` = 100 * `Pop hispanic` / `Total population`) %>% 
  select(- `Pop hispanic`) %>% 
  
  mutate(`Percent with disability` = 100 * (`M u5 d` + `M 5 to 17 d` + `M 18 to 34 d` + 
                                                        `M 35 to 64 d` + `M 65 to 74 d` + 
                                                        `M 75 p d` + `F u5 d` +
                                                        `F 5 to 17 d` + `F 18 to 34 d` + 
                                                        `F 35 to 64 d` + `F 65 to 74 d` + 
                                                        `F 75 p d`) / 
           `Denom disability`) %>% 
  mutate(`Percent 65 years and over with disability` = 
           100 * (`M 65 to 74 d` + `M 75 p d` + `F 65 to 74 d` + `F 75 p d`) / 
           (`Denom d M 65 to 74` + `Denom d M 75p` + `Denom d F 65 to 74` + 
              `Denom d F 75p`)) %>% 
  select(-c(`M u5 d`, `M 5 to 17 d`, `M 18 to 34 d`, `M 35 to 64 d`, `M 65 to 74 d`, 
            `M 75 p d`, `F u5 d`, `F 5 to 17 d`, `F 18 to 34 d`, `F 35 to 64 d`,
            `F 65 to 74 d`, `F 75 p d`, `Denom disability`,
            `Denom d M 65 to 74`, `Denom d M 75p`, `Denom d F 65 to 74`, 
            `Denom d F 75p`)) %>% 
  mutate(`Percent with self-care difficulty` = 100 * (`M 5 to 17 scd` + `M 18 to 34 scd` + 
                                                        `M 35 to 64 scd` + `M 65 to 74 scd` + 
                                                        `M 75 p scd` + 
                                                        `F 5 to 17 scd` + `F 18 to 34 scd` + 
                                                        `F 35 to 64 scd` + `F 65 to 74 scd` + 
                                                        `F 75 p scd`) / 
           `Denom self-care difficulty`) %>% 
  mutate(`Percent 65 years and over with self-care difficulty` = 
           100 * (`M 65 to 74 scd` + `M 75 p scd` + `F 65 to 74 scd` + `F 75 p scd`) / 
           (`Denom scd M 65 to 74` + `Denom scd M 75p` + `Denom scd F 65 to 74` + 
              `Denom scd F 75p`)) %>% 
  select(-c(`M 5 to 17 scd`, `M 18 to 34 scd`, `M 35 to 64 scd`, `M 65 to 74 scd`, 
              `M 75 p scd`, `F 5 to 17 scd`, `F 18 to 34 scd`, `F 35 to 64 scd`,
            `F 65 to 74 scd`, `F 75 p scd`, `Denom self-care difficulty`,
            `Denom scd M 65 to 74`, `Denom scd M 75p`, `Denom scd F 65 to 74`, 
              `Denom scd F 75p`)) %>% 
  mutate(`Percent with ambulatory difficulty` = 100 * (`M 5 to 17 ad` + `M 18 to 34 ad` + 
                                                        `M 35 to 64 ad` + `M 65 to 74 ad` + 
                                                        `M 75 p ad` + 
                                                        `F 5 to 17 ad` + `F 18 to 34 ad` + 
                                                        `F 35 to 64 ad` + `F 65 to 74 ad` + 
                                                        `F 75 p ad`) / 
           `Denom ambulatory difficulty`) %>% 
  mutate(`Percent 65 years and over with ambulatory difficulty` = 
           100 * (`M 65 to 74 ad` + `M 75 p ad` + `F 65 to 74 ad` + `F 75 p ad`) / 
           (`Denom ad M 65 to 74` + `Denom ad M 75p` + `Denom ad F 65 to 74` + 
              `Denom ad F 75p`)) %>% 
  select(-c(`M 5 to 17 ad`, `M 18 to 34 ad`, `M 35 to 64 ad`, `M 65 to 74 ad`, 
            `M 75 p ad`, `F 5 to 17 ad`, `F 18 to 34 ad`, `F 35 to 64 ad`,
            `F 65 to 74 ad`, `F 75 p ad`, `Denom ambulatory difficulty`,
            `Denom ad M 65 to 74`, `Denom ad M 75p`, `Denom ad F 65 to 74`, 
            `Denom ad F 75p`)) %>% 
  mutate(`Percent with independent living difficulty` = 100 * (`M 18 to 34 id` + 
                                                         `M 35 to 64 id` + `M 65 to 74 id` + 
                                                         `M 75 p id` + 
                                                         `F 18 to 34 id` + 
                                                         `F 35 to 64 id` + `F 65 to 74 id` + 
                                                         `F 75 p id`) / 
           `Denom independent living difficulty`) %>% 
  mutate(`Percent 65 years and over with independent living difficulty` = 
           100 * (`M 65 to 74 id` + `M 75 p id` + `F 65 to 74 id` + `F 75 p id`) / 
           (`Denom id M 65 to 74` + `Denom id M 75p` + `Denom id F 65 to 74` + 
              `Denom id F 75p`)) %>% 
  select(-c(`M 18 to 34 id`, `M 35 to 64 id`, `M 65 to 74 id`, 
            `M 75 p id`, `F 18 to 34 id`, `F 35 to 64 id`,
            `F 65 to 74 id`, `F 75 p id`, `Denom independent living difficulty`,
            `Denom id M 65 to 74`, `Denom id M 75p`, `Denom id F 65 to 74`, 
            `Denom id F 75p`)) %>% 
  pivot_longer(-c(GEOID, NAME)) %>% 
  mutate(state_fips = str_sub(GEOID, 1, 2)) %>% 
  right_join(study_state_fips, by = c("state_fips" = "st")) %>%
  mutate(in_study = if_else(GEOID %in% study_counties$fips, 
                            "study_counties", "other_counties"),
         in_study = fct_relevel(in_study, "study_counties")) %>%
  group_by(name, in_study) %>% 
  summarize(median_value = formatC(median(value), big.mark = ",", 
                                   digits = 1, format = "f"), 
            iqr = paste(formatC(quantile(value, c(0.25, 0.75), na.rm = TRUE), 
                                digits = 1, big.mark = ",", format = "f"), 
                        collapse = ", "),
            full = paste0(median_value, " (", iqr, ")")) %>% 
  select(-c(median_value, iqr)) %>% 
  pivot_wider(names_from = in_study, values_from = full)

order_tab <- tribble(
  ~ name, ~ group, ~ order, 
  
  "Median age", "Age", 18,                                                                     
  "Percent 65 years and over", "Age", 1,                                                     
  "Population 65 years and over", "Age", 19,  
  
  "Percent White alone", "Race, ethnicity, and language", 2,                                                            
  "Percent Black / African American alone", "Race, ethnicity, and language", 3,                                      
  "Percent Hispanic or Latino", "Race, ethnicity, and language", 4,                                                     
  "Percent language other than English spoken at home", "Race, ethnicity, and language", 5,  
  
  "Median household income in the past 12 months", "Income and poverty", 6,                                  
  "Percent below poverty level", "Income and poverty", 7,                                                    
  "Percent 65 years and over below poverty level", "Income and poverty", 8,
  "Percent of households receiving public assistance incomes of food stamps / SNAP", "Income and poverty", 9,
  
  "Percent with disability", "Disability", 11,                                                        
  "Percent with independent living difficulty", "Disability", 12,                                     
  "Percent with ambulatory difficulty", "Disability", 13,                                           
  "Percent with self-care difficulty", "Disability", 14,                                              
  
  "Percent 65 years and over with disability", "Disability", 15,                                      
  "Percent 65 years and over with independent living difficulty", "Disability", 16,                   
  "Percent 65 years and over with ambulatory difficulty", "Disability", 17,                           
  "Percent 65 years and over with self-care difficulty", "Disability", 18,                            
  
  "Total population", "Other", 20,              
)


pop_tab3 <- tab_3 %>% 
  # Re-order for grouping
  left_join(order_tab, by = "name") %>% 
  arrange(order) %>% 
  select(-c(group, order)) %>% 
  filter(name != "Total population") %>% 
  filter(name != "Population 65 years and over") %>% 
  filter(name != "Median household income in the past 12 months") %>% 
  filter(name != "Median age") %>% 
  # Create table
  kable(format = "latex", booktabs = TRUE, 
        align = "lcc",
        escape = FALSE, 
        col.names = linebreak(c("Characteristic", 
                                paste0("Study counties\n(N = 180)", 
                                       footnote_marker_symbol(1, "latex")), 
                                paste0("Other counties\nin study states",
                                       footnote_marker_symbol(2, "latex"),
                                       "\n(N = 2,216)", 
                                       footnote_marker_symbol(1, "latex"))), 
                              align = "c"), 
        caption = "Reproduction of Table S2 in the Supplemental Appendix of the revised
        manuscript. Caption from the text: \\textit{``\\textbf{Table S2:} Comparison of some demographic characteristics of counties included in this study
        compared to other counties in the 34 eastern US states/districts shown in 
        Figure 1 of the main text. Each table cell gives the median value of the county-level 
        estimates of that demographic
        characteristic across all counties in the given group of counties, while shown in 
        parentheses are the interquartile range in the county-level measurements of the 
        characteristic across the group of counties.
        Values are based on the 5-year American Community Survey centered on 2010.''}",
        label = "countydemocompare") %>% 
  kable_styling() %>% 
  
  group_rows(group_label = "Age", start_row = 1, end_row = 1) %>% 
  group_rows(group_label = "Race, ethnicity, and language", start_row = 2, end_row = 5) %>%
  group_rows(group_label = "Income and poverty", start_row = 6, end_row = 8) %>%
  group_rows(group_label = "Disability, all ages", start_row = 9, end_row = 12) %>% 
  group_rows(group_label = "Disability, 65 years and over", start_row = 13, end_row = 16) %>% 
  landscape() %>% 
  footnote(
    symbol = c("Median values of each characteristic are given, with the interquartile range across the counties given in parentheses.",
               "The 34 eastern US states/districts covered in this study are shown in Figure 1 of the main text."),
    title_format = c("italic")
  )

fileConn <- file("Epidemiology_revision/additional_analyses/tables/pop3.tex")
writeLines(pop_tab3, fileConn)
close(fileConn)

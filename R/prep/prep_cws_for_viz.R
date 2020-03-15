source(file.path("R", "packages.R"))
library(jsonlite)

cws_wide <- read_csv(file.path("output_data", "cws", "wide", "cws_2018_all_geos_wide.csv")) %>%
  filter(name %in% c("Connecticut", "Fairfield County", "Greater Hartford", "Greater New Haven", "Valley", "Greater New London", "Greater Waterbury"))

# json structure:
# new_haven: {
#   local_govt_responsive: [
#     
#   ]
# }

group_order <- c("Total", 
                 "Male", "Female", 
                 "Ages 18-34", "Ages 35-49", "Ages 50-64", "Ages 65+", 
                 "White", "Black", "Latino", "Non-white", 
                 "High school or less", "Some college or Associate's", "Bachelor's or higher", 
                 "<$15K", "$15K-$30K", "<$30K", "$30K-$50K", "$50K-$75K", "$30K-$75K", "$75K+", "$75K-$100K", "$100K-$200K", "$200K+")

cws_split <- cws_wide %>%
  mutate_at(vars(name, category, group), as_factor) %>%
  mutate(category = as_factor(category) %>%
           fct_relevel("Total", "Age", "Race/Ethnicity", "Education"),
         group = as_factor(group) %>%
           fct_recode("Non-white" = "Not white") %>%
           fct_relevel(group_order) %>%
           fct_relabel(str_replace, "\\<", "Under ") %>%
           fct_recode("Some college or AA" = "Some college or Associate's")) %>%
  pivot_longer(-name:-group, names_to = "indicator") %>%
  split(.$name) %>%
  map(~split(., .$indicator)) %>%
  map_depth(2, select, -name, -indicator)

saveRDS(cws_split, "output_data/cws/misc/cws_indicators.rds")
# write_json(cws_split, file.path("output_data", "cws", "misc", "cws_indicators.json"), auto_unbox = TRUE)
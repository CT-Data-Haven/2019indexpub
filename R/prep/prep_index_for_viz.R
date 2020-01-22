source(file.path("R", "packages.R"))

idx_path <- file.path("input_data", "indexes")
towns <- cwi::xwalk %>% distinct(town) %>% pull()
regions <- c("Fairfield County", "Greater Hartford", "Greater New Haven")
subregions <- list("Greater New Haven" = c("New Haven Inner Ring", "New Haven Outer Ring"),
                   "Greater Hartford" = c("Hartford Inner Ring", "Hartford Outer Ring"),
                   "Fairfield County" = c("6 wealthiest towns")) %>%
  enframe(name = "region", value = "name") %>%
  unnest(name)

group_order <- c("Total", 
                 "Male", "Female", 
                 "Ages 18-34", "Ages 35-49", "Ages 50-64", "Ages 65+", 
                 "White", "Black", "Latino", "Non-white", 
                 "High school or less", "Some college or Associate's", "Bachelor's or higher", 
                 "<$15K", "$15K-$30K", "$30K-$50K", "$50K-$75K", "$75K-$100K", "$100K-$200K", "$200K+")

region_df <- enframe(cwi::regions, name = "region", value = "name") %>%
  unnest(name) %>%
  filter(region %in% regions) %>%
  bind_rows(tibble(region = c("Connecticut", regions), name = c("Connecticut", regions))) %>%
  bind_rows(subregions)

neighborhood_assets <- read_csv(file.path(idx_path, "assets_index.csv")) %>%
  select(name, category, group, value = composite_scaled) %>%
  mutate(name = str_replace_all(name, "_", " ") %>% 
           str_to_title() %>%
           recode("Valley" = "Lower Naugatuck Valley")) %>%
  filter(name %in% c(regions, "Connecticut") | (name %in% c(towns, subregions$name) & category == "Total")) %>%
  mutate(level = case_when(
    name == "Connecticut" ~ "state",
    name %in% c(regions, subregions$name) ~ "region",
    TRUE ~ "town"
  ))

personal_wellbeing <- read_csv(file.path(idx_path, "personal_wellbeing_index.csv")) %>%
  select(name, category, group, value = composite_scaled) %>%
  mutate(name = str_replace_all(name, "_", " ") %>% 
           str_to_title() %>%
           recode("Valley" = "Lower Naugatuck Valley")) %>%
  filter(name %in% c(regions, "Connecticut") | (name %in% c(towns, subregions$name) & category == "Total")) %>%
  mutate(level = case_when(
    name == "Connecticut" ~ "state",
    name %in% c(regions, subregions$name) ~ "region",
    TRUE ~ "town"
  ))

community <- read_csv(file.path(idx_path, "community_index.csv")) %>%
  select(level, name, value = wbidx) %>%
  filter(str_detect(level, "(state|region|town|county)")) %>%
  mutate(category = "Total", group = "Total") %>%
  mutate(name = str_remove(name, "( town.+|, Connecticut)$")) %>%
  mutate(level = as.factor(level) %>%
           fct_relabel(str_remove, "^\\d+_") %>%
           fct_recode(region = "county") %>%
           fct_drop())

index_df <- lst(neighborhood_assets, personal_wellbeing, community) %>%
  imap(~rename(.x, !!.y := value)) %>%
  reduce(left_join, by = c("level", "name", "category", "group")) %>%
  mutate_if(is.numeric, round) %>%
  mutate(level = as_factor(level) %>%
           fct_relevel("state", "region")) %>%
  mutate(category = as_factor(category) %>%
           fct_relevel("Total", "Age", "Race/Ethnicity", "Education"),
         group = as_factor(group) %>%
           fct_recode("Non-white" = "Not white") %>%
           fct_relevel(group_order) %>%
           fct_relabel(str_replace, "\\<", "Under ") %>%
           fct_recode("Some college or AA" = "Some college or Associate's")) %>%
  arrange(level, name, category, group) %>%
  mutate(l = as.numeric(level)) %>%
  unite(level, l, level) %>%
  select(level, name, everything()) %>%
  left_join(region_df, by = "name") %>%
  filter(category %in% c("Total", "Age", "Race/Ethnicity", "Education", "Income"))

saveRDS(index_df, "output_data/cws/misc/index_scatterplot.rds")
# jsonlite::write_json(index_df, "output_data/cws/misc/index_scatterplot.json")



# index components
personal_wellbeing_comp <- read_csv(file.path(idx_path, "pwi_components.csv")) %>%
  select(-year) %>%
  mutate(name = str_replace_all(name, "_", " ") %>% 
           str_to_title() %>%
           recode("Valley" = "Lower Naugatuck Valley")) %>%
  mutate(category = as_factor(category) %>%
           fct_relevel("Total", "Age", "Race/Ethnicity", "Education"),
         group = as_factor(group) %>%
           fct_recode("Non-white" = "Not white") %>%
           fct_relevel(group_order) %>%
           fct_relabel(str_replace, "\\<", "Under ") %>%
           fct_recode("Some college or AA" = "Some college or Associate's")) %>%
  arrange(name, category, group) %>%
  semi_join(index_df, by = c("name", "category", "group")) %>%
  spread(key = indicator, value) %>%
  left_join(distinct(index_df, level, name, group), by = c("name", "group")) %>%
  left_join(region_df, by = "name") %>%
  mutate_at(vars(category, group), as_factor) %>%
  mutate(level = as.factor(level) %>%
           fct_relabel(str_remove, "^\\d+_")) %>%
  arrange(level, name, category, group) %>%
  select(level, name, everything()) %>%
  filter(level %in% c("state", "region"))



neighborhood_assets_comp <- read_csv(file.path(idx_path, "assets_components.csv")) %>%
  select(-year, -name, -response) %>%
  mutate(proper_name = recode(proper_name, "Valley" = "Lower Naugatuck Valley")) %>%
  rename(name = proper_name) %>%
  mutate(category = as_factor(category) %>%
           fct_relevel("Total", "Age", "Race/Ethnicity", "Education"),
         group = if_else(category == "Total", "Total", group) %>% 
           as_factor() %>%
           fct_recode("Non-white" = "Not white") %>%
           fct_relevel(group_order) %>%
           fct_relabel(str_replace, "\\<", "Under ") %>%
           fct_recode("Some college or AA" = "Some college or Associate's")) %>%
  arrange(name, category, group) %>%
  semi_join(index_df, by = c("name", "category", "group")) %>%
  left_join(distinct(index_df, level, name, group), by = c("name", "group")) %>%
  left_join(region_df, by = "name") %>%
  mutate_at(vars(category, group), as_factor) %>%
  mutate(level = as.factor(level) %>%
           fct_relabel(str_remove, "^\\d+_")) %>%
  arrange(level, name, category, group) %>%
  select(level, name, everything()) %>%
  filter(level %in% c("state", "region")) %>%
  spread(key = indicator, value) %>%
  rename_all(str_replace, "cond", "condition")

community_comp <- read_csv(file.path(idx_path, "community_index.csv")) %>%
  select(-ends_with("f"), -X1, -id, -rank, -wbidx) %>%
  filter(str_detect(level, "(state|region|town|county)")) %>%
  mutate(name = str_remove(name, "( town.+|, Connecticut)$")) %>%
  mutate(level = as.factor(level) %>%
           fct_relabel(str_remove, "^\\d+_") %>%
           fct_recode(region = "county") %>%
           fct_drop()) %>%
  gather(key, value, -level, -name) %>%
  mutate(key = as.factor(key) %>%
           fct_recode(child_poverty = "kidspov", median_household_income = "minc", adults_without_insurance = "noins", housing_cost_burden = "pctburden", adults_with_hs_diploma = "pcths", opportunity_youth = "pctoppyouth", poverty = "povrate", preschool_enrollment = "presch", workers_with_short_commutes = "shortcomm", unemployment = "unemprate", life_expectancy = "wm_exp", youthful_labor_force = "ythforce")) %>%
  spread(key, value) %>%
  mutate(category = "Total", group = "Total") %>%
  semi_join(index_df, by = c("name", "category", "group")) %>%
  left_join(region_df, by = "name") %>%
  arrange(level, name, category, group) %>%
  select(level, name, category, group, everything()) 

comps_df <- lst(personal_wellbeing_comp, neighborhood_assets_comp, community_comp) %>%
  set_names(str_remove, "_comp") %>%
  map(mutate, l = as.numeric(level)) %>%
  map(unite, level, l, level)

saveRDS(comps_df, "output_data/cws/misc/index_components.rds")
# jsonlite::write_json(comps_df, "output_data/cws/misc/index_components.json")


# metadata
indicators <- comps_df %>%
  map(names) %>%
  map(~.[!. %in% c("level", "name", "category", "group", "region")]) %>%
  map_dfr(enframe, value = "indicator", .id = "dataset") %>%
  select(-name)

meta <- list(
  median_household_income = "$0,.0f",
  life_expectancy = "0.1f"
) %>%
  enframe(name = "indicator", value = "format") %>%
  unnest(format) %>%
  full_join(indicators, by = "indicator") %>%
  replace_na(list(format = "0.0%")) %>%
  split(.$dataset) %>%
  map(select, -dataset) %>%
  map(deframe) %>%
  map(as.list)

jsonlite::write_json(meta, "output_data/cws/misc/score_meta.json", auto_unbox = T)
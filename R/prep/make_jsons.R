source(file.path("R", "packages.R"))
library(jsonlite)

in_path <- file.path("output_data", "cws", "misc")

comps <- readRDS(file.path(in_path, "index_components.rds"))
scores <- readRDS(file.path(in_path, "index_scatterplot.rds"))
indics <- readRDS(file.path(in_path, "cws_indicators.rds"))

make_abbr <- function(x) {
  x %>%
    str_match_all("(?:_|\\b)([a-z])") %>%
    map(~.[,2]) %>%
    map_chr(paste, collapse = "")
}

############ scores

write_json(scores, "output_data/cws/misc/index_scatterplot.json")

############ score components

write_json(comps, "output_data/cws/misc/index_components.json")

############ survey indicator bank

# lookup of all indicators across all datasets to make codes
srvy_meta <- googlesheets4::sheets_find("surveyq") %>%
  pull(id) %>%
  googlesheets4::read_sheet() %>%
  mutate(display = coalesce(display, clean_titles(question))) %>%
  mutate(topic = as.factor(topic) %>% fct_relevel("health_risk_factors", "healthcare_access", "reasons_for_missing_healthcare")) %>%
  replace_na(list(denom = "Share of adults")) %>%
  arrange(topic, display) %>%
  group_by(topic) %>%
  mutate(code = make_abbr(topic) %>% paste0(row_number())) %>%
  ungroup()

# list of data nested by region, nested by topic, questions as columns
cws_list <- indics %>%
  map_depth(2, filter, category %in% c("Total", "Age", "Race/Ethnicity", "Education", "Income")) %>%
  map_depth(2, mutate_if, is.factor, fct_drop) %>%
  map_depth(2, arrange, category) %>%
  map(~bind_rows(., .id = "question")) %>%
  map(left_join, srvy_meta %>% select(topic, question), by = "question") %>%
  map(~split(., .$topic)) %>%
  map_depth(2, pivot_wider, names_from = question) %>%
  map_depth(2, select, -topic) %>%
  set_names(str_replace_all, "_", " ") %>%
  set_names(str_to_title)



# cws_list <- indics %>%
#   map_depth(2, filter, category %in% c("Total", "Age", "Race/Ethnicity", "Education", "Income")) %>%
#   map_depth(2, filter, !is.na(value)) %>%
#   # map(function(loc_list) imap(loc_list, ~mutate(.x, question = .y))) %>%
#   # map_depth(2, ~left_join(., srvy_meta %>% select(code, question), by = "question")) %>%
#   # map_depth(2, select, code, category, group, value) %>%
#   set_names(str_replace_all, "_", " ") %>%
#   set_names(str_to_title)

write_json(cws_list, "output_data/cws/misc/cws_indicators.json", na = "null")

# write_json(srvy_meta)
srvy_meta %>%
  split(.$topic) %>%
  map(select, question, display, denom) %>%
  write_json("output_data/cws/misc/cws_meta.json")

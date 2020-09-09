source(file.path("R", "packages.R"))
source(file.path("R", "utils", "cws_read_functions.R"))
# source(file.path("R", "prep", "anti_xtab.R"))

survey_year <- 2015

named_paths <- list.files(file.path("input_data", "crosstabs", survey_year), pattern = str_glue("^DataHaven{survey_year}.+\\.xlsx?$"), full.names = T) %>%
  name_paths()

cws_read <- suppressWarnings(imap(named_paths, read_cws, year = survey_year)) %>%
  map(mutate, year = survey_year)

# there are some oddball categories & groups on regions e.g. EHHD has EHHD Total instead of Total
all_lvls <- union(levels(cws_read$`5CT`$category), levels(cws_read$`Fairfield County`$category))
lvls_to_total <- cws_read %>%
  map(~setdiff(levels(.$category), all_lvls)) %>%
  compact() %>%
  set_names(NULL) %>%
  set_names(rep_along(., "Total"))


cws18 <- cws_read %>%
  map(mutate, category = fct_recode(category, !!!lvls_to_total)) %>%
  map(mutate, group = if_else(category == "Total", "Total", as.character(group)) %>%
        as_factor()) %>%
  map(~filter(., !str_detect(response, "^Summary:")))

weights18 <- suppressWarnings(map(named_paths, clean_weights)) %>%
  map(mutate, year = survey_year)


# write out
base_path_out <- file.path("output_data", "cws")

# gnh has mislabeled eviction, so omitting it for now
lookup18 <- suppressWarnings(make_lookup(cws18[names(cws18) != "Greater New Haven"]))
write_csv(lookup18, file.path(base_path_out, "lookup", str_glue("cws_lookup_{survey_year}.csv")))


pwalk(list(cws18, weights18, names(cws18)), write_cws, base_path = base_path_out, year = survey_year)



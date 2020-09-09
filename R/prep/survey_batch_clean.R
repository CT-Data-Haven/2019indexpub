library(tidyverse)

source(file.path("R", "utils", "anti_xtab.R"))

# survey_year <- 2018
years <- c(2015, 2018) %>% set_names()

######## functions
get_paths <- function(yr) {
  files <- list.files(file.path("input_data", "crosstabs", yr), pattern = str_glue("^DataHaven{yr}.+\\.xlsx?$"), full.names = T)
  regions <- files %>%
    str_extract("(?<=DataHaven\\d{4} ).+$") %>%
    str_remove("( Crosstabs Pub| Crosstabs)?\\.xlsx?$") %>%
    str_replace_all("^CCF$", "Greater Waterbury") %>%
    str_replace_all("^CRCOG$", "Greater Hartford") %>%
    str_remove_all("(Statewide|CCF|CRCOG|Region|Towns)") %>%
    str_replace_all("Cty", "County") %>%
    str_replace_all("(?<=[a-z])\\B(?=[A-Z])", " ") %>%
    # str_trim() %>%
    str_remove_all("\\s{2,}") %>%
    str_replace("(Inner Ring|Outer Ring)([\\w\\s]+$)", "\\2 \\1") %>%
    str_remove("Greater (?=[\\w\\s]+Ring)") %>%
    str_trim()
  setNames(files, regions)
}

read_cws <- function(yr, path, region) {
  rd <- read_xtabs(path, year = yr) %>% 
    mutate(x3 = str_replace(x3, "(CRCOG|CCF)", "Total") %>%
             str_remove(" Region")) %>%
    xtab2df() %>%
    filter(!is.na(code))
  if (!"group" %in% names(rd)) {
    rd$group <- rd$category
  }
  if (region == "5CT") {
    rd <- rd %>% 
      filter(category != "Total") %>%
      mutate(category = group)
  }
  rd %>%
    mutate_at(vars(category, group), as_factor) %>%
    mutate(group = group %>%
             fct_recode(Male = "M", Female = "F", Black = "Black/Afr Amer", Latino = "Hispanic", "$75K-$100K" = "$75K-100K") %>%
             fct_relabel(str_replace, "School", "school") %>%
             fct_relabel(str_replace, " High", " high") %>%
             fct_relabel(str_replace, "^\\b(?=\\d)", "Ages ") %>%
             fct_relabel(str_replace, " White", " white") %>%
             fct_relabel(str_remove, "(?<=\\<)\\s") %>%
             fct_relabel(str_remove_all, "(?<=/)\\s") %>%
             fct_recode("Not white" = "Non-White")) %>%
    mutate(category = category %>%
             fct_recode("5CT" = "Five Connecticuts") %>%
             fct_relabel(str_remove, "^[\\w\\s]+(?=Total)") %>%
             fct_relabel(str_remove, "(?<=Total)[\\w\\s]+$") %>%
             fct_relabel(str_remove, " Statewide") %>%
             fct_recode("With children" = "Children in HH")) %>%
    mutate_at(vars(category, group), fct_recode, Total = region) %>%
    filter(category != "Connecticut") %>%
    mutate(year = yr) %>%
    mutate_if(is.factor, fct_drop)
}

get_weights <- function(yr, path) {
  read_weights(path) %>%
    bind_rows(tibble(group = "Total", weight = 1)) %>%
    mutate(year = yr) %>%
    mutate(group = as_factor(group) %>%
             fct_relabel(str_replace, " to ", "-") %>%
             fct_relabel(str_replace, " (and older|or more)", "+") %>%
             fct_relabel(str_replace, "Less than (?=\\$)", "<") %>%
             fct_relabel(str_replace, " High", " high") %>%
             fct_relabel(str_remove, " degree") %>%
             fct_relabel(str_replace_all, ",000", "K") %>%
             fct_relabel(str_replace, " - ", "-") %>%
             fct_relabel(str_replace, "^\\b(?=\\d)", "Ages ") %>%
             fct_recode(Black = "African American/Black", Latino = "Hispanic") %>%
             fct_relabel(str_replace, " White", " white") %>%
             fct_recode("Not white" = "Non-White") %>%
             fct_relabel(str_remove, " or GED"))
}

get_lookup <- function(df) {
  df %>%
    mutate(question = question %>% 
           str_replace_all("sinced", "since")) %>%
    mutate(short_question = question %>%
             str_to_lower() %>%
             str_remove("^\\([\\w\\s']*?\\)") %>%
             str_remove_all("[[:punct:]´]") %>%
             str_trim() %>%
             str_replace_all("\\s{2,}", " ") %>%
             str_sub(end = 100)
    ) %>%
    select(code, short_question) %>%
    mutate(code = as_factor(code)) %>%
    arrange(code) %>%
    distinct(code, .keep_all = T) %>%
    rename(question = short_question)
}



####### clean up
all_paths <- years %>%
  map(get_paths)
# way easier to deal with all this if it's all in one df, can split later by year & name
cws_read <- all_paths %>%
  imap_dfr(function(paths, year) {
    imap_dfr(paths, ~read_cws(year, .x, .y), .id = "name")
  }) %>%
  mutate_at(vars(name, category, group), as_factor)

# there are some oddball categories & groups on regions e.g. EHHD has EHHD Total instead of Total
all_lvls <- cws_read %>%
  filter(name %in% c("5CT", "Fairfield County")) %>%
  pull(category) %>%
  fct_drop() %>%
  levels()

lvls_to_total <- setdiff(levels(cws_read$category), all_lvls)

cws_df <- cws_read %>%
  mutate(category = fct_collapse(category, Total = lvls_to_total),
         group = as_factor(if_else(category == "Total", "Total", as.character(group))))

weights <- all_paths %>%
  imap(function(paths, year) map(paths, ~get_weights(year, .)))

lookups <- cws_df %>%
  split(.$year) %>%
  map(get_lookup)



######## write out
cws_df %>%
  split(.$year) %>%
  iwalk(function(cws, yr) {
    cws %>%
      mutate_if(is.factor, fct_drop) %>%
      split(.$name) %>%
      iwalk(function(df, region) {
        out <- df %>% select(-question, -name)
        region_name <- str_to_lower(region) %>% str_replace_all("\\s", "_")
        filename <- str_glue("{region_name}_{yr}_cws_data.csv")
        path <- file.path("output_data", "cws", "long", yr, filename)
        write_csv(out, path)
      })
  })

weights %>%
  iwalk(function(wt, yr) {
    iwalk(wt, function(df, region) {
      region_name <- str_to_lower(region) %>% str_replace_all("\\s", "_")
      filename <- str_glue("{region_name}_{yr}_cws_weights.csv")
      path <- file.path("output_data", "cws", "weights", yr, filename)
      write_csv(df, path)
    })
  })

lookups %>%
  iwalk(function(df, yr) {
    filename <- str_glue("cws_lookup_{yr}.csv")
    path <- file.path("output_data", "cws", "lookup", filename)
    write_csv(df, path)
  })

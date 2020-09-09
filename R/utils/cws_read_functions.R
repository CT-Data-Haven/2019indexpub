# source(file.path("..", "packages.R"))
source(file.path("R", "utils", "anti_xtab.R"))

# take filepaths and create clean location names; return named vector of paths
name_paths <- function(paths) {
  set_names(paths, ~str_extract(., "(?<=DataHaven\\d{4} ).+$") %>%
              str_remove("( Crosstabs Pub| Crosstabs)?\\.xlsx$") %>%
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
  )
}

# from named path, read_xtabs, start cleaning levels
read_cws <- function(path, region, year = 2018) {
  rd <- read_xtabs(path, year = year) %>% 
      mutate(x3 = str_replace(x3, "(CRCOG|CCF)", "Total") %>%
               str_remove(" Region")) %>%
      xtab2df()
    if (!"group" %in% names(rd)) {
      rd$group <- rd$category
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
               fct_recode("Not white" = "Non-White", "Children in household" = "Yes", "No children in household" = "No")) %>%
      mutate(category = category %>%
               fct_relabel(str_remove, "^[\\w\\s]+(?=Total)") %>%
               fct_relabel(str_remove, "(?<=Total)[\\w\\s]+$") %>%
               fct_relabel(str_remove, " Statewide") %>%
               fct_recode("With children" = "Children in HH")) %>%
      mutate_at(vars(category, group), fct_recode, Total = region)
}



clean_weights <- function(path) {
  read_weights(path) %>%
    bind_rows(tibble(group = "Total", weight = 1)) %>%
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
             fct_recode("Not white" = "Non-White", "Children in household" = "Yes", "No children in household" = "No") %>%
             fct_relabel(str_remove, " or GED"))
}


# clean up lookup info from list of cws18
# make lookup from all cws18 dataframes, since a few crosstabs might be missing questions altogether
# fix common typos
# also a few codes are duplicated, e.g. RENTFORECLA for both bank took possession & missed mortgage payments
make_lookup <- function(data_lst) {
  look <- bind_rows(data_lst) %>%
    mutate(question = question %>% 
             str_replace_all("sinced", "since")) %>%
    mutate(short_question = question %>%
             str_to_lower() %>%
             str_remove("^\\([\\w\\s']*?\\)") %>%
             str_remove_all("[[:punct:]´]") %>%
             str_remove_all("\\=") %>%
             # str_trim() %>%
             str_replace_all("\\s{2,}", " ") %>%
             str_replace_all("(?<=\\<)moveyear(?=\\>)", "year") %>%
             str_replace_all("selfreported height and weight", "height and weight") %>%
             str_remove_all("yesno (to|for) each") %>%
             str_remove("an eviction is when your landlord forces you to move when you dont want to") %>%
             str_remove("did any of the following contribute to your moving from your previous home in <year>") %>%
             str_remove("if moved since 2016 currently do not own a home and previously (owned|rented) a home") %>%
             str_remove("if moved since 2016 and currently do not own a home") %>%
             str_remove("if unfairly fired unfairly denied a promotion or raise or not hired for a job for unfair reasons?") %>%
             str_remove("if unfairly stopped searched questioned physically threatened or abused by the police") %>%
             str_replace("(?<=\\<)5 for men .*4 for women(?=\\>)", "4 women 5 men") %>%
             str_trim() # %>%
           # str_sub(end = 160)
    ) %>%
    select(code, short_question) %>%
    mutate(code = as_factor(code)) %>%
    arrange(code) %>%
    # distinct(code, .keep_all = T) %>%
    rename(question = short_question)
  
  code_dupes <- look %>%
    distinct(code, question) %>%
    arrange(code) %>%
    group_by(code) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    group_by(code) %>%
    mutate(code_dedupe = paste(code, row_number(), sep = "x"))
  
  look %>%
    distinct(code, question) %>%
    left_join(code_dupes, by = c("code", "question")) %>%
    mutate(code = as.character(code)) %>%
    mutate(code_dedupe = coalesce(code_dedupe, code)) %>%
    select(code = code_dedupe, question) %>%
    mutate(question = str_sub(question, end = 120))
}


# write out both cws data and weights into separate csv files
write_cws <- function(cws, wts, region, base_path, year) {
  region_name <- str_to_lower(region) %>% str_replace_all("\\s", "_")
  cws_out <- cws %>% select(-question)
  cws_filename <- str_glue("{region_name}_{year}_cws_data.csv")
  wts_filename <- str_glue("{region_name}_{year}_cws_weights.csv")
  write_csv(cws_out, file.path(base_path, "long", cws_filename))
  write_csv(wts, file.path(base_path, "weights", wts_filename))
}



# collapse responses, remove nonanswers, filter for desired responses
# assumes normal column names so I skipped doing tidyeval columns
collapse_response <- function(.data, categories = list(excellent_good = c("Excellent", "Good")), nons = c("Don't know", "Refused")) {
  keeps <- names(categories)
  df1 <- .data %>%
    mutate(response = fct_collapse(response, !!!categories)) %>%
    group_by_at(vars(-value)) %>%
    summarise(value = sum(value)) 
  
  if (is.null(nons)) {
    out <- df1 %>% filter(response %in% keeps)
  } else {
    out <- df1 %>%
      sub_nonanswers(nons = nons) %>%
      filter(response %in% keeps)
  }
  ungroup(out)
}

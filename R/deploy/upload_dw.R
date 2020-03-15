library(tidyverse)
library(data.world)

dataset <- "camille86/cws2018"

desc_survey <- "Subset of indicators from the 2018 DataHaven Community Wellbeing Survey by location and demographic group"
url_survey <- "https://github.com/CT-Data-Haven/2019indexpub/blob/master/output_data/cws/wide/cws_2018_all_geos_wide.csv"
req_survey <- file_create_or_update_request(file_name = "cws_overview_by_group_2018.csv", 
                              description = desc_survey, 
                              labels = list("clean data"), 
                              url = url_survey)


# index values
desc_scores <- "Scores for DataHaven's three indexes: Community, Personal Wellbeing, and Neighborhood Assets"
url_scores <- "https://github.com/CT-Data-Haven/2019indexpub/blob/master/output_data/cws/misc/index_scores_distro.csv"
req_scores <- file_create_or_update_request(file_name = "index_scores_2018.csv",
                                      description = desc_scores,
                                      labels = list("clean data"),
                                      url = url_scores)

list(req_survey, req_scores) %>%
  map(~update_dataset(dataset, dataset_update_request(files = list(.))))


# add license: cc sharealike
update_dataset(dataset, dataset_update_request(license_string = "CC-BY-SA"))

read_csv("R/utils/dataworld_urls.csv") %>%
  pivot_longer(-file) %>%
  split(.$file) %>%
  map(select, -file) %>%
  map(deframe) %>%
  map(as.list) %>%
  jsonlite::write_json("output_data/cws/misc/downloads.json", auto_unbox = TRUE)

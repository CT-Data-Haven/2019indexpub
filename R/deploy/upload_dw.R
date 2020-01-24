library(tidyverse)
library(data.world)

dataset <- "camille86/cws2018"

desc1 <- "Subset of indicators from the 2018 DataHaven Community Wellbeing Survey by location and demographic group"
url1 <- "https://github.com/CT-Data-Haven/2019indexpub/blob/master/output_data/cws/wide/cws_2018_all_geos_wide.csv"
req1 <- file_create_or_update_request(file_name = "cws_overview_by_group_2018.csv", 
                              description = desc1, 
                              labels = list("clean data"), 
                              url = url1)

update_dataset(dataset, dataset_update_request(files = list(req1)))

# index values
desc2 <- "Scores for DataHaven's three indexes: Community, Personal Wellbeing, and Neighborhood Assets"
url2 <- "https://github.com/CT-Data-Haven/2019indexpub/blob/master/output_data/cws/misc/index_scores_distro.csv"
req2 <- file_create_or_update_request(file_name = "index_scores_2018",
                                      description = desc2,
                                      labels = list("clean data"),
                                      url = url2)

update_dataset(dataset, dataset_update_request(files = list(req2)))

# add license: cc sharealike
update_dataset(dataset, dataset_update_request(license_string = "CC-BY-SA"))

read_csv("R/utils/dataworld_urls.csv") %>%
  deframe() %>%
  as.list() %>%
  jsonlite::write_json("output_data/cws/misc/downloads.json")

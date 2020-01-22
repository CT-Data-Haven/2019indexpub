library(tidyverse)
library(data.world)

url <- "camille86/neighborhoods18"
dwapi::upload_file(url, "output_data/cws/wide/cws_2018_all_geos_wide.csv", "cws_by_group_2018.csv")
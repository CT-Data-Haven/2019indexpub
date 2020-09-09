# subset of previous script for 2015
# when making wide data of all regions, will have to check that codes exist to protect on smaller areas with missing questions
source(file.path("R", "packages.R"))
source(file.path("R", "utils", "anti_xtab.R"))
source(file.path("R", "utils", "cws_read_functions.R"))

# google sheet of which indicators to include with their headings & codes

lookup <- read_csv("https://docs.google.com/spreadsheets/d/1UY6slMipfH_XouWhlefe0pA_gBwtHGJlzvtu7gUsk8A/export?format=csv&gid=434948615") %>%
  filter(!is.na(indicator)) %>%
  mutate(indicator = as_factor(indicator))

cws_read <- list.files(file.path("output_data", "cws", "long", "2015"), "*.csv", full.names = TRUE) %>%
  set_names(~str_match_all(., "/([\\w\\d]+)_2015") %>% map_chr(2)) %>%
  map_dfr(read_csv, .id = "name") %>%
  inner_join(lookup %>% select(indicator, code), by = "code") %>%
  filter(!str_detect(response, "(Summary|\\*|based on)") | indicator == "obesity") %>%
  filter(category != "Race/Ethnicity" | group %in% c("White", "Black", "Latino")) %>%
  mutate_at(vars(category, group), as_factor)



cws_split <- cws_read %>%
  split(.$indicator) %>%
  map(mutate, response = as_factor(response))


out <- list()

# satisfied with area: yes
out$satisfied_with_area <- cws_split$satisfied_w_area %>%
  collapse_response(list(yes = "Yes"))

# a bunch of these have the same arguments: excellent/good, nons = don't know enough.., refused
# local govt is responsive; police approval; suitable employment; good to raise kids; condition of parks
ex_qs <- c("local_govt_responsive", "police_approval", "suitable_employment", "good_to_raise_kids", "good_cond_of_parks")

out[ex_qs] <- cws_split[ex_qs] %>%
  map(collapse_response, nons = c("Don't know enough about it in order to say", "Refused"))
rm(ex_qs)

# influence local govt: great, moderate, a little
out$influence_over_local_govt <- cws_split$influence_over_local_govt %>%
  collapse_response(list(at_least_a_little = c("Great influence", "Moderate influence", "A little influence")))

# access to car: very, fairly often
out$access_to_car <- cws_split$access_to_car %>%
  collapse_response(list(access = c("Very often", "Fairly often")))


# same arguments: strongly/somewhat agree
# walking distance; sidewalks; biking; rec facilities; trust neighbors; youth role models; improving area
ag_qs <- c("locations_in_walking_dist", "safe_sidewalks", "safe_biking", "rec_facilities_avail", "trust_neighbors", "youth_have_role_models")
out[ag_qs] <- cws_split[ag_qs] %>%
  map(collapse_response, list(agree = c("Strongly agree", "Somewhat agree")))
rm(ag_qs)

# safe walking at night: switch to somewhat/strongly disagree
out$safe_walking_at_night <- cws_split$unsafe_walking_at_night %>%
  collapse_response(list(disagree = c("Strongly disagree", "Somewhat disagree")))

# would save fire station: very/somewhat likely
out$would_save_fire_station <- cws_split$would_save_fire_station %>%
  collapse_response(list(likely = c("Very likely", "Somewhat likely")))


# self rated health: excellent/very good
out$very_good_self_rate_health <- cws_split$very_good_self_rate_health %>%
  collapse_response(list(very_good = c("Excellent", "Very good")))

# satisfied w/ life:  completely/mostly
out$life_satisfaction <- cws_split$satisfied_w_life %>%
  collapse_response(list(satisfied = c("Completely satisfied", "Mostly satisfied")))

# happy: completely/mostly
out$happy <- cws_split$happy %>%
  collapse_response(list(happy = c("Completely", "Mostly")))

# anxious: completely/mostly
out$anxious <- cws_split$anxious %>%
  collapse_response(list(anxious = c("Completely", "Mostly")))

# medical: yes
y_qs <- c("diabetes", "have_health_insurance", "didnt_get_med_care", "postponed_med_care", "couldnt_afford_rx")
out[y_qs] <- cws_split[y_qs] %>%
  map(collapse_response, list(yes = "Yes"))
rm(y_qs)

# skipping asthma since it was phrased differently in 2015


# medical home: no med_home1, none at all med_home2
out$no_medical_home <- list(
  cws_split$med_home1 %>% collapse_response(list(no = "No")),
  cws_split$med_home2 %>% collapse_response(list(none = "None at all"))
) %>%
  map(spread, key = response, value) %>%
  reduce(inner_join, by = c("name", "category", "group", "year")) %>%
  mutate(value = no * none) %>%
  select(name, category, group, year, value)

# er visit at least once: 1 to 2, 3 or more
out$visited_er <- cws_split$visited_er %>%
  collapse_response(list(at_least_once = c("1 to 2", "3 or more")))

# dental visit: within last 6 mo, more than 6mo
out$dental_visit_past_yr <- cws_split$dental_visit_past_yr %>%
  collapse_response(list(within_yr = c("Within the last 6 months", "More than 6 months but less than a year")))


# take out depression: phrasing changed

# smoking: smoke1 yes x smoke2 every/some days
out$smoking <- list(
  cws_split$smoke1 %>% collapse_response(list(smoke100 = "Yes")),
  cws_split$smoke2 %>% collapse_response(list(current = c("Every day", "Some days")))
) %>%
  map(spread, key = response, value) %>%
  reduce(inner_join, by = c("name", "category", "group", "year")) %>%
  mutate(value = smoke100 * current) %>%
  select(name, category, group, year, value)



# financial insecurity: just getting by, finding difficult/very difficult
out$financial_insecurity <- cws_split$financial_insecurity %>%
  collapse_response(list(insecure = c("Just getting by", "Finding it difficult", "Finding it very difficult")))

# savings: less than a month, less than 2 months
out$less_than_2mo_savings <- cws_split$less_than_2mo_savings %>%
  collapse_response(list(less_than_2 = c("Less than a month", "At least one month but less than 2")))

# taking out underemployment since I always screw it up
# underemployment: (no job + (part time x want full time x working)) / labor force
# unemployed <- cws_split$underemp1 %>%
#   collapse_response(list(working = "Yes", unemployed = "No, but would like to work"), nons = NULL)
# labor_force <- cws_split$underemp1 %>%
#   collapse_response(list(labor_force = c("Yes", "No, but would like to work")), nons = NULL)
# part_time <- cws_split$underemp2 %>% 
#   collapse_response(list(parttime = "Part time"), nons = "Refused")
# prefer_ft <- cws_split$underemp3 %>%
#   collapse_response(list(prefer_ft = "Rather have a full time job"), nons = "Refused")
# 
# out$underemployment <- lst(labor_force, unemployed, part_time, prefer_ft) %>%
#   map(spread, key = response, value) %>%
#   map(select, -code, -indicator) %>%
#   reduce(inner_join, by = c("name", "category", "group", "year")) %>%
#   mutate(value = (unemployed + (working * parttime * prefer_ft)) / labor_force) %>%
#   select(name, category, group, year, value)
# rm(unemployed, labor_force, part_time, prefer_ft)

# no bank account: no
out$no_bank_account <- cws_split$no_bank_account %>%
  collapse_response(list(no = "No"))

# bunch of questions with yes
# food, housing
y_qs <- c("food_insecurity", "housing_insecurity")
out[y_qs] <- cws_split[y_qs] %>%
  map(collapse_response, list(yes = "Yes"))
rm(y_qs)



# join all
out_df <- out %>%
  map(select, name, category, group, value) %>%
  imap(~rename(.x, !!.y := value)) %>%
  reduce(full_join, by = c("name", "category", "group")) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(name = clean_titles(name, cap_all = TRUE))
# add somewhere for name
write_csv(out_df, "output_data/cws/wide/cws_2015_all_geos_wide.csv", na = "")
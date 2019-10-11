# when making wide data of all regions, will have to check that codes exist to protect on smaller areas with missing questions
source(file.path("R", "packages.R"))
source(file.path("R", "utils", "anti_xtab.R"))
source(file.path("R", "utils", "cws_read_functions.R"))

# google sheet of which indicators to include with their headings & codes
lookup <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vREw976xkTlpAG31fnVJXrRHwFeoMKOZC38dPTJ0svH7GAQ6_-LPi70u1WP2XROcbsLQsrvkE7bCxFq/pub?gid=2061966225&single=true&output=csv") %>%
  filter(!is.na(indicator)) %>%
  mutate(indicator = as_factor(indicator))

gnh <- read_csv("output_data/cws/long/greater_new_haven_2018_cws_data.csv") %>%
  inner_join(lookup %>% select(-question), by = "code") %>%
  filter(!str_detect(response, "(Summary|\\*|based on)") | indicator == "obesity") %>%
  mutate_at(vars(category, group), as_factor)
gnh %>% distinct(indicator, response) %>% group_by(indicator) %>% summarise(response = paste(response, collapse = ",")) %>% write_tsv("cws.txt")

cws_split <- gnh %>%
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

# evicted from last home: yes
out$evicted <- cws_split$evicted %>%
  collapse_response(list(yes = "Yes"))

# received eviction notice: yes
out$received_evict_notice <- cws_split$received_evict_notice %>%
  collapse_response(list(yes = "Yes"))

# same arguments: strongly/somewhat agree
# walking distance; sidewalks; biking; rec facilities; trust neighbors; youth role models; improving area
ag_qs <- c("locations_in_walking_dist", "safe_sidewalks", "safe_biking", "rec_facilities_avail", "trust_neighbors", "youth_have_role_models", "neighbors_improving_area")
out[ag_qs] <- cws_split[ag_qs] %>%
  map(collapse_response, list(agree = c("Strongly agree", "Somewhat agree")))
rm(ag_qs)

# safe walking at night: switch to somewhat/strongly disagree
out$safe_walking_at_night <- cws_split$unsafe_walking_at_night %>%
  collapse_response(list(disagree = c("Strongly disagree", "Somewhat disagree")))

# would save fire station: very/somewhat likely
out$would_save_fire_station <- cws_split$would_save_fire_station %>%
  collapse_response(list(likely = c("Very likely", "Somewhat likely")))

# youth experiences: almost certain/very likely
cert_qs <- c("youth_likely_graduate_hs", "youth_likely_job_w_opportunity", "youth_likely_in_gang", "youth_likely_abuse_drugs_alc", "youth_likely_felony")
out[cert_qs] <- cws_split[cert_qs] %>%
  map(collapse_response, list(likely = c("Almost certain", "Very likely")))
rm(cert_qs)

# self rated health: excellent/very good
out$very_good_self_rate_health <- cws_split$very_good_self_rate_health %>%
  collapse_response(list(very_good = c("Excellent", "Very good")))

# satisfied w/ life:  completely/mostly
out$life_satisfaction <- cws_split$life_satisfaction %>%
  collapse_response(list(satisfied = c("Completely satisfied", "Mostly satisfied")))

# happy: completely/mostly
out$happy <- cws_split$happy %>%
  collapse_response(list(happy = c("Completely", "Mostly")))

# anxious: completely/mostly
out$anxious <- cws_split$anxious %>%
  collapse_response(list(anxious = c("Completely", "Mostly")))

# medical: yes
y_qs <- c("diabetes", "have_health_insurance", "didnt_get_med_care", "postponed_med_care", "couldnt_afford_rx", "altered_rx")
out[y_qs] <- cws_split[y_qs] %>%
  map(collapse_response, list(yes = "Yes"))
rm(y_qs)

# current asthma: yes have asthma x yes current
out$asthma <- list(
  cws_split$asthma1 %>% collapse_response(list(asthma = "Yes")),
  cws_split$asthma2 %>% collapse_response(list(current = "Yes"))
) %>%
  map(spread, key = response, value) %>%
  reduce(inner_join, by = c("category", "group", "year")) %>%
  mutate(value = asthma * current) %>%
  select(category, group, year, value)



# medical home: no med_home1, none at all med_home2
out$no_medical_home <- list(
  cws_split$med_home1 %>% collapse_response(list(no = "No")),
  cws_split$med_home2 %>% collapse_response(list(none = "None at all"))
) %>%
  map(spread, key = response, value) %>%
  reduce(inner_join, by = c("category", "group", "year")) %>%
  mutate(value = no * none) %>%
  select(category, group, year, value)

# er visit at least once: 1 to 2, 3 or more
out$visited_er <- cws_split$visited_er %>%
  collapse_response(list(at_least_once = c("1 to 2", "3 or more")))

# dental visit: within last 6 mo, more than 6mo
out$dental_visit_past_yr <- cws_split$dental_visit_past_yr %>%
  collapse_response(list(within_yr = c("Within the last 6 months", "More than 6 months but less than a year")))

# receive emotional support: always, usually
out$receive_emotional_support <- cws_split$receive_emotional_support %>%
  collapse_response(list(usually = c("Always", "Usually")))

# depression: nearly every day, more than half
out$depression <- cws_split$depression %>%
  collapse_response(list(most_days = c("Nearly every day", "More than half the days")))

# smoking: smoke1 yes x smoke2 every/some days
out$smoking <- list(
  cws_split$smoke1 %>% collapse_response(list(smoke100 = "Yes")),
  cws_split$smoke2 %>% collapse_response(list(current = c("Every day", "Some days")))
) %>%
  map(spread, key = response, value) %>%
  reduce(inner_join, by = c("category", "group", "year")) %>%
  mutate(value = smoke100 * current) %>%
  select(category, group, year, value)

# heavy drinking: six to ten, more than ten
out$heavy_drinking <- cws_split$heavy_drinking %>%
  collapse_response(list(at_least_6 = c("Six to ten", "More than ten")))

# know someone abusing opioids: yes one, yes 2 to 4, yes 5 or more
out$know_someone_abusing_opioids <- cws_split$know_someone_abusing_opioids %>%
  collapse_response(list(at_least_1 = c("Yes, one person", "Yes, 2 to 4 people", "Yes, 5 or more people")))

# know someone died opioids: yes one, yes 2 to 4, yes 5 or more
out$know_someone_died_opioids <- cws_split$know_someone_died_opioids %>%
  collapse_response(list(at_least_1 = c("Yes, one person", "Yes, 2 to 4 people", "Yes, 5 or more people")))

# financial insecurity: just getting by, finding difficult/very difficult
out$financial_insecurity <- cws_split$financial_insecurity %>%
  collapse_response(list(insecure = c("Just getting by", "Finding it difficult", "Finding it very difficult")))

# savings: less than a month, less than 2 months
out$less_than_2mo_savings <- cws_split$less_than_2mo_savings %>%
  collapse_response(list(less_than_2 = c("Less than a month", "At least one month but less than 2")))

# debt: be in debt
out$negative_net_worth <- cws_split$in_debt %>%
  collapse_response(list(negative = "Be in debt"))

# underemployment: (no job + (part time x want full time)) / labor force
unemployed <- cws_split$underemp1 %>%
  collapse_response(list(working = "Yes", unemployed = "No, but would like to work"), nons = NULL)
labor_force <- cws_split$underemp1 %>%
  collapse_response(list(labor_force = c("Yes", "No, but would like to work")), nons = NULL)
part_time <- cws_split$underemp2 %>% 
  collapse_response(list(parttime = "Part time"), nons = "Refused")
prefer_ft <- cws_split$underemp3 %>%
  collapse_response(list(prefer_ft = "Rather have a full time job"), nons = "Refused")

out$underemployment <- lst(labor_force, unemployed, part_time, prefer_ft) %>%
  map(spread, key = response, value) %>%
  map(select, -code, -indicator) %>%
  reduce(inner_join, by = c("category", "group", "year")) %>%
  mutate(value = (unemployed + (working * parttime * prefer_ft)) / labor_force) %>%
  select(category, group, year, value)
rm(unemployed, labor_force, part_time, prefer_ft)

# no bank account: no
out$no_bank_account <- cws_split$no_bank_account %>%
  collapse_response(list(no = "No"))

# bunch of questions with yes
# food, housing, utility insecurity, discrim
y_qs <- c("food_insecurity", "housing_insecurity", "utility_insecurity", "discrim_in_workplace", "discrim_by_police", "discrim_getting_service", "discrim_in_housing", "discrim_in_healthcare")
out[y_qs] <- cws_split[y_qs] %>%
  map(collapse_response, list(yes = "Yes"))
rm(y_qs)



# join all
out_df <- out %>%
  map(select, category, group, year, value) %>%
  imap(~rename(.x, !!.y := value)) %>%
  reduce(full_join, by = c("category", "group", "year")) %>%
  mutate_if(is.numeric, round, digits = 2)
# add somewhere for name
write_csv(out_df, "output_data/cws/wide/gnh_pilot_profile.csv", na = "")
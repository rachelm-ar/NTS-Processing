### 
# Code purpose: Build robust NTEM segmented trip rates from NTS extract
# Trip rates segmented without household_composition and household to produce trip rates for infill to address low sample size issue


library("tidyverse")

# Import ntem_build - NTEM segmented dataset 
nts_ntem_df <- read_csv('Y:/NTS/tfn_ntem_build.csv', guess_max = 21474836)

# Analysis to remove incomplete surveys for prior years.

# Get unq household by year

unq_household <- nts_ntem_df %>%
  select(SurveyYear, HouseholdID, OutCom_B02ID) %>%
  distinct()

household_individual_count <- nts_ntem_df %>%
  filter(!is.na(IndividualID)) %>%
  select(SurveyYear, HouseholdID, IndividualID) %>%
  distinct() %>%
  group_by(SurveyYear, HouseholdID) %>%
  count() %>%
  rename(individuals = n)

household_trip_count <- nts_ntem_df %>%
  filter(!is.na(TripID)) %>%
  select(SurveyYear, HouseholdID, TripID) %>%
  distinct() %>%
  group_by(SurveyYear, HouseholdID) %>%
  count() %>%
  rename(trips = n)

household_days_count <- nts_ntem_df %>%
  filter(!is.na(TripID)) %>%
  select(SurveyYear, HouseholdID, TravDay) %>%
  distinct() %>%
  group_by(SurveyYear, HouseholdID) %>%
  count() %>%
  rename(days = n)

household_trip_distance <- nts_ntem_df %>%
  filter(!is.na(TripDisIncSW)) %>%
  select(SurveyYear, HouseholdID, TripDisIncSW) %>%
  distinct() %>%
  group_by(SurveyYear, HouseholdID) %>%
  count() %>%
  rename(logged_distance = n)

household_trip_time <- nts_ntem_df %>%
  filter(!is.na(TripTravTime)) %>%
  select(SurveyYear, HouseholdID, TripTravTime) %>%
  distinct() %>%
  group_by(SurveyYear, HouseholdID) %>%
  count() %>%
  rename(logged_time = n)

unq_household <- unq_household %>%
  left_join(household_individual_count) %>%
  left_join(household_trip_count) %>%
  left_join(household_days_count) %>%
  left_join(household_trip_distance) %>%
  left_join(household_trip_time) %>%
  mutate(individuals = replace_na(individuals, 0)) %>%
  mutate(trips = replace_na(trips, 0)) %>%
  mutate(days = replace_na(days, 0)) %>%
  mutate(logged_distance = replace_na(logged_distance, 0)) %>%
  mutate(logged_time = replace_na(logged_time, 0))

usable_households <- unq_household %>%
  filter(trips > 0) %>%
  select(SurveyYear, HouseholdID)

# Check the difference between completed and non completed
complete_households <- unq_household %>%
  filter(OutCom_B02ID == 1)

nrow_complete <- nrow(complete_households)

# Build set of the completes that will drop out under the new rules
complete_drop_households <- complete_households %>%
  filter(trips == 0)

nrow_drop_complete <- nrow(complete_drop_households)

print(nrow_drop_complete/nrow_complete*100)
# So I'm a but unsure how about 3% of the completed households are completed.
# They don't look completed!

incomplete_households <- unq_household %>%
  filter(OutCom_B02ID == 2)

hist(complete_households$individuals)
hist(incomplete_households$individuals)
hist(complete_households$trips)
hist(incomplete_households$trips)
hist(complete_households$days)
hist(incomplete_households$days)

# I think I'm going to remove houses with 0 trips
nts_completed <- usable_households %>%
  left_join(nts_ntem_df) %>%
  filter(cars!='-8')

# referring to ntem_tt, used column names from ntem_tt in nts_analysis_cruella.R
# cols <- colnames(ntem_tt)

cols <- c('traveller_type', 'age', 'gender', 'household', 'cars', 'household_composition', 'employment')

tt <- nts_completed %>%
  select(cols) %>%
  distinct()

# code to create simplified traveller type
# (without household and household_composition, 44 instead of 88 categories)

new_traveller_types <- structure(list(t1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
                                             12, 13, 14, 15, 16, 17, 18, 19, 20,
                                             21, 22, 23, 24, 25, 26, 27, 28, 29,
                                             30, 31, 32, 33, 34, 35, 36, 37, 38,
                                             39, 40, 41, 42, 43, 44, 45, 46, 47,
                                             48, 49, 50, 51, 52, 53, 54, 55, 56,
                                             57, 58, 59, 60, 61, 62, 63, 64, 65,
                                             66, 67, 68, 69, 70, 71, 72, 73, 74,
                                             75, 76, 77, 78, 79, 80, 81, 82, 83,
                                             84, 85, 86, 87, 88),
                                      t2 = c(1, 2, 1, 3, 4, 1, 3, 4, 5, 6, 5, 7,
                                             8, 5, 7, 8, 9, 10, 9, 11, 12, 9, 11,
                                             12, 13, 14, 13, 15, 16, 13, 15, 16,
                                             17, 18, 17, 19, 20, 17, 19, 20, 21,
                                             22, 21, 23, 24, 21, 23, 24, 25, 26,
                                             25, 27, 28, 25, 27, 28, 29, 30, 29,
                                             31, 32, 29, 31, 32, 33, 34, 33, 35,
                                             36, 33, 35, 36, 37, 38, 37, 39, 40,
                                             37, 39, 40, 41, 42, 41, 43, 44, 41,
                                             43, 44)), .Names = c("t1", "t2"), class = "data.frame", row.names = c(NA, -88L))

#nts_completed <- nts_completed %>% mutate(trav_type_simplified = traveller_type)

nts_completed$trav_type_simplified <- new_traveller_types$t2[match(nts_completed$traveller_type, new_traveller_types$t1)]


# Data segment builds ####
# Total trips by year

# To generate trip rates - apply w5 to trip data and apply w2 to individual data (i.e. Diary sample) 
# To calculate household car ownership - apply w3 to the household data (Interview sample) 
# To calculate the proportion of driving licence holders - apply w3 to the individual data (Interview sample) 
# To determine the unweighted sample size for trip rate analysis - apply w1 to the trip data and w1 to the individual data (Diary sample) 
# To determine the unweighted sample size for household car ownership or driving licence figures - apply no weights or w0 (Interview sample) 

# Ian Williams NTS report method
# Trips in each record multiplied by trip rate W5xHh and by their individual weight W2
# Then summed over the week for all relevant indivuals
# These total trips are then divided by the weighted sum of individuals, weighted by W2

setwd('Y:/NTS/less_segmentation/')
samples_by_year <- nts_completed %>%
  filter(!is.na(TripID)) %>%
  group_by(SurveyYear) %>%
  count() %>%
  write_csv('samples_by_year2.csv')

last_3 <- c(2017,2016,2015)
samples_last_3 <- nts_completed %>%
  filter(SurveyYear %in% last_3) %>%
  filter(!is.na(TripID)) %>%
  count() %>%
  write_csv('last_3_sample_size.csv')

ntem_samples <- nts_completed %>%
  filter(!is.na(TripID)) %>%
  group_by(trav_type_simplified) %>%
  count() %>%
  write_csv('ntem_types.csv')

ntem_samples_last_3 <- nts_completed %>%
  filter(SurveyYear %in% last_3) %>%
  filter(!is.na(TripID)) %>%
  group_by(trav_type_simplified) %>%
  count() %>%
  write_csv('ntem_types_last_3.csv')

trips_by_purpose <- nts_completed %>%
  filter(!is.na(TripID)) %>%
  group_by(trip_purpose) %>%
  count() %>%
  write_csv('trips_by_purpose.csv')

trips_by_purpose_last_3 <- nts_completed %>%
  filter(SurveyYear %in% last_3) %>%
  filter(!is.na(TripID)) %>%
  group_by(trip_purpose) %>%
  count() %>%
  write_csv('trips_by_purpose_last_3.csv')

trips_by_area_type_last_3 <- nts_completed %>%
  filter(SurveyYear %in% last_3) %>%
  filter(!is.na(TripID)) %>%
  group_by(area_type) %>%
  count() %>%
  write_csv('trips_by_area_type_last_3.csv')


# Use only three last years

# last_3 <- c(2017,2016,2015)
# nts_completed <- nts_completed %>%
#  filter(SurveyYear %in% last_3) %>%
#  filter(!is.na(TripID))


# Get trip rate by segment per weekday ####

hb_tr <- c(1,2,3,4,5,6,7,8)
nhb_tr <- c(12,13,14,15,16,18)

# Ian Williams NTS report method
# Trips in each record multiplied by trip rate W5xHh and by their individual weight W2
# Then summed over the week for all relevant indivuals
# These total trips are then divided by the weighted sum of individuals, weighted by W2

##### Weekly trip rates #####

# Step 1 - trips by trip rate & individual weight
week_total_trips <- nts_completed %>%
  mutate(weighted_trip = W1 * W5xHh * W2) %>%
  group_by(hb_purpose, trav_type_simplified, area_type) %>%
  summarise(trip_sample = n(), weekly_trips = sum(weighted_trip, na.rm=TRUE)) %>%
  ungroup()

# Step 2 - Get sum of weighted individuals
year_id_audit <- nts_completed %>%
  select(SurveyYear, IndividualID) %>%
  unique() %>%
  group_by(IndividualID) %>%
  # No values above 1 = no recurring yearly IDs
  count() %>%
  filter(n != 1)

week_total_individuals <- nts_completed %>%
  select(IndividualID, trav_type_simplified, area_type, W1, W2) %>%
  unique() %>%
  mutate(person_weight = W1 * W2) %>%
  group_by(trav_type_simplified, area_type) %>%
  summarise(person_sample = n(), person_weight = sum(W2)) %>%
  ungroup()

# Step 3 - Divide total trips by weighted sum of individuals
week_weighted_trips <- week_total_trips %>%
  left_join(week_total_individuals,
            by = c('trav_type_simplified', 'area_type')) %>%
  mutate(tfn_trip_rate = weekly_trips/person_weight)

# Commented out - very slow
#nts_completed %>% write_csv('nts_completed.csv')

week_weighted_trips %>% write_csv('weekly_trip_rates.csv')

# NTEM Comparison - delete before sending to Secure Lab ####
week_weighted_trips <- week_weighted_trips %>%
  mutate(hb_purpose = as.numeric(hb_purpose),
         area_type = as.numeric(area_type))

# Won't work now due to extra col
#ntem_tr <- read_csv('Y:/NorMITs Synthesiser/import/ntem_trip_rates_2016.csv') %>%
#  rename(hb_purpose = purpose) %>%
# won't work now with trav_type_simplified instead of traveller_type
#  left_join(week_weighted_trips,
#            by = c('hb_purpose', 'trav_type_simplified', 'area_type')) %>%
#  write_csv('ntem_comparison_DRAFT.csv')


#### Daily trip rates

# Step 1 - filter to weekdays only
weekd <- c(1,2,3,4,5)
wd_trips <- nts_completed %>%
  filter(TravDay %in% weekd)

# Step 2 - trips by trip rate & individual weight
wd_total_trips <- wd_trips %>%
  mutate(weighted_trip = W1 * W5xHh * W2) %>%
  group_by(hb_purpose, trav_type_simplified, area_type) %>%
  summarise(trip_sample = n(), daily_trips = sum(weighted_trip)/5) %>%
  ungroup()

# Step 3 - Get sum of weighted individuals
year_id_audit <- nts_completed %>%
  select(SurveyYear, IndividualID) %>%
  unique() %>%
  group_by(IndividualID) %>%
  # No values above 1 = no recurring yearly IDs
  count() %>%
  filter(n != 1)

wd_total_individuals <- wd_trips %>%
  select(IndividualID, traveller_type, trav_type_simplified, area_type, W1, W2) %>%
  unique() %>%
  mutate(person_weight = W1 * W2) %>%
  group_by(trav_type_simplified, area_type) %>%
  summarise(person_sample = n(), person_weight = sum(W2)) %>%
  ungroup()

# Step 4 - Divide total trips by weighted sum of individuals
wd_weighted_trips <- wd_total_trips %>%
  left_join(wd_total_individuals,
            by = c('trav_type_simplified', 'area_type')) %>%
  mutate(trip_rate = daily_trips/person_weight)

wd_weighted_trips %>% write_csv('weighted_trip_rates.csv')


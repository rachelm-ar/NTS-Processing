
# Code purpose: Build robust NTEM segmented trip rates from NTS extract

# Import ntem_build - NTEM segmented dataset 
nts_ntem_df <- read_csv('Y:/NTS/tfn_ntem_build.csv', guess_max = 10^9)

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

cols <- colnames(ntem_tt)

tt <- nts_completed %>%
  select(cols) %>%
  distinct()

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

setwd('Y:/NTS/')
samples_by_year <- nts_completed %>%
  filter(!is.na(TripID)) %>%
  group_by(SurveyYear) %>%
  count() %>%
  write_csv('samples_by_year.csv')

last_3 <- c(2007,2006,2005)
samples_last_3 <- nts_completed %>%
  filter(SurveyYear %in% last_3) %>%
  filter(!is.na(TripID)) %>%
  count() %>%
  write_csv('last_3_sample_size.csv')

ntem_samples <- nts_completed %>%
  filter(!is.na(TripID)) %>%
  group_by(traveller_type) %>%
  count() %>%
  write_csv('ntem_types.csv')

ntem_samples_last_3 <- nts_completed %>%
  filter(SurveyYear %in% last_3) %>%
  filter(!is.na(TripID)) %>%
  group_by(traveller_type) %>%
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
  group_by(hb_purpose, traveller_type, area_type) %>%
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
  select(IndividualID, traveller_type, area_type, W1, W2) %>%
  unique() %>%
  mutate(person_weight = W1 * W2) %>%
  group_by(traveller_type, area_type) %>%
  summarise(person_sample = n(), person_weight = sum(W2)) %>%
  ungroup()

# Step 3 - Divide total trips by weighted sum of individuals
week_weighted_trips <- week_total_trips %>%
  left_join(week_total_individuals,
            by = c('traveller_type', 'area_type')) %>%
  mutate(tfn_trip_rate = weekly_trips/person_weight)

nts_completed %>% write_csv('nts_completed.csv')

week_weighted_trips %>% write_csv('weekly_trip_rates.csv')

# NTEM Comparison - delete before sending to Secure Lab ####
week_weighted_trips <- week_weighted_trips %>%
  mutate(hb_purpose = as.numeric(hb_purpose),
         area_type = as.numeric(area_type))

# Won't work now due to extra col
ntem_tr <- read_csv('Y:/NorMITs Synthesiser/import/ntem_trip_rates_2016.csv') %>%
  rename(hb_purpose = purpose) %>%
  left_join(week_weighted_trips,
            by = c('hb_purpose', 'traveller_type', 'area_type')) %>%
  write_csv('ntem_comparison_DRAFT.csv')


#### Daily trip rates

# Step 1 - filter to weekdays only
weekd <- c(1,2,3,4,5)
wd_trips <- nts_completed %>%
  filter(TravDay %in% weekd)

# Step 2 - trips by trip rate & individual weight
wd_total_trips <- wd_trips %>%
  mutate(weighted_trip = W1 * W5xHh * W2) %>%
  group_by(hb_purpose, traveller_type, area_type) %>%
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
  select(IndividualID, traveller_type, area_type, W1, W2) %>%
  unique() %>%
  mutate(person_weight = W1 * W2) %>%
  group_by(traveller_type, area_type) %>%
  summarise(person_sample = n(), person_weight = sum(W2)) %>%
  ungroup()

# Step 4 - Divide total trips by weighted sum of individuals
wd_weighted_trips <- wd_total_trips %>%
  left_join(wd_total_individuals,
            by = c('traveller_type', 'area_type')) %>%
  mutate(trip_rate = daily_trips/person_weight)

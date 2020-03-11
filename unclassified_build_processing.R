### Organising NTS data and trip rates for regression analysis and selection ###


### Load libraries and import data ##############################################
library("tidyverse")
library("survey")
library("sjstats")
library("MASS")
library("compare")

select <- dplyr::select


# Import unclassified build
unclassified_build <- read_csv("Y:/NTS/tfn_unclassified_build.csv")


### Prepare dataset #############################################################


# Recode trip purposes as NTEM classification hb_purpose
unclassified_build <- unclassified_build %>%
  mutate(hb_purpose = case_when(
  TripPurpTo_B01ID == 1 ~ '1', # Work
  TripPurpTo_B01ID == 2 ~ '2', # In course of work
  TripPurpTo_B01ID == 3 ~ '3', # Education
  TripPurpTo_B01ID == 4 ~ '4', # Food shopping
  TripPurpTo_B01ID == 5 ~ '4', # Non food shopping
  TripPurpTo_B01ID == 6 ~ '5', # Personal business medical
  TripPurpTo_B01ID == 7 ~ '5', # Personal business eat / drink
  TripPurpTo_B01ID == 8 ~ '5', # Personal business other
  TripPurpTo_B01ID == 9 ~ '6', # Eat / drink with friends
  TripPurpTo_B01ID == 10 ~ '7', # Visit friends
  TripPurpTo_B01ID == 11 ~ '6', # Other social
  TripPurpTo_B01ID == 12 ~ '6', # Entertain /  public activity
  TripPurpTo_B01ID == 13 ~ '6', # Sport: participate
  TripPurpTo_B01ID == 14 ~ '8', # Holiday: base
  TripPurpTo_B01ID == 15 ~ '8', # Day trip / just walk
  TripPurpTo_B01ID == 16 ~ '6', # Other non-escort
  TripPurpTo_B01ID == 17 ~ '99', # Escort home
  TripPurpTo_B01ID == 18 ~ '1', # Escort work
  TripPurpTo_B01ID == 19 ~ '2', # Escort in course of work
  TripPurpTo_B01ID == 20 ~ '3', # Escort education
  TripPurpTo_B01ID == 21 ~ '4', # Escort shopping / personal business
  TripPurpTo_B01ID == 22 ~ '7', # Other escort
  TripPurpTo_B01ID == 23 ~ '99', # Home
  TRUE ~ as.character('unclassified')
))


# drop 'unclassified' hb_purpose
unclassified_build <- subset(unclassified_build, hb_purpose != 'unclassified')


# Recode gender(Sex_B01ID) as gender(Male or Female)
unclassified_build <- unclassified_build %>%
  mutate(gender = case_when(
    Sex_B01ID == 1 ~ 'Male',
    Sex_B01ID == 2 ~ 'Female',
    TRUE ~ as.character(Sex_B01ID)
  ))


# Combine Age and work status to age_workstatus
unclassified_build <- unclassified_build %>%
  mutate(age_work_status = case_when(
    Age_B01ID %in% c(1,2,3,4,5) ~ '0-16_child',
    Age_B01ID %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18) & EcoStat_B01ID %in% c(1,2) ~ '16-74_fte',
    Age_B01ID %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18) & EcoStat_B01ID %in% c(3,4) ~ '16-74_pte',
    Age_B01ID %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18) & EcoStat_B01ID == 7 ~ '16-74_stu',
    Age_B01ID %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18) & EcoStat_B01ID %in% c(5,6,8,9,10,11) ~ '16-74_unm',
    Age_B01ID %in% c(19,20,21) & EcoStat_B01ID %in% c(1,3)  ~ '75+_fte', # (n = 2279)
    Age_B01ID %in% c(19,20,21) & EcoStat_B01ID %in% c(2,4)  ~ '75+_pte', # (n = 5758)
    Age_B01ID %in% c(19,20,21) & EcoStat_B01ID %in% c(5,6,8,7,9,10,11) ~ '75+_retired', # This includes students (n=3), drop instead?
    TRUE ~ as.character(Age_B01ID)
  ))

                                                      
# Recode number of cars(NumCarVan_B02ID) as cars
unclassified_build <- unclassified_build %>%
  mutate(cars = case_when(
  NumCarVan_B02ID == 1 ~ '0',
  NumCarVan_B02ID == 2 ~ '1',
  NumCarVan_B02ID == 3 ~ '2+',
    TRUE ~ as.character(NumCarVan_B02ID)
  ))


# Drop cars = -8
unclassified_build <- subset(unclassified_build, cars != -8)


# Recode number of adults in household(HHoldNumAdults) as hh_adults
unclassified_build <- unclassified_build %>%
  mutate(hh_adults = case_when(
    HHoldNumAdults == 1 ~ '1', # 1 Adult
    HHoldNumAdults == 2 ~ '2', # 2 Adults
    HHoldNumAdults >= 3 ~ '3+', # 3+ Adults
    TRUE ~ as.character(HHoldNumAdults)
  ))


### From special_license_extraction_script, not sure if correct (correcting number of vehicles for adults in household?)
#nts_df <- nts_df %>%
#  mutate(NumCarVan_B02ID = case_when(
#    HHoldNumAdults == '1 Adult' & NumCarVan_B02ID == '1' ~ '1+',
#    HHoldNumAdults == '1 Adult' & NumCarVan_B02ID == '2+' ~ '1+',
#    TRUE ~ as.character(NumCarVan_B02ID)
#  ))


# Recode area type (HHoldAreaType1_B01ID) as NTEM area classification area_type
unclassified_build <- unclassified_build %>%
  mutate(area_type = case_when(
  HHoldAreaType1_B01ID == 1 ~ '1',
  HHoldAreaType1_B01ID == 2 ~ '2',
  HHoldAreaType1_B01ID == 3 ~ '3',
  HHoldAreaType1_B01ID == 4 ~ '3',
  HHoldAreaType1_B01ID == 5 ~ '3',
  HHoldAreaType1_B01ID == 6 ~ '3',
  HHoldAreaType1_B01ID == 7 ~ '3',
  HHoldAreaType1_B01ID == 8 ~ '3',
  HHoldAreaType1_B01ID == 9 ~ '4',
  HHoldAreaType1_B01ID == 10 ~ '5',
  HHoldAreaType1_B01ID == 11 ~ '6',
  HHoldAreaType1_B01ID == 12 ~ '6',
  HHoldAreaType1_B01ID == 13 ~ '7',
  HHoldAreaType1_B01ID == 14 ~ '7',
  HHoldAreaType1_B01ID == 15 ~ '8',
  TRUE ~ as.character(HHoldAreaType1_B01ID)
))


# Drop area_type = -8
unclassified_build <- subset(unclassified_build, area_type != -8)


# Convert SOC Types(XSOC2000_B02ID) as soc_stat
unclassified_build <- unclassified_build %>%
  mutate(soc_cat = case_when(
    XSOC2000_B02ID == 1 ~ '1', # 1	Managers and senior officials
    XSOC2000_B02ID == 2 ~ '1', # 2	Professional occupations
    XSOC2000_B02ID == 3 ~ '1', # 3	Associate professional and technical occupations
    XSOC2000_B02ID == 4 ~ '2', # 4	Administrative and secretarial occupations
    XSOC2000_B02ID == 5 ~ '2', # 5	Skilled trades occupations
    XSOC2000_B02ID == 6 ~ '2', # 6	Personal service occupations
    XSOC2000_B02ID == 7 ~ '2', # 7	Sales and customer service occupations
    XSOC2000_B02ID == 8 ~ '3', # 8	Process, plant and machine operatives
    XSOC2000_B02ID == 9 ~ '3', # 9	Elementary occupations
    TRUE ~ as.character(XSOC2000_B02ID)
  ))


# NS-Sec already in correct classification, rename for legibility
unclassified_build <- unclassified_build %>%
  rename(ns_sec = NSSec_B03ID)


# recode main mode(MainMode_B04ID) as main_mode
unclassified_build <- unclassified_build %>%
  mutate(main_mode = case_when(
    MainMode_B04ID == 1 ~ '1', # Walk
    MainMode_B04ID == 2 ~ '2', # Bicycle
    MainMode_B04ID == 3 ~ '3', # Car/van driver
    MainMode_B04ID == 4 ~ '3', # Car/van passenger
    MainMode_B04ID == 5 ~ '3', # Motorcycle
    MainMode_B04ID == 6 ~ '3', # Other private transport
    MainMode_B04ID == 7 ~ '5', # Bus in London
    MainMode_B04ID == 8 ~ '5', # Other local bus
    MainMode_B04ID == 9 ~ '5', # Non-local bus
    MainMode_B04ID == 10 ~ '99', # London Underground - leave for now
    MainMode_B04ID == 11 ~ '6', # Surface rail
    MainMode_B04ID == 12 ~ '3', # Taxi/minicab ie. a car
    MainMode_B04ID == 13 ~ '5', # Other public transport ie. small bus or light rail.
    TRUE ~ as.character(MainMode_B04ID)
  ))


# Create time_period from day(TravDay) and hour(TripTravTime)
# Confirm peak hours
unclassified_build <- unclassified_build %>%
  mutate(time_period = case_when(
    TravDay %in% c(1,2,3,4,5) & TripStart_B01ID %in% c(7,8,9) ~ 'AM', # Weekday AM peak
    TravDay %in% c(1,2,3,4,5) & TripStart_B01ID %in% c(10,11,12,13,14,15,16) ~ 'IP', # Weekday IP peak
    TravDay %in% c(1,2,3,4,5) & TripStart_B01ID %in% c(17,18,19) ~ 'PM', # Weekday PM peak
    TravDay %in% c(1,2,3,4,5) & TripStart_B01ID %in% c(1,2,3,4,5,6,20,21,22,23,24) ~ 'OP', # Offpeak
    TravDay %in% c(6) ~ 'SAT', # Saturday
    TravDay %in% c(6) ~ 'SUN', # Sunday
    TRUE ~ as.character('unclassified')
  ))


# Drop unclassified time_period
unclassified_build <- subset(unclassified_build, time_period != 'unclassified')


# Transform catgeorical variables to factors
facts <- c("hb_purpose", "age_work_status", "gender", "hh_adults", "cars", "area_type", "soc_cat", "ns_sec", "main_mode", "time_period")
unclassified_build <- unclassified_build %>% mutate_at(facts, funs(factor))


### Analysis to remove incomplete surveys for prior years.
# Obtain unique households
unq_household <- unclassified_build %>%
  select(SurveyYear, HouseholdID, OutCom_B02ID) %>%
  distinct()


# Count individuals per household, remove missing IndividualID 
household_individual_count <- unclassified_build %>%
  filter(!is.na(IndividualID)) %>%
  select(SurveyYear, HouseholdID, IndividualID) %>%
  distinct() %>%
  group_by(SurveyYear, HouseholdID) %>%
  count() %>%
  rename(individuals = n)


# Count trips per household, remove missing TripID
household_trip_count <- unclassified_build %>%
  filter(!is.na(TripID)) %>%
  select(SurveyYear, HouseholdID, TripID) %>%
  distinct() %>%
  group_by(SurveyYear, HouseholdID) %>%
  count() %>%
  rename(trips = n)


# Count days per household, remove missing TripID
household_days_count <- unclassified_build %>%
  filter(!is.na(TripID)) %>%
  select(SurveyYear, HouseholdID, TravDay) %>%
  distinct() %>%
  group_by(SurveyYear, HouseholdID) %>%
  count() %>%
  rename(days = n)


# Count trip distance by household, remove missing TripDisIncSW
household_trip_distance <- unclassified_build %>%
  filter(!is.na(TripDisIncSW)) %>%
  select(SurveyYear, HouseholdID, TripDisIncSW) %>%
  distinct() %>%
  group_by(SurveyYear, HouseholdID) %>%
  count() %>%
  rename(logged_distance = n)


# Count travel time by household, remove missing TripTravTime
household_trip_time <- unclassified_build %>%
  filter(!is.na(TripTravTime)) %>%
  select(SurveyYear, HouseholdID, TripTravTime) %>%
  distinct() %>%
  group_by(SurveyYear, HouseholdID) %>%
  count() %>%
  rename(logged_time = n)


# Obtain unique households with individuals, days, trips, distance and time, replace NAs
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


# Remove households with 0 trips (no trips = 0 left, but kept in script)
usable_households <- unq_household %>%
  filter(trips > 0) %>%
  select(SurveyYear, HouseholdID)


# Join usable households to unclassified_build
unclassified_build <- usable_households %>% left_join(unclassified_build)



### Weekly trip rate calculations (Ian Williams NTS Report Method) #####################

# Step 1 - Weighted trips and trip sample by segment
week_total_trips <- unclassified_build %>%
  mutate(weighted_trip = W1 * W5xHh * W2) %>%
  group_by(hb_purpose, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, area_type) %>%
  summarise(trip_sample = n(), weekly_trips = sum(weighted_trip, na.rm=TRUE)) %>%
  ungroup()


# Step 2 - Get sum of weighted individuals
### Necessary to include hb_purpose? ###???### Yes because person_sample calculation?
week_total_individuals <- unclassified_build %>%
  select(IndividualID, hb_purpose, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, area_type, W1, W2) %>%
  unique() %>%
  mutate(person_weight = W1 * W2) %>%
  group_by(hb_purpose, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, area_type, area_type) %>%
  summarise(person_sample = n(), person_weight = sum(W2)) %>%
  ungroup()


# Step 3 - Divide total trips by weighted sum of individuals
week_weighted_trips <- week_total_trips %>%
  left_join(week_total_individuals,
            by = c('hb_purpose', 'age_work_status', 'gender', 'hh_adults', 'cars', 'soc_cat', 'ns_sec', 'area_type')) %>%
  mutate(tfn_trip_rate = weekly_trips/person_weight)

week_weighted_trips %>% write_csv('weekly_trip_rates_new.csv')

getwd()


## to do/check:


# 1) soc classifiation based on tfn user segmentation note (sharepoint) soc 1-3 (1), 4-7 (2), 8-9 (3)
# 2) missing: job accessibility by rail, job accessibility by highway  - CS to include?
# 3) not included household_comp as not clear where from, also likely highly correlated with household_adults
# 4) cars recoding depending on adults in household?
# 5) need to segment by main_mode and time_period for mode/time regression?
# 6) drop 75+_fte, 75+_pte, 75+_stu?














                                                      








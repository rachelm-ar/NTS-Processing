#' Humza's HB trip pre-processing
#' Changes from unclassified_build_processing.R:
#' 1. hb_trip_purpose: correctly '4 and 5' were mixed up and many others
#' 2. age_work_status: full time and part time was mixed up 
#' select <- dplyr::select


# Load libraries and import data ------------------------------------------
library("dplyr")
library("readr")
library("tidyr")
library("data.table")

select <- dplyr::select

# Import unclassified build
unclassified_build <- read_csv("C:/Users/Pluto/Documents/Trip_rate_testing/tfn_unclassified_build.csv")

unclassified_build2 <- unclassified_build
unclassified_build <- unclassified_build2

# Import new area_type classification of wards
new_area_types <- read_csv("Y:/NTS/new area type lookup/new_area_type.csv")

# Recode purpose classifications ------------------------------------------------

# Classify trip origin to get HB/NHB splits
hb <- c(23)
nhb <- c(1,2,3,4,5,6,7,8,9,
         10,11,12,13,14,15,
         16,17,18,19,20,21,22)

unclassified_build <- unclassified_build %>%
  mutate(trip_origin = case_when(
    TripPurpFrom_B01ID %in% hb ~ 'hb',
    TripPurpFrom_B01ID %in% nhb ~ 'nhb',
    TRUE ~ as.character(NA)
  ))

# Recode trip purposes as NTEM classification hb_purpose for HB trips
#' When we remove Day trip/just walk we still have too many walking trips, but now we have lost alot of trips in all other modes
#' This suggests we add Day trip/just walk in but exclude walking trips
#' Since we still have too many walking trips, let's exclude short walking trips for holiday too.

#TripPurp14 <- unclassified_build %>%
#  filter(TripPurpTo_B01ID %in% c(14,15), MainMode_B04ID != 1)
#
#unclassified_build <- unclassified_build %>%
#  filter(!TripPurpTo_B01ID %in% c(14,15))

#TripPurp14 <- TripPurp14 %>%
#  filter((MainMode_B04ID %in% 2:13 | (MainMode_B04ID == 1 & TripDisIncSW > 1)))

unclassified_build <- unclassified_build %>%
  bind_rows(TripPurp14)

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
    TripPurpTo_B01ID == 22 ~ '6', # Other escort
    TripPurpTo_B01ID == 23 ~ '99', # Home
    TRUE ~ as.character('unclassified')
  ))

# Classify hb prior leg of nhb trips
unclassified_build <- unclassified_build %>%
  mutate(nhb_purpose_hb_leg = case_when(
    TripPurpFrom_B01ID == 1 ~ '1', # Work
    TripPurpFrom_B01ID == 2 ~ '2', # In course of work
    TripPurpFrom_B01ID == 3 ~ '3', # Education
    TripPurpFrom_B01ID == 4 ~ '4', # Food shopping
    TripPurpFrom_B01ID == 5 ~ '4', # Non food shopping
    TripPurpFrom_B01ID == 6 ~ '5', # Personal business medical
    TripPurpFrom_B01ID == 7 ~ '5', # Personal business eat / drink
    TripPurpFrom_B01ID == 8 ~ '5', # Personal business other
    TripPurpFrom_B01ID == 9 ~ '6', # Eat / drink with friends
    TripPurpFrom_B01ID == 10 ~ '7', # Visit friends
    TripPurpFrom_B01ID == 11 ~ '6', # Other social
    TripPurpFrom_B01ID == 12 ~ '6', # Entertain /  public activity
    TripPurpFrom_B01ID == 13 ~ '6', # Sport: participate
    TripPurpFrom_B01ID == 14 ~ '8', # Holiday: base
    TripPurpFrom_B01ID == 15 ~ '8', # Day trip / just walk
    TripPurpFrom_B01ID == 16 ~ '6', # Other non-escort
    TripPurpFrom_B01ID == 17 ~ '99', # Escort home
    TripPurpFrom_B01ID == 18 ~ '1', # Escort work
    TripPurpFrom_B01ID == 19 ~ '2', # Escort in course of work
    TripPurpFrom_B01ID == 20 ~ '3', # Escort education
    TripPurpFrom_B01ID == 21 ~ '4', # Escort shopping / personal business
    TripPurpFrom_B01ID == 22 ~ '7', # Other escort
    TripPurpFrom_B01ID == 23 ~ '99', # Home
    TRUE ~ as.character('unclassified')
  ))

# TODO: This will need editing to match hb_purpose
# Recode trip purposes as NTEM classification nhb_purpose for NHB trips
unclassified_build <- unclassified_build %>%
  mutate(nhb_purpose = case_when(
    TripPurpTo_B01ID == 1 ~ '12', # Work
    TripPurpTo_B01ID == 2 ~ '12', # In course of work
    TripPurpTo_B01ID == 3 ~ '13', # Education
    TripPurpTo_B01ID == 4 ~ '14', # Food shopping
    TripPurpTo_B01ID == 5 ~ '14', # Non food shopping
    TripPurpTo_B01ID == 6 ~ '15', # Personal business medical
    TripPurpTo_B01ID == 7 ~ '15', # Personal business eat / drink
    TripPurpTo_B01ID == 8 ~ '15', # Personal business other
    TripPurpTo_B01ID == 9 ~ '16', # Eat / drink with friends
    TripPurpTo_B01ID == 10 ~ '16', # Visit friends
    TripPurpTo_B01ID == 11 ~ '16', # Other social
    TripPurpTo_B01ID == 12 ~ '16', # Entertain /  public activity
    TripPurpTo_B01ID == 13 ~ '16', # Sport: participate
    TripPurpTo_B01ID == 14 ~ '18', # Holiday: base
    TripPurpTo_B01ID == 15 ~ '18', # Day trip / just walk
    TripPurpTo_B01ID == 16 ~ '16', # Other non-escort
    TripPurpTo_B01ID == 17 ~ '99', # Escort home
    TripPurpTo_B01ID == 18 ~ '12', # Escort work
    TripPurpTo_B01ID == 19 ~ '12', # Escort in course of work
    TripPurpTo_B01ID == 20 ~ '13', # Escort education
    TripPurpTo_B01ID == 21 ~ '14', # Escort shopping / personal business
    TripPurpTo_B01ID == 22 ~ '16', # Other escort
    TripPurpTo_B01ID == 23 ~ '99', # Home
    TRUE ~ as.character('unclassified')
  ))

# Pick a final purpose depending on purpose from
unclassified_build <- unclassified_build %>%
  mutate(trip_purpose = case_when(
    trip_origin == 'hb' ~ hb_purpose,
    trip_origin == 'nhb' ~ nhb_purpose,
    TRUE ~ as.character('unclassified')
  ))

# drop 'unclassified' hb_purpose as cannot be used in regression by trip_purpose
unclassified_build <- subset(unclassified_build, trip_purpose != 'unclassified')

# Recode exploratory variable classifications -----------------------------------------

# Recode gender(Sex_B01ID) as gender(Male or Female)
unclassified_build <- unclassified_build %>%
  mutate(gender = case_when(
    Sex_B01ID == 1 ~ 'Male',
    Sex_B01ID == 2 ~ 'Female',
    TRUE ~ as.character(Sex_B01ID)
  ))

# Combine Age and work status to age_workstatus (excluding 75+ AND pte/fte/stu as insignificant N (<0.2% combined))
unclassified_build <- unclassified_build %>%
  mutate(age_work_status = case_when(
    Age_B01ID %in% c(1,2,3,4,5) ~ '0-16_child',
    Age_B01ID %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18) & EcoStat_B01ID %in% c(1,3) ~ '16-74_fte',
    Age_B01ID %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18) & EcoStat_B01ID %in% c(2,4) ~ '16-74_pte',
    Age_B01ID %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18) & EcoStat_B01ID == 7 ~ '16-74_stu',
    Age_B01ID %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18) & EcoStat_B01ID %in% c(5,6,8,9,10,11) ~ '16-74_unm',
    Age_B01ID %in% c(19,20,21) & EcoStat_B01ID %in% c(5,6,8,9,10,11) ~ '75+_retired',
    TRUE ~ as.character('unclassified')
  )) %>% subset(age_work_status != 'unclassified')

# Recode number of cars(NumCarVan_B02ID) as cars
unclassified_build <- unclassified_build %>%
  mutate(cars = case_when(
    NumCarVan_B02ID == 1 ~ '0',
    NumCarVan_B02ID == 2 ~ '1',
    NumCarVan_B02ID == 3 ~ '2+',
    TRUE ~ as.character(NumCarVan_B02ID)
  ))

# Adapt number of cars available to NTEM methodology
unclassified_build <- unclassified_build %>%
  mutate(cars = case_when(
    HHoldNumAdults == 1 & cars == '1' ~ '1+',
    HHoldNumAdults == 1 & cars == '2+' ~ '1+',
    TRUE ~ as.character(cars)
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

unclassified_build %>%
  filter(trip_purpose == 8) %>%
  count(main_mode)

# Set time period params
am_peak <- c(8,9,10)
inter_peak <- c(11,12,13,14,15,16)
pm_peak <- c(17,18,19)
off_peak <- c(1,2,3,4,5,6,7,20,21,22,23,24)

# Create start time based on day(TravDay) and time period(TripTravTime)
unclassified_build <- unclassified_build %>%
  mutate(start_time = case_when(
    TravDay %in% c(1,2,3,4,5) & TripStart_B01ID %in% am_peak ~ '1', # Weekday AM peak
    TravDay %in% c(1,2,3,4,5) & TripStart_B01ID %in% inter_peak ~ '2', # Weekday IP peak
    TravDay %in% c(1,2,3,4,5) & TripStart_B01ID %in% pm_peak ~ '3', # Weekday PM peak
    TravDay %in% c(1,2,3,4,5) & TripStart_B01ID %in% off_peak ~ '4', # Offpeak
    TravDay %in% c(6) ~ '5', # Saturday
    TravDay %in% c(7) ~ '6', # Sunday
    TRUE ~ as.character('unclassified')
  ))

# Create end time based on day(TravDay) and time period(TripTravTime)
unclassified_build <- unclassified_build %>%
  mutate(end_time = case_when(
    TravDay %in% c(1,2,3,4,5) & TripEnd_B01ID %in% am_peak ~ '1', # Weekday AM peak
    TravDay %in% c(1,2,3,4,5) & TripEnd_B01ID %in% inter_peak ~ '2', # Weekday IP peak
    TravDay %in% c(1,2,3,4,5) & TripEnd_B01ID %in% pm_peak ~ '3', # Weekday PM peak
    TravDay %in% c(1,2,3,4,5) & TripEnd_B01ID %in% off_peak ~ '4', # Offpeak
    TravDay %in% c(6) ~ '5', # Saturday
    TravDay %in% c(7) ~ '6', # Sunday
    TRUE ~ as.character('unclassified')
  ))

# Drop unclassified time_period - apply only for time_period regression?
# unclassified_build <- subset(unclassified_build, start_time != 'unclassified' | end_time != 'unclassified')

# Append tfn area types
unclassified_build <- unclassified_build %>%
  left_join(new_area_types, by = c('HHoldOSWard_B01ID'='uk_ward_zones')) %>%
  rename('tfn_area_type' = new_area_type) %>%
  drop_na(tfn_area_type)

# Transform catgeorical variables to factors
#facts <- c("hb_purpose", "nhb_purpose", "nhb_purpose_hb_leg", "age_work_status", "gender", "hh_adults", "cars", "tfn_area_type", "area_type", "soc_cat", "ns_sec", "main_mode", "start_time", "end_time")
#unclassified_build <- unclassified_build %>% mutate_at(facts, funs(factor))

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

# Re-weight short-walk trips by a factor of 7
walk_trips <- unclassified_build %>%
  select(IndividualID, trip_purpose, W5xHh, main_mode, TripDisIncSW) %>%
  filter(TripDisIncSW < 1, main_mode == 1 & trip_purpose != 8) %>%
  mutate(W5xHh = 7)

# Replace updated short walk trips with new trip weights
setDT(unclassified_build)             
setDT(walk_trips) 

unclassified_build[walk_trips, on = c("IndividualID", "trip_purpose", "main_mode", "TripDisIncSW"), W5xHh := i.W5xHh]
unclassified_build <- as_tibble(unclassified_build)

#unclassified_build %>% write_csv("Y:/NTS/classified_nts_walk_infill.csv")

# Apply Ian Williams Weighting methodology
weighted_trip_rates <- unclassified_build %>%
  filter(trip_purpose %in% c(1:8)) %>%
  mutate(trip_weights = W1 * W5xHh * W2) %>%
  group_by(IndividualID, trip_purpose, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, W2) %>%
  summarise(trip_weights = sum(trip_weights),
            weekly_trips = n())%>%
  ungroup() %>%
  mutate(trip_rate = trip_weights/W2,
         trip_purpose = as.integer(trip_purpose)) %>%
  select(-trip_weights, -W2)

trip_rates_export <- weighted_trip_rates %>%
  complete(nesting(IndividualID, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type),
           trip_purpose = 1:8,
           fill = list(weekly_trips = 0, trip_rate = 0))

trip_rates_export %>% write_csv("C:/Users/Pluto/Documents/Trip_rate_testing/trip_rate_model_input_test.csv")
### Load libraries and import data ##############################################
library("tidyverse")

# Import unclassified build
unclassified_build <- read_csv("Y:/NTS/tfn_unclassified_build.csv")

backup_df <- unclassified_build

# Import new area_type classification of wards
new_area_types <- read_csv("Y:/NTS/new area type lookup/new_area_type.csv")



### Prepare dataset #############################################################


# Classify trip origin to get HB/NHB splits, drop trip origin NA
hb <- c(23)
nhb <- c(1,2,3,4,5,6,7,8,9,
         10,11,12,13,14,15,
         16,17,18,19,20,21,22)

unclassified_build <- unclassified_build %>%
  mutate(trip_origin = case_when(
    TripPurpFrom_B01ID %in% hb ~ 'hb',
    TripPurpFrom_B01ID %in% nhb ~ 'nhb',
    TRUE ~ as.character(NA) 
  )) %>%
  filter(trip_origin != is.na(trip_origin))


# Recode trip purposes as NTEM classification hb_purpose for HB trips
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
    TRUE ~ '-8'
  ))


# Drop hb_purpose = 99 - Return leg for hb trips and -99 (NA)
unclassified_build <- unclassified_build %>% filter((hb_purpose != '99') & (hb_purpose != '-8'))

# Pick a final purpose depending on purpose from & drop unclassified trip_purpose
unclassified_build <- unclassified_build %>%
  mutate(trip_purpose = case_when(
    trip_origin == 'hb' ~ hb_purpose,
    TRUE ~ as.character('unclassified')
  )) %>%
  filter(trip_purpose != 'unclassified')


# Recode gender(Sex_B01ID), children are gender neutral in NTEM
unclassified_build <- unclassified_build %>%
  mutate(gender = case_when(
    Age_B01ID %in% c(1,2,3,4,5) ~ 'Child',
    Sex_B01ID == 1 ~ 'Male',
    Sex_B01ID == 2 ~ 'Female',
    TRUE ~ as.character(Sex_B01ID)
  ))

# Combine Age and work status to age_workstatus (excluding 75+ AND pte/fte/stu as insignificant N (<0.2% combined))
unclassified_build <- unclassified_build %>%
  mutate(age_work_status = case_when(
    Age_B01ID %in% c(1,2,3,4,5) ~ '0-16_child',
    Age_B01ID %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18) & EcoStat_B01ID %in% c(1,2) ~ '16-74_fte',
    Age_B01ID %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18) & EcoStat_B01ID %in% c(3,4) ~ '16-74_pte',
    Age_B01ID %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18) & EcoStat_B01ID == 7 ~ '16-74_stu',
    Age_B01ID %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18) & EcoStat_B01ID %in% c(5,6,8,9,10,11) ~ '16-74_unm',
    Age_B01ID %in% c(19,20,21) & EcoStat_B01ID %in% c(5,6,8,9,10,11) ~ '75+_retired',
    TRUE ~ '-8'
  )) %>% filter(age_work_status != '-8')


# Recode number of cars(NumCarVan_B02ID) as cars and drop -8
unclassified_build <- unclassified_build %>%
  mutate(cars = case_when(
    NumCarVan_B02ID == 1 ~ '0',
    NumCarVan_B02ID == 2 ~ '1',
    NumCarVan_B02ID == 3 ~ '1',
    TRUE ~ as.character(NumCarVan_B02ID)
  )) %>%
  filter(cars != '-8')


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
  )) %>%
  filter(area_type != '-8')

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
    TRUE ~ '-8'
  )) %>%
  filter(main_mode != '-8' & main_mode != '99')

# Add new area_type (
unclassified_build <- unclassified_build %>% left_join(new_area_types, by = c("HHoldOSWard_B01ID" = "uk_ward_zones"))

# Drop NA area type (problem: significant number of NA for HHoldOSWard_B01ID, n = 135495 out of 1959837 (~7%)
unclassified_build <- unclassified_build %>% filter(new_area_type != is.na(new_area_type))
# Mutate area type 1 and 2 into area_type 1_2
unclassified_build <- unclassified_build %>%
  mutate(new_area_type = case_when(
    new_area_type %in% c(1,2) ~ '1_2',
        TRUE ~ as.character(new_area_type)
  ))

# Transform catgeorical variables to factors
facts <- c("hb_purpose", "cars", "area_type", "new_area_type",  "main_mode")
unclassified_build <- unclassified_build %>% mutate_at(facts, funs(factor))


# Weight trips, calculate trips by mode and mode split, export csv
mode_df <- unclassified_build %>%
  mutate(weighted_trips = W1 * W2 * W5xHh) %>%
  select(IndividualID, hb_purpose, cars, new_area_type, main_mode, weighted_trips, W2) %>%
  group_by(IndividualID, hb_purpose, cars, new_area_type, main_mode, W2) %>%
  summarise(weighted_trips = sum(weighted_trips, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(trips = weighted_trips/W2) %>%
  select(hb_purpose, cars, new_area_type, main_mode, trips) %>%
  group_by(hb_purpose, cars, new_area_type, main_mode, .drop = FALSE) %>%
  summarise(mode_trips = sum(trips, na.rm = TRUE)) %>%
  group_by(hb_purpose, cars, new_area_type, .drop = FALSE) %>%
  mutate(total_trips = sum(mode_trips)) %>%
  mutate(mode_split = mode_trips/total_trips) %>%
  write_csv("Y:/NTS/mode_time_splits/20-05-2020 area type 1+2 combined/hb_mode_split.csv")


# Transpose and export csv  
#pmd_df_transposed <- pmd_df %>%
#  arrange(main_mode) %>% pivot_wider(id_cols=hb_purpose:new_area_type, names_from = main_mode, values_from = mode_split, values_fill = list(mode_split = 0)) %>%
#  write_csv("Y:/NTS/mode_time_splits/20-05-2020 area type 1+2 combined/hb_mode_split_transposed.csv")





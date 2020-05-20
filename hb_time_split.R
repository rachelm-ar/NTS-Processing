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
    TRUE ~ '-8'
    )) %>%
  filter(cars != '-8')


# Recode number of adults in household(HHoldNumAdults) as hh_adults
unclassified_build <- unclassified_build %>%
  mutate(hh_adults = case_when(
    HHoldNumAdults == 1 ~ '1', # 1 Adult
    HHoldNumAdults == 2 ~ '2', # 2 Adults
    HHoldNumAdults >= 3 ~ '3+', # 3+ Adults
    TRUE ~ '-8'
  )) %>%
  filter(hh_adults != '-8')


# Adapt number of cars available to NTEM methodology
unclassified_build <- unclassified_build %>%
  mutate(cars = case_when(
    HHoldNumAdults == 1 & cars == '1' ~ '1+',
    HHoldNumAdults == 1 & cars == '2+' ~ '1+',
    TRUE ~ '-8'
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
    TRUE ~ '-9'
  ))


# NS-Sec already in correct classification, rename for legibility
unclassified_build <- unclassified_build %>%
  rename(ns_sec = NSSec_B03ID)

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
    TRUE ~ as.character('-8')
  )) %>%
  filter(start_time != '-8')

# Lookup NTEM traveller type

# Unite variables to create traveller type
unclassified_build <- unclassified_build %>%
  unite("traveller_type", "age_work_status", "gender", "hh_adults", "cars", remove=TRUE, sep="_")

NTEM_types <- list(c(1, 2, 3, 4, 5,  6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
                     25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
                     53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
                     81, 82, 83, 84, 85, 86, 87, 88))

traveller_type <- list(c("0-16_child_Child_1_0", "0-16_child_Child_1_1+", "0-16_child_Child_2_0", "0-16_child_Child_2_1", "0-16_child_Child_2_2+",
                         "0-16_child_Child_3+_0", "0-16_child_Child_3+_1", "0-16_child_Child_3+_2+",
                         "16-74_fte_Male_1_0", "16-74_fte_Male_1_1+", "16-74_fte_Male_2_0", "16-74_fte_Male_2_1", "16-74_fte_Male_2_2+",
                         "16-74_fte_Male_3+_0", "16-74_fte_Male_3+_1", "16-74_fte_Male_3+_2+", "16-74_pte_Male_1_0", "16-74_pte_Male_1_1+", "16-74_pte_Male_2_0",
                         "16-74_pte_Male_2_1", "16-74_pte_Male_2_2+", "16-74_pte_Male_3+_0", "16-74_pte_Male_3+_1", "16-74_pte_Male_3+_2+", "16-74_stu_Male_1_0",
                         "16-74_stu_Male_1_1+", "16-74_stu_Male_2_0", "16-74_stu_Male_2_1", "16-74_stu_Male_2_2+", "16-74_stu_Male_3+_0", "16-74_stu_Male_3+_1",
                         "16-74_stu_Male_3+_2+", "16-74_unm_Male_1_0", "16-74_unm_Male_1_1+", "16-74_unm_Male_2_0", "16-74_unm_Male_2_1", "16-74_unm_Male_2_2+",
                         "16-74_unm_Male_3+_0", "16-74_unm_Male_3+_1", "16-74_unm_Male_3+_2+", "75+_retired_Male_1_0", "75+_retired_Male_1_1+", "75+_retired_Male_2_0",
                         "75+_retired_Male_2_1", "75+_retired_Male_2_2+", "75+_retired_Male_3+_0", "75+_retired_Male_3+_1", "75+_retired_Male_3+_2+", "16-74_fte_Female_1_0",
                         "16-74_fte_Female_1_1+", "16-74_fte_Female_2_0", "16-74_fte_Female_2_1", "16-74_fte_Female_2_2+", "16-74_fte_Female_3+_0", "16-74_fte_Female_3+_1",
                         "16-74_fte_Female_3+_2+", "16-74_pte_Female_1_0", "16-74_pte_Female_1_1+", "16-74_pte_Female_2_0", "16-74_pte_Female_2_1", "16-74_pte_Female_2_2+",
                         "16-74_pte_Female_3+_0", "16-74_pte_Female_3+_1", "16-74_pte_Female_3+_2+", "16-74_stu_Female_1_0", "16-74_stu_Female_1_1+", "16-74_stu_Female_2_0",
                         "16-74_stu_Female_2_1", "16-74_stu_Female_2_2+", "16-74_stu_Female_3+_0", "16-74_stu_Female_3+_1", "16-74_stu_Female_3+_2+", "16-74_unm_Female_1_0",
                         "16-74_unm_Female_1_1+", "16-74_unm_Female_2_0", "16-74_unm_Female_2_1", "16-74_unm_Female_2_2+", "16-74_unm_Female_3+_0", "16-74_unm_Female_3+_1",
                         "16-74_unm_Female_3+_2+", "75+_retired_Female_1_0", "75+_retired_Female_1_1+", "75+_retired_Female_2_0", "75+_retired_Female_2_1", "75+_retired_Female_2_2+",
                         "75+_retired_Female_3+_0", "75+_retired_Female_3+_1", "75+_retired_Female_3+_2+"))

NTEM_lookup_table <- as.data.frame(map2_dfr(traveller_type, NTEM_types, ~ tibble(traveller_type = .x, NTEM_types = .y)))

### add ntem_traveller_type
unclassified_build <- unclassified_build %>% left_join(NTEM_lookup_table)


# Add new area_typ
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
facts <- c("hb_purpose", "NTEM_types", "area_type", "new_area_type", "soc_cat", "ns_sec", "main_mode", "start_time")
unclassified_build <- unclassified_build %>% mutate_at(facts, funs(factor))


# Weight trips, calculate trips by time period and time split, export csv
time_df <- unclassified_build %>%
  mutate(weighted_trips = W1 * W2 * W5xHh) %>%
  select(IndividualID, hb_purpose, NTEM_types, new_area_type, start_time, weighted_trips, W2) %>%
  group_by(IndividualID, hb_purpose, NTEM_types, new_area_type, start_time, W2) %>%
  summarise(weighted_trips = sum(weighted_trips, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(trips = weighted_trips/W2) %>%
  select(hb_purpose, NTEM_types, new_area_type, start_time, trips) %>%
  group_by(hb_purpose, NTEM_types, new_area_type, start_time, .drop = FALSE) %>%
  summarise(tp_trips = sum(trips, na.rm = TRUE)) %>%
  group_by(hb_purpose, NTEM_types, new_area_type, .drop = FALSE) %>%
  mutate(total_trips = sum(tp_trips)) %>%
  mutate(time_split = tp_trips/total_trips) %>%
  write_csv("Y:/NTS/mode_time_splits/20-05-2020 area type 1+2 combined/hb_time_split.csv")



# Transpose and export csv  
#pmd_df_transposed <- pmd_df %>%
#  arrange(main_mode) %>% pivot_wider(id_cols=hb_purpose:new_area_type, names_from = main_mode, values_from = mode_split, values_fill = list(mode_split = 0)) %>%
#  write_csv("Y:/NTS/mode_time_splits/20-05-2020 area type 1+2 combined/hb_mode_split_transposed.csv")


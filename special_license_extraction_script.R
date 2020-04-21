
# Code purpose: Get trips from NTS data
# Become adaptable for different purposes, with same base code
# Export at csv
# Port to Secure lab PC to streamline working process
# Can only be used with access to special license dataset from UK Data Service.

require(tidyverse)

# Path to tab files - should be SPSS equivalent?
file <- 'D:/UKDA-7553-tab/tab'
export <- 'Y:/NTS/'

attitudes_file_path = paste0(file, '/attitudesspecial2017_protect.tab')
days_file_path = paste0('/dayspecial2017_protect.tab')
individual_file_path <- paste0(file, '/individualspecial2017_protect.tab')
psu_id_file_path <- paste0(file, '/psuspecial2017_protect.tab')
household_file_path <- paste0(file, '/householdspecial2017_protect.tab')
trip_file_path <- paste0(file, '/tripspecial2017_protect.tab')
stage_file_path <- paste0(file, '/stagesecure2017_protect.tab')

# TODO: Separate join to put trip and stage together for occupancy factors

column_method <- 'ntem'

# Col definitions ####

psu_cols <- c('PSUID', 'PSUPopDensity')

# For the purposes of NTEM replication, we only need the following:
full_household_cols = c('HouseholdID', # Unique Household Identifier - Second Heirarchical Variable
                        'PSUID', # Joining variable
                        'SurveyYear', # We want to know the year
                        'OutCom_B02ID', # Competitiveness of household
                        'HHIncome2002_B01ID', # 23 household income bands
                        'HHoldAreaType1_B01ID', # 15 categories of Household area type
                        'HHoldNumAdults', # Number of adults in household
                        'NumCar', # Number of 3/4 wheelers (Excl Land Rover and Jeep)
                        'NumMCycle', # Number of Motorcycles
                        'NumVanLorry', # Number of vans/lorries
                        'NumCarVan_B02ID', # Number of household cars/light vans incl. landrover, jeep, minibus etc in NTEM bandings
                        'WalkRail', # Walk time from household to nearest rail station
                        'StationKmTRACC', # KM to nearest rail station by road
                        'DescTa_B01ID', # Frequency of rail service at nearest railway station
                        'HHoldAreaType2_B01ID', # Household Area Type - Settlement Size
                        'W1',
                        'W2'
)

ntem_household_cols <- c('PSUID',
                         'SurveyYear',
                         'HouseholdID',
                         'HHoldOSWard_B01ID',
                         'HHoldOSLAUA_B01ID',
                         'OutCom_B02ID',
                         'HHoldAreaType1_B01ID',
                         'HHoldNumAdults',
                         'NumCarVan_B02ID',
                         'W1',
                         'W2')

full_individual_cols = c('IndividualID', # Unique individual identifier - third heirarchical variable 
                         'HouseholdID', # Joining variable
                         'PSUID', # Joining variable
                         'PersNo', # Person number within household
                         'Age_B01ID', # Age of person
                         'Sex_B01ID', # Gender of person 
                         'XSOC2000_B02ID', # Standard occupational classification
                         'NSSec_B03ID', # National Statistics Socio-Economic Classification of individual - high level
                         'IndIncome2002_B02ID', # Individual income in three bands
                         'EcoStat_B01ID') # Working status of individual

# For the purposes of NTEM replication, we the following will do:

ntem_individual_cols = c('PSUID',
                         'HouseholdID',
                         'IndividualID',
                         'Age_B01ID',
                         'Sex_B01ID',
                         'XSOC2000_B02ID', # Standard occupational classification
                         'NSSec_B03ID', # National Statistics Socio-Economic Classification of individual - high level
                         'EcoStat_B01ID')

npr_individual_cols <- c('PSUID',
                         'HouseholdID',
                         'IndividualID',
                         'Age_B01ID',
                         'Sex_B01ID',
                         'XSOC2000_B02ID', # Standard occupational classification
                         'NSSec_B03ID', # National Statistics Socio-Economic Classification of individual - high level
                         'EcoStat_B01ID')

full_trip_cols = c('TripID', # Unique Trip ID
                   'DayID', # ID given to all trips made by an indicidual on a given travel day
                   'IndividualID', # Joining variable
                   'HouseholdID', # Joining variable
                   'PSUID', # Joining variable
                   'PersNo', # Joining variable
                   'TravDay', # Day of travel week
                   'TripPurpose_B01ID', # Trip Purpose
                   'NumStages_B01ID', # Number of trip stages in bands
                   'TripDisIncSW', # Trip distance, in miles
                   'TripStart_B02ID', # 51 bands of trip start time
                   'TripEnd_B02ID', # 51 bands of trip end time
                   'TripTravTime', # Total travel time in minutes
                   'W5', # Trip/Stage weight 
                   'W5xHh' # Trip/Stage weight excluding household
)

# TODO: NTM purpose banding could be instructive: TripPurpose_B07ID

ntem_trip_cols = c('PSUID',
                   'HouseholdID',
                   'IndividualID',
                   'TripID',
                   'TravDay',
                   'MainMode_B04ID',
                   'TripPurpFrom_B01ID',
                   'TripPurpTo_B01ID',
                   'TripStart_B01ID', # 51 bands of trip start time
                   'TripEnd_B01ID',
                   'TripDisIncSW',
                   'TripTravTime',
                   'TripOrigUrbCd_B01ID',
                   'TripDestUrbCd_B01ID',
                   'TripOrigCounty_B01ID',
                   'TripDestCounty_B01ID',
                   'TripOrigGOR_B02ID',
                   'TripDestGOR_B02ID',
                   'W5',
                   'W5xHh')

# Imports ####
# NTS is organised in a hierarchical structure. This starts with PSUs:
# The column we will need is:

psu_df <- read_delim(psu_id_file_path, delim = "\t", guess_max = 400000) %>%
  select(psu_cols)

# The fully completed measure is not available for the older survey data.
# It used to be in a variable called OutCom_B01ID - but is removed from all but the last 3 years
# The solution is to remove the 2s for the last 3 years, compare completed to uncompleted and filter out
# older surveys that look uncompleted

# Import
household_df <- read_delim(household_file_path, delim = "\t", guess_max = 400000) %>%
  select(ntem_household_cols)

year_com_count <- household_df %>%
  select(SurveyYear, OutCom_B02ID) %>%
  group_by(SurveyYear, OutCom_B02ID) %>%
  count()

# The third variable is individuals. 
individual_df <- read_delim(individual_file_path, delim = "\t", guess_max = 400000) %>%
  select(npr_individual_cols)

# The fourth variable is trips
# Columns of interest:

trip_df <- read_delim(trip_file_path, delim = "\t", guess_max = 4000000) %>%
  select(ntem_trip_cols)

# Table Joins ####
# We now join these tables together using the Heirarchical variables

nts_df <- psu_df %>%
  left_join(household_df, by = 'PSUID') %>%
  filter(!is.na(HouseholdID)) # Lots of NULL due to the household competitiveness - remove.

nts_df <- nts_df %>%
  left_join(individual_df, by = c('PSUID', 'HouseholdID'))

nts_df <- nts_df %>%
  left_join(trip_df, by=c('PSUID', 'HouseholdID', 'IndividualID'))

nts_df %>% write_csv(paste0(export, '/tfn_unclassified_build.csv'))

# Variable classifications ####
# We now need to process the dataframe so the NTS segmentation matches that of NTEM. 
# Specifically, we need to line up the NTS data with NTEM purposes, area types and traveller types

# Traveller Types

# Convert Age_B01ID to NTEM types

nts_df <- nts_df %>%
  mutate(Age_B01ID = case_when(
    Age_B01ID == 1 ~ 'under 16',
    Age_B01ID == 2 ~ 'under 16',
    Age_B01ID == 3 ~ 'under 16',
    Age_B01ID == 4 ~ 'under 16',
    Age_B01ID == 5 ~ 'under 16',
    Age_B01ID == 6 ~ '16-74',
    Age_B01ID == 7 ~ '16-74',
    Age_B01ID == 8 ~ '16-74',
    Age_B01ID == 9 ~ '16-74', 
    Age_B01ID == 10 ~ '16-74', 
    Age_B01ID == 11 ~ '16-74',
    Age_B01ID == 12 ~ '16-74',
    Age_B01ID == 13 ~ '16-74',
    Age_B01ID == 14 ~ '16-74', 
    Age_B01ID == 15 ~ '16-74', 
    Age_B01ID == 16 ~ '16-74', 
    Age_B01ID == 17 ~ '16-74', 
    Age_B01ID == 18 ~ '16-74',
    Age_B01ID == 19 ~ '75 or over',
    Age_B01ID == 20 ~ '75 or over',
    Age_B01ID == 21 ~ '75 or over',
    TRUE ~ as.character(Age_B01ID)
  ))

audit_age <- nts_df %>%
  group_by(Age_B01ID) %>%
  count()

# We now Convert 'Sex_B01ID' to gender types
nts_df <- nts_df %>%
  mutate(Sex_B01ID = case_when(
    Sex_B01ID == 1 ~ 'Male',
    Sex_B01ID == 2 ~ 'Females',
    TRUE ~ as.character(Sex_B01ID)
  ))

audit_sex <- nts_df %>%
  group_by(Sex_B01ID) %>%
  count()

# NB children are genderless in NTEM
nts_df <- nts_df %>%
  mutate(Sex_B01ID = case_when(
    Age_B01ID == 'under 16' ~ 'Children',
    TRUE ~ as.character(Sex_B01ID)
  ))

audit_sex <- nts_df %>%
  group_by(Sex_B01ID) %>%
  count()

# We convert ''HHoldNumAdults' to NTEM household occupancy
nts_df <- nts_df %>%
  mutate(HHoldNumAdults = case_when(
    HHoldNumAdults == 1 ~ '1 Adult',
    HHoldNumAdults == 2 ~ '2 Adults',
    HHoldNumAdults >= 3 ~ '3+ Adults',
    TRUE ~ as.character(HHoldNumAdults)
    ))

audit_adults <- nts_df %>%
  group_by(HHoldNumAdults) %>%
  count()

# We convert EcoStat_B01ID to NTEM employment types
nts_df <- nts_df %>%
  mutate(EcoStat_B01ID = case_when(
    EcoStat_B01ID == 1 ~ 'fte',
    EcoStat_B01ID == 2 ~ 'pte',
    EcoStat_B01ID == 3 ~ 'fte', 
    EcoStat_B01ID == 4 ~ 'pte',
    EcoStat_B01ID == 5 ~ 'unm',
    EcoStat_B01ID == 6 ~ 'unm',
    EcoStat_B01ID == 7 ~ 'stu',
    EcoStat_B01ID == 8 ~ 'unm',
    EcoStat_B01ID == 9 ~ 'unm',
    EcoStat_B01ID == 10 ~ 'unm',
    EcoStat_B01ID == 11 ~ 'unm',
    TRUE ~ as.character(EcoStat_B01ID)
  ))

# use age to get non-working pops
nts_df <- nts_df %>%
  mutate(EcoStat_B01ID = case_when(
    Age_B01ID == 'under 16' ~ 'non_wa',
    Age_B01ID == '75 or over' ~ 'non_wa',
    TRUE ~ as.character(EcoStat_B01ID)
  ))

# -9 = Did not answer
audit_employment <- nts_df %>%
  group_by(EcoStat_B01ID) %>%
  count()

# Cars are converted based on the following dictionary

audit_car_van <- nts_df %>%
  group_by(NumCarVan_B02ID) %>%
  count()

nts_df <- nts_df %>%
  mutate(NumCarVan_B02ID = case_when(
    NumCarVan_B02ID == 1 ~ '0',
    NumCarVan_B02ID == 2 ~ '1',
    NumCarVan_B02ID == 3 ~ '2+',
    TRUE ~ as.character(NumCarVan_B02ID)
  ))

nts_df <- nts_df %>%
  mutate(NumCarVan_B02ID = case_when(
    HHoldNumAdults == '1 Adult' & NumCarVan_B02ID == '1' ~ '1+',
    HHoldNumAdults == '1 Adult' & NumCarVan_B02ID == '2+' ~ '1+',
    TRUE ~ as.character(NumCarVan_B02ID)
  ))

# -8 == NA
audit_car_van <- nts_df %>%
  group_by(NumCarVan_B02ID) %>%
  count()

# Classify trip origin to get HB/NHB splits

hb <- c(23)
nhb <- c(1,2,3,4,5,6,7,8,9,
         10,11,12,13,14,15,
         16,17,18,19,20,21,22)

nts_df <- nts_df %>%
  mutate(trip_origin = case_when(
    TripPurpFrom_B01ID %in% hb ~ 'hb',
    TripPurpFrom_B01ID %in% nhb ~ 'nhb',
    TRUE ~ as.character(NA)
  ))

audit_trip_origin <- nts_df %>%
  group_by(trip_origin) %>%
  count()

nts_df <- nts_df %>%
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

audit_hb_purpose <- nts_df %>%
  group_by(hb_purpose) %>%
  count()

nts_df <- nts_df %>%
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

audit_nhb_purpose <- nts_df %>%
  group_by(nhb_purpose) %>%
  count()

# Pick a final purpose depending on purpose from
nts_df <- nts_df %>%
  mutate(trip_purpose = case_when(
    trip_origin == 'hb' ~ hb_purpose,
    trip_origin == 'nhb' ~ nhb_purpose,
    TRUE ~ as.character('unclassified')
  ))

audit_purpose <- nts_df %>%
  group_by(trip_purpose) %>%
  count()

# Set time period params
am_peak <- c(8,9,10)
inter_peak <- c(11,12,13,14,15,16)
pm_peak <- c(17,18,19)
off_peak <- c(1,2,3,4,5,6,7,20,21,22,23,24)

# Convert start time TripStart_B01ID to NTEM time period
nts_df <- nts_df %>%
  mutate(TripStart_B01ID = case_when(
    TravDay == 6 ~ '5',
    TravDay == 7 ~ '6',
    TripStart_B01ID %in% am_peak ~ '1',
    TripStart_B01ID %in% inter_peak ~ '2',
    TripStart_B01ID %in% pm_peak ~ '3',
    TripStart_B01ID %in% off_peak ~ '4',
    TRUE ~ as.character(TripStart_B01ID)
  ))

# Convert end time TripEnd_B01ID to NTEM time period
nts_df <- nts_df %>%
  mutate(TripEnd_B01ID = case_when(
    TravDay == 6 ~ '5',
    TravDay == 7 ~ '6',
    TripEnd_B01ID %in% am_peak ~ '1',
    TripEnd_B01ID %in% inter_peak ~ '2',
    TripEnd_B01ID %in% pm_peak ~ '3',
    TripEnd_B01ID %in% off_peak ~ '4',
    TRUE ~ as.character(TripEnd_B01ID)
  ))

# Classify MainMode_B04ID into NTEM modes w/ Car aggregation
nts_df <- nts_df %>%
  mutate(MainMode_B04ID = case_when(
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

# Convert HHoldAreaType1_B01ID to Area Types
nts_df <- nts_df %>%
  mutate(HHoldAreaType1_B01ID = case_when(
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

# -8 NA
audit_area_type <- nts_df %>%
  group_by(HHoldAreaType1_B01ID) %>%
  count()

# Convert SOC Types
nts_df <- nts_df %>%
  mutate(XSOC2000_B02ID = case_when(
    XSOC2000_B02ID == 1 ~ '1', # 1	Managers and senior officials
    XSOC2000_B02ID == 2 ~ '1', # 2	Professional occupations
    XSOC2000_B02ID == 3 ~ '2', # 3	Associate professional and technical occupations
    XSOC2000_B02ID == 4 ~ '2', # 4	Administrative and secretarial occupations
    XSOC2000_B02ID == 5 ~ '2', # 5	Skilled trades occupations
    XSOC2000_B02ID == 6 ~ '3', # 6	Personal service occupations
    XSOC2000_B02ID == 7 ~ '3', # 7	Sales and customer service occupations
    XSOC2000_B02ID == 8 ~ '3', # 8	Process, plant and machine operatives
    XSOC2000_B02ID == 9 ~ '3', # 9	Elementary occupations
    TRUE ~ as.character(XSOC2000_B02ID)
  ))


# Also mit
# -8	10	2	NA
# -9	11	2	DNA

# NS-Sec is already in the right categories
# Proof that there's only 1 NS-Sec in a given household
ns_sec_is_household <- nts_df %>%
  select(HouseholdID, NSSec_B03ID) %>%
  distinct() %>%
  group_by(HouseholdID, NSSec_B03ID) %>%
  count() %>%
  filter(n == max(n))

# Clean up to NTEM style variable names

nts_df <- nts_df %>%
  rename(age = Age_B01ID,
         gender = Sex_B01ID,
         household = HHoldNumAdults,
         cars = NumCarVan_B02ID,
         employment = EcoStat_B01ID,
         area_type = HHoldAreaType1_B01ID,
         soc_cat = XSOC2000_B02ID,
         ns_sec = NSSec_B03ID,
         main_mode = MainMode_B04ID)

# Build NTEM dataframe
traveller_type <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                    21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,
                    38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,
                    55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,
                    72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88)
age <- c('under 16','under 16','under 16','under 16','under 16','under 16','under 16','under 16',
         '16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74',
         '16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74',
         '16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74',
         '75 or over','75 or over','75 or over','75 or over','75 or over','75 or over','75 or over',
         '75 or over','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74',
         '16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74',
         '16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','16-74','75 or over',
         '75 or over','75 or over','75 or over','75 or over','75 or over','75 or over','75 or over')
gender <- c('Children','Children','Children','Children','Children','Children','Children','Children',
            'Male','Male','Male','Male','Male','Male','Male','Male','Male','Male','Male','Male','Male',
            'Male','Male','Male','Male','Male','Male','Male','Male','Male','Male','Male','Male','Male',
            'Male','Male','Male','Male','Male','Male','Male','Male','Male','Male','Male','Male','Male',
            'Male','Females','Females','Females','Females','Females','Females','Females','Females','Females',
            'Females','Females','Females','Females','Females','Females','Females','Females','Females','Females',
            'Females','Females','Females','Females','Females','Females','Females','Females','Females','Females',
            'Females','Females','Females','Females','Females','Females','Females','Females','Females','Females',
            'Females')
household <- c('1 Adult','1 Adult','2 Adults','2 Adults','2 Adults','3+ Adults','3+ Adults','3+ Adults','1 Adult',
               '1 Adult','2 Adults','2 Adults','2 Adults','3+ Adults','3+ Adults','3+ Adults','1 Adult','1 Adult',
               '2 Adults','2 Adults','2 Adults','3+ Adults','3+ Adults','3+ Adults','1 Adult','1 Adult','2 Adults',
               '2 Adults','2 Adults','3+ Adults','3+ Adults','3+ Adults','1 Adult','1 Adult','2 Adults','2 Adults',
               '2 Adults','3+ Adults','3+ Adults','3+ Adults','1 Adult','1 Adult','2 Adults','2 Adults','2 Adults',
               '3+ Adults','3+ Adults','3+ Adults','1 Adult','1 Adult','2 Adults','2 Adults','2 Adults','3+ Adults',
               '3+ Adults','3+ Adults','1 Adult','1 Adult','2 Adults','2 Adults','2 Adults','3+ Adults','3+ Adults',
               '3+ Adults','1 Adult','1 Adult','2 Adults','2 Adults','2 Adults','3+ Adults','3+ Adults','3+ Adults',
               '1 Adult','1 Adult','2 Adults','2 Adults','2 Adults','3+ Adults','3+ Adults','3+ Adults','1 Adult',
               '1 Adult','2 Adults','2 Adults','2 Adults','3+ Adults','3+ Adults','3+ Adults')
cars <- c('0','1+','0','1','2+','0','1','2+','0','1+','0','1','2+','0','1','2+','0','1+','0','1','2+','0','1','2+',
          '0','1+','0','1','2+','0','1','2+','0','1+','0','1','2+','0','1','2+','0','1+','0','1','2+','0','1','2+',
          '0','1+','0','1','2+','0','1','2+','0','1+','0','1','2+','0','1','2+','0','1+','0','1','2+','0','1','2+',
          '0','1+','0','1','2+','0','1','2+','0','1+','0','1','2+','0','1','2+')
household_composition <- c(1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,
                           1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,
                           1,2,3,4,5,6,7,8)
employment <- c('non_wa','non_wa','non_wa','non_wa','non_wa','non_wa','non_wa','non_wa','fte','fte','fte','fte',
                'fte','fte','fte','fte','pte','pte','pte','pte','pte','pte','pte','pte','stu','stu','stu','stu',
                'stu','stu','stu','stu','unm','unm','unm','unm','unm','unm','unm','unm','non_wa','non_wa','non_wa',
                'non_wa','non_wa','non_wa','non_wa','non_wa','fte','fte','fte','fte','fte','fte','fte','fte','pte',
                'pte','pte','pte','pte','pte','pte','pte','stu','stu','stu','stu','stu','stu','stu','stu','unm',
                'unm','unm','unm','unm','unm','unm','unm','non_wa','non_wa','non_wa','non_wa','non_wa','non_wa','non_wa','non_wa')

# Convert row data to table
ntem_tt <- tibble(traveller_type, age, gender, household, cars, household_composition, employment)

# Join NTEM segments - 
nts_ntem_df <- nts_df %>%
  left_join(ntem_tt)

# Export processed data for analysis
nts_ntem_df %>%
  write_csv(export, '/tfn_ntem_build.csv')

# END
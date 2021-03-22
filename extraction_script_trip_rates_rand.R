
# Code purpose: Get trips from NTS data
# Become adaptable for different purposes, with same base code
# Export at csv
# Port to Secure lab PC to streamline working process
# Can only be used with access to special license dataset from UK Data Service.

require(tidyverse)

# Path to tab files - should be SPSS equivalent?
file <- 'C:/Users/genie/Documents/UKDA-7553-tab/tab'
export <- 'C:/Users/genie/Documents'

attitudes_file_path = paste0(file, '/attitudes_special_2002-2019_protect.tab')
days_file_path = paste0(file, '/day_special_2002-2019_protect.tab')
individual_file_path <- paste0(file, '/individual_special_2002-2019_protect.tab')
psu_id_file_path <- paste0(file, '/psu_special_2002-2019_protect.tab')
household_file_path <- paste0(file, '/household_special_2002-2019_protect.tab')
ldj_file_path <- paste0(file, '/ldj_special_2002-2019_protect.tab')
trip_file_path <- paste0(file, '/trip_special_2002-2019_protect.tab')
stage_file_path <- paste0(file, '/stage_special_2002-2019_protect.tab')

# TODO: Separate join to put trip and stage together for occupancy factors

column_method <- 'ntem'

# Col definitions ####

psu_cols <- c('PSUID', 'PSUPopDensity')

household_cols <- c('PSUID',
                    'SurveyYear',
                    'HouseholdID',
                    'HHoldGOR_B02ID',
                    'HHoldOSLAUA_B01ID',
                    'HHoldCountry_B01ID',
                    'HHIncome2002_B01ID',
                    'HHIncQISEngTS_B01ID',
                    'HHIncQDS2005_B01ID',
                    'HHoldCVAvail_B01ID',
                    'W1',
                    'W2',
                    'W3')

# For the purposes of NTEM replication, we the following will do:
individual_cols <- c('PSUID',
                     'HouseholdID',
                     'IndividualID') # National Statistics Socio-Economic Classification of individual - high level

# TODO: NTM purpose banding could be instructive: TripPurpose_B07ID

trip_cols = c('PSUID',
              'HouseholdID',
              'IndividualID',
              'DayID',
              'TripID',
              'TripDestCounty_B01ID',
              'TripDestUA2009_B01ID',
              'TripOrigGOR_B02ID',
              'TripOrigUA2009_B01ID',
              'MainMode_B04ID',
              'TripDisExSW',
              'TripPurpFrom_B01ID',
              'TripPurpose_B01ID',
              'TripPurpTo_B01ID',
              'TravDay',
              'TripStart',
              'TripTotalTime',
              'W5',
              'W5xHH')

ldj_cols <- c('PSUID',
              'IndividualID',
              'HouseholdID',
              'TripID',
              'LDJID',
              'LDJDestGOR_B02ID',
              'LDJDestCounty_B01ID',
              'LDJDestUA2009_B01ID',
              'LDJOrigCounty_B01ID',
              'LDJOrigGOR_B02ID',
              'LDJOrigUA2009_B01ID',
              'LDJDistance',
              'LDJMode_B02ID',
              'LDJMode_B03ID',
              'LDJPurpFrom_B01ID',
              'LDJPurpose_B01ID',
              'LDJPurpose_B02ID',
              'LDJPurpTo_B01ID',
              'LDJMonth',
              'LDJMonth_B01ID',
              'LDJWeekDay_B01ID',
              'W4')

# Target survey years
tsy <- c(2015, 2016, 2017, 2018, 2019)

days_cols = c('PSUID',
              'IndividualID',
              'HouseholdID',
              'DayID',
              'TravDay',
              'TravelMonth_B01ID',
              'TravelDayType_B01ID',
              'TravelWeekDay_B01ID',
              'TravelWeekDay_B02ID')

# TODO: NTM purpose banding could be instructive: TripPurpose_B07ID

# Imports ####
# NTS is organised in a hierarchical structure. This starts with PSUs:
# The column we will need is:

psu_df <- read_delim(psu_id_file_path, delim = "\t", guess_max = 1000) %>%
  select(psu_cols)

# The fully completed measure is not available for the older survey data.
# It used to be in a variable called OutCom_B01ID - but is removed from all but the last 3 years
# The solution is to remove the 2s for the last 3 years, compare completed to uncompleted and filter out
# older surveys that look uncompleted

# Import household
household_df <- read_delim(household_file_path, delim = "\t", guess_max = 1000) %>%
  select(household_cols)

# import individuals. 
individual_df <- read_delim(individual_file_path, delim = "\t", guess_max = 1000) %>%
  select(individual_cols)

days_df <- read_delim(days_file_path, delim = "\t", guess_max = 1000) %>%
  select(days_cols)

#  Import trips
# Columns of interest:
trip_df <- read_delim(trip_file_path, delim = "\t", guess_max = 1000) %>%
  select(trip_cols)

# import ldj
ldj_df <- read_delim(ldj_file_path, delim='\t', guess_max = 1000) %>%
  select(ldj_cols)

# Table Joins ####
# We now join these tables together using the Heirarchical variables

# Work out len


nts_df <- psu_df %>%
  left_join(household_df, by = 'PSUID') %>%
  filter(!is.na(HouseholdID)) # Lots of NULL due to the household competitiveness - remove.
rm(psu_df)
rm(household_df)
gc()

nts_df <- nts_df %>%
  left_join(individual_df, by = c('PSUID', 'HouseholdID'))
rm(individual_df)
gc()

nts_df <- nts_df %>%
  left_join(days_df, by=c('PSUID', 'HouseholdID', 'IndividualID'))
rm(days_df)
gc()

nts_df <- nts_df %>%
  left_join(trip_df, by=c('PSUID', 'HouseholdID', 'IndividualID', 'DayID'))
rm(trip_df)
gc()

nts_df <- nts_df %>%
  left_join(ldj_df, by=c('PSUID', 'HouseholdID', 'IndividualID', 'TripID'))
rm(ldj_df)
gc()

# Filter SurveyYear
nts_df <- nts_df %>%
  filter(SurveyYear %in% tsy)

nts_df %>% write_csv(paste0(export, '/tfn_rand_ve_analytical_build_15-19.csv'))

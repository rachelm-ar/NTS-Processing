
# Code purpose: Get trips from NTS data
# Become adaptable for different purposes, with same base code
# Export at csv
# Port to Secure lab PC to streamline working process
# Can only be used with access to special license dataset from UK Data Service.

require(tidyverse)

# Path to tab files - should be SPSS equivalent?
file <- 'D:/UKDA-7553-tab/tab'
export <- 'D:/NTS/'

attitudes_file_path = paste0(file, '/attitudesspecial2017_protect.tab')
days_file_path = paste0('/dayspecial2017_protect.tab')
individual_file_path <- paste0(file, '/individualspecial2017_protect.tab')
psu_id_file_path <- paste0(file, '/psuspecial2017_protect.tab')
household_file_path <- paste0(file, '/householdspecial2017_protect.tab')
ldj_file_path <- paste0(file, '/ldjspecial2017_protect.tab')
trip_file_path <- paste0(file, '/tripspecial2017_protect.tab')
stage_file_path <- paste0(file, '/stagesecure2017_protect.tab')

# TODO: Separate join to put trip and stage together for occupancy factors

column_method <- 'ntem'

# Col definitions ####

psu_cols <- c('PSUID', 'PSUPopDensity')

household_cols <- c('PSUID',
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

# For the purposes of NTEM replication, we the following will do:
individual_cols <- c('PSUID',
                     'HouseholdID',
                     'IndividualID',
                     'Age_B01ID',
                     'Sex_B01ID',
                     'XSOC2000_B02ID', # Standard occupational classification
                     'NSSec_B03ID', # National Statistics Socio-Economic Classification of individual - high level
                     'SIC1992_B02ID',
                     'CarAccess_B01ID',
                     'DrivLic_B02ID',
                     'EcoStat_B01ID')

# TODO: NTM purpose banding could be instructive: TripPurpose_B07ID

trip_cols = c('PSUID',
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
              'TripDestUA2009_B01ID',
              'TripOrigUA2009_B01ID',
              'TripOrigAreaType1_B01ID',
              'TripOrigAreaType2_B01ID',
              'TripDestAreaType1_B01ID',
              'TripDestAreaType2_B01ID',
              'W5',
              'W5xHh')

ldj_cols <- c('PSUID',
              'IndividualID',
              'HouseholdID',
              'TripID',
              'LDJID',
              'W4',
              'LDJPurpFrom_B01ID')

stage_cols <- c('PSUID',
                'IndividualID',
                'HouseholdID',
                'TripID',
                'StageID',
                'NumParty_B01ID',
                'StageMode_B04ID',
                'StageDistance_B01ID')

# Imports ####
# NTS is organised in a hierarchical structure. This starts with PSUs:
# The column we will need is:

psu_df <- read_delim(psu_id_file_path, delim = "\t", guess_max = 400000) %>%
  select(psu_cols)

# The fully completed measure is not available for the older survey data.
# It used to be in a variable called OutCom_B01ID - but is removed from all but the last 3 years
# The solution is to remove the 2s for the last 3 years, compare completed to uncompleted and filter out
# older surveys that look uncompleted

# Import household
household_df <- read_delim(household_file_path, delim = "\t", guess_max = 400000) %>%
  select(household_cols)

year_com_count <- household_df %>%
  select(SurveyYear, OutCom_B02ID) %>%
  group_by(SurveyYear, OutCom_B02ID) %>%
  count()

# import individuals. 
individual_df <- read_delim(individual_file_path, delim = "\t", guess_max = 400000) %>%
  select(individual_cols)

#  Import trips
# Columns of interest:
trip_df <- read_delim(trip_file_path, delim = "\t", guess_max = 4000000) %>%
  select(trip_cols)

# import ldj
ldj_df <- read_delim(ldj_file_path, delim='\t', guess_max = 400000) %>%
  select(ldj_cols)

stage_df <- read_delim(stage_file_path, delim='\t', guess_max = 5000000) %>%
  select(stage_cols)

# Table Joins ####
# We now join these tables together using the Heirarchical variables

nts_df <- psu_df %>%
  left_join(household_df, by = 'PSUID') %>%
  filter(!is.na(HouseholdID)) # Lots of NULL due to the household competitiveness - remove.

nts_df <- nts_df %>%
  left_join(individual_df, by = c('PSUID', 'HouseholdID'))

nts_df <- nts_df %>%
  left_join(trip_df, by=c('PSUID', 'HouseholdID', 'IndividualID'))

nts_df <- nts_df %>%
  left_join(ldj_df, by=c('PSUID', 'HouseholdID', 'IndividualID', 'TripID'))

# LDJ TEST
ldj_purp_count <- nts_df %>%
  select(LDJID) %>%
  group_by(LDJID) %>%
  count()

nts_df <- nts_df %>%
  left_join(stage_df, by=c('PSUID', 'HouseholdID', 'IndividualID', 'TripID'))

nts_df %>% write_csv(paste0(export, '/tfn_unclassified_build.csv'))

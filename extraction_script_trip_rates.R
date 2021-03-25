# Code purpose: Get trips from NTS data
# Become adaptable for different purposes, with same base code
# Export at csv
# Port to Secure lab PC to streamline working process
# Can only be used with access to special license dataset from UK Data Service.

require(tidyverse)

# Path to tab files - should be SPSS equivalent?
file <- 'C:/Users/Pluto/Documents/NTS/UKDA-7553-tab/tab'
export <- 'C:/Users/Pluto/Documents/NTS'

attitudes_file_path = paste0(file, '/attitudes_special_2002-2019_protect.tab')
days_file_path = paste0(file, '/day_special_2002-2019_protect.tab')
individual_file_path <- paste0(file, '/individual_special_2002-2019_protect.tab')
psu_id_file_path <- paste0(file, '/psu_special_2002-2019_protect.tab')
household_file_path <- paste0(file, '/household_special_2002-2019_protect.tab')
ldj_file_path <- paste0(file, '/ldj_special_2002-2019_protect.tab')
trip_file_path <- paste0(file, '/trip_special_2002-2019_protect.tab')
stage_file_path <- paste0(file, '/stage_special_2002-2019_protect.tab')

table_cols <- read_csv("Y:/NTS/import/extraction_cols_hb_trip_rates.csv")

# TODO: build function in utils.r that will lapply over to avoid repetition
psu_cols <- table_cols %>% pull(psu_cols) %>% discard(is.na) 

household_cols <- table_cols %>% pull(household_cols) %>% discard(is.na) 

individual_cols <- table_cols %>% pull(individual_cols) %>% discard(is.na) 

days_cols <- table_cols %>% pull(days_cols) %>% discard(is.na) 

trip_cols <- table_cols %>% pull(trip_cols) %>% discard(is.na) 

ldj_cols <- table_cols %>% pull(ldj_cols) %>% discard(is.na) 

stage_cols <- table_cols %>% pull(stage_cols) %>% discard(is.na) 

psu_df <- read_delim(psu_id_file_path, delim = "\t", guess_max = 1000) %>%
  select(psu_cols)

### Read in df's and select columns

household_df <- read_delim(household_file_path, delim = "\t", guess_max = 1000) %>%
  select(household_cols)

individual_df <- read_delim(individual_file_path, delim = "\t", guess_max = 1000) %>%
  select(individual_cols)

days_df <- read_delim(days_file_path, delim = "\t", guess_max = 1000) %>%
  select(days_cols)

trip_df <- read_delim(trip_file_path, delim = "\t", guess_max = 1000) %>%
  select(trip_cols)


psu_df <- read_delim(psu_id_file_path, delim = "\t", guess_max = 1000) %>%
  select(psu_cols)

nts_df <- psu_df %>%
  left_join(household_df, by = 'PSUID') %>%
  filter(!is.na(HouseholdID)) # Lots of NULL due to the household competitiveness - remove.

nts_df <- nts_df %>%
  left_join(individual_df, by = c('PSUID', 'HouseholdID'))

nts_df <- nts_df %>%
  left_join(days_df, by=c('PSUID', 'HouseholdID', 'IndividualID'))

nts_df <- nts_df %>%
  left_join(trip_df, by=c('PSUID', 'HouseholdID', 'IndividualID', 'DayID'))

nts_df %>%
  na.omit() %>% 
  write_csv(paste0(export, '/tfn_unclassified_build_no_stage19.csv'))
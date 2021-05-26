read_custom_select <- function(file_path, col_name, column_subsets){
  
  select_cols <- column_subsets %>%
    pull(col_name) %>%
    discard(is.na)
  
  read_delim(file_path, delim = "\t", guess_max = 1000) %>%
    select(all_of(select_cols))
  
  
}

create_ub <- function(username = user,
                      extract_version,
                      drive,
                      tsy = 2002:2019){
  
  # Load libraries
  library_list <- c("stringr",
                    "readr",
                    "dplyr",
                    "purrr")
  
  library_c(library_list)
  
# Paths -------------------------------------------------------------------
  
  nts_c_dir <- str_c("C:/Users/", username, "/Documents/NTS_C/")
  
  import_dir <- str_c(nts_c_dir, "UKDA-7553-tab/tab/")
  
  nts_dir <- ifelse(drive == "C", nts_c_dir, "Y:/NTS/")
  
  column_subsets_dir <- str_c(nts_dir, "import/ub_columns/extraction_cols_", extract_version , ".csv")
  
  # Paths
  attitudes_path <- str_c(import_dir, 'attitudes_special_2002-2019_protect.tab')
  days_file_path <-  str_c(import_dir, 'day_special_2002-2019_protect.tab')
  individual_file_path <- str_c(import_dir, 'individual_special_2002-2019_protect.tab')
  psu_id_file_path <- str_c(import_dir, 'psu_special_2002-2019_protect.tab')
  household_file_path <- str_c(import_dir, 'household_special_2002-2019_protect.tab')
  ldj_file_path <- str_c(import_dir, 'ldj_special_2002-2019_protect.tab')
  trip_file_path <- str_c(import_dir, 'trip_special_2002-2019_protect.tab')
  stage_file_path <- str_c(import_dir, 'stage_special_2002-2019_protect.tab')
  
  # export dir
  export_dir <- str_c(nts_c_dir, "unclassified builds/ub_", extract_version, ".csv")
  
# Read in -----------------------------------------------------------------
  
  column_subsets <- read_csv(column_subsets_dir)
  
  psu_df <- read_custom_select(psu_id_file_path, "psu_cols", column_subsets)
  household_df <- read_custom_select(household_file_path, "household_cols", column_subsets)
  individual_df <- read_custom_select(individual_file_path, "individual_cols", column_subsets)
  days_df <- read_custom_select(days_file_path, "days_cols", column_subsets)
  trip_df <- read_custom_select(trip_file_path, "trip_cols", column_subsets)
  ldj_df <- read_custom_select(ldj_file_path, "ldj_cols", column_subsets)
  stage_df <- read_custom_select(stage_file_path, "stage_cols", column_subsets)
  
  nts_df <- psu_df %>%
    left_join(household_df, by = 'PSUID') %>%
    filter(!is.na(HouseholdID)) # Lots of NULL due to the household competitiveness - remove.
  
  nts_df <- nts_df %>%
    left_join(individual_df, by = c('PSUID', 'HouseholdID'))
  
  nts_df <- nts_df %>%
    left_join(days_df, by=c('PSUID', 'HouseholdID', 'IndividualID'))
  
  nts_df <- nts_df %>%
    left_join(trip_df, by=c('PSUID', 'HouseholdID', 'IndividualID', 'DayID'))
  
  nts_df <- nts_df %>%
    left_join(ldj_df, by = c('PSUID', 'HouseholdID', 'IndividualID', 'TripID'))
  
  # nts_df <- nts_df %>%
  #  left_join(stage_df)
  
  nts_df <- nts_df %>%
    filter(SurveyYear %in% tsy)
    
  #  nts_df %>% 
  #  rename(ns = NSSec_B03ID) %>%
  #  mutate(ns = ifelse(ns == -9, 6, ns)) %>% 
  #  group_by(HouseholdID) %>%
  #  mutate(ns = min(ns)) %>%
  #  ungroup() %>%
  #  mutate(ns = ifelse(ns == 6, 5, ns)) %>%
  #  write_csv(export_dir)
  
  nts_df %>% 
    write_csv(export_dir)
  
}

nts_df %>% 
    rename(ns = NSSec_B03ID) %>%
    mutate(ns = ifelse(ns == -9, 6, ns)) %>% 
    group_by(HouseholdID) %>%
    mutate(ns = min(ns)) %>%
    ungroup() %>%
    mutate(ns = ifelse(ns == 6, 5, ns)) %>%
  count(SurveyYear, ns)

rand_df <- read_csv("Y:/NTS/unclassified builds/ub_RAND.csv")

rand_df %>%
  count(SurveyYear, ns) %>%
  print(n=40)

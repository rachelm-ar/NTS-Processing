extract_raw_nts <- function(import_dir,
                            export_dir,
                            extract_version,
                            extract_name,
                            tsy){
  
  # Custom function to install if not already and load
  library_c("tidyverse")
  
  # Paths
  attitudes_path <- str_c(import_dir, '/attitudes_special_2002-2019_protect.tab')
  days_file_path <-  str_c(import_dir, '/day_special_2002-2019_protect.tab')
  individual_file_path <- str_c(import_dir, '/individual_special_2002-2019_protect.tab')
  psu_id_file_path <- str_c(import_dir, '/psu_special_2002-2019_protect.tab')
  household_file_path <- str_c(import_dir, '/household_special_2002-2019_protect.tab')
  ldj_file_path <- str_c(import_dir, '/ldj_special_2002-2019_protect.tab')
  trip_file_path <- str_c(import_dir, '/trip_special_2002-2019_protect.tab')
  stage_file_path <- str_c(import_dir, '/stage_special_2002-2019_protect.tab')
  
  column_subsets_dir <- str_c("Y:/NTS/import/ub_columns/extraction_cols_", 
                              extract_version , ".csv")
  
  ### Read in
  column_subsets <- read_csv(column_subsets_dir)
  
  # TODO: build function in utils.r that will lapply over to avoid repetition
  psu_cols <- column_subsets %>% 
    pull(psu_cols) %>% 
    discard(is.na) 
  
  household_cols <- column_subsets %>% 
    pull(household_cols) %>% 
    discard(is.na) 
  
  individual_cols <- column_subsets %>% 
    pull(individual_cols) %>% 
    discard(is.na) 
  
  days_cols <- column_subsets %>% 
    pull(days_cols) %>% 
    discard(is.na) 
  
  trip_cols <- column_subsets %>% 
    pull(trip_cols) %>% 
    discard(is.na) 
  
  ldj_cols <- column_subsets %>% 
    pull(ldj_cols) %>% 
    discard(is.na) 
  
  stage_cols <- column_subsets %>% 
    pull(stage_cols) %>% 
    discard(is.na) 
  
  psu_df <- read_delim(psu_id_file_path, delim = "\t", guess_max = 1000) %>% 
    select(psu_cols)
  
  household_df <- read_delim(household_file_path, delim = "\t", guess_max = 1000) %>%
    select(household_cols)
  
  individual_df <- read_delim(individual_file_path, delim = "\t", guess_max = 1000) %>%
    select(individual_cols)
  
  days_df <- read_delim(days_file_path, delim = "\t", guess_max = 1000) %>%
    select(days_cols)
  
  trip_df <- read_delim(trip_file_path, delim = "\t", guess_max = 1000) %>%
    select(trip_cols)
  
  nts_df <- psu_df %>%
    left_join(household_df, by = 'PSUID') %>%
    filter(!is.na(HouseholdID)) # Lots of NULL due to the household competitiveness - remove.
  
  nts_df <- nts_df %>%
    left_join(individual_df, by = c('PSUID', 'HouseholdID'))
  
  nts_df <- nts_df %>%
    left_join(days_df, by=c('PSUID', 'HouseholdID', 'IndividualID'))
  
  nts_df <- nts_df %>%
    left_join(trip_df, by=c('PSUID', 'HouseholdID', 'IndividualID', 'DayID'))
  
  if(!missing(tsy)) nts_df <- filter(nts_df, SurveyYear %in% tsy)
  
  nts_df %>% 
    na.omit() %>% 
  write_csv(str_c(export_dir, extract_name, '.csv'))
  
}

library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(vroom)
library(purrr)

save_processed = TRUE
version = "hb_trip_rates"

# Initialise Directories/Read CSVS/Source R scripts ----------------------------

# Change this to your username and redirect to github folder if not in documents
username <- Sys.info()[[6]]

# NTS folder
nts_dir <- "Y:/NTS/"

# Import folder
import_dir <- str_c(nts_dir, "import/")

# Export folder
export_dir <- str_c(import_dir, "classified builds")

# Column names
column_names_dir <- str_c(import_dir, "variables_hb_trip_rates.csv")
column_names <- read_csv(column_names_dir)
column_names <- pull(column_names)

# Read in unclassified build
unclassified_build_dir <- str_c(import_dir, "tfn_unclassified_build_no_stage19.csv")
unclassified_build <- vroom(unclassified_build_dir, col_select = column_names)

lookup_dir <- str_c(nts_dir, "lookups/")
lookups_dir <- str_c(paste0("C:/Users/", username, "/Documents/GitHub/NTS-Processing/lookups.R"))

if(file.exists(lookups_dir)){
  
  source(lookups_dir)
  
} else {
  
  stop("Github Folder not in documents - add custom path 'lookups_dir' to lookups.R in NTS Processing")
  
}

# Classify Purposes -------------------------------------------------------
unclassified_build <- unclassified_build %>%
  lu_trip_origin() %>% 
  lu_hb_purpose() %>%
  lu_nhb_purpose() %>% 
  lu_nhb_purpose_hb_leg() %>% 
  mutate(trip_purpose = ifelse(trip_origin == "hb", hb_purpose, nhb_purpose))

# Classify Other variables ------------------------------------------------
unclassified_build <- unclassified_build %>%
  lu_gender() %>% 
  lu_age_work_status() %>% 
  lu_cars() %>% 
  lu_hh_adults() %>% 
  lu_soc_cat() %>%
  lu_main_mode() %>%
  lu_start_time() %>% 
  lu_end_time() %>%
  lu_tfn_area_type() %>%
  mutate(ns_sec = ifelse(NSSec_B03ID == -9, 99, NSSec_B03ID))

# Short Walk Weighting
unclassified_build <- unclassified_build %>%
  mutate(sw_weight = ifelse(TripDisIncSW < 1 & main_mode == 1, 7, 1))

# Save classified build
if(save_processed) write_csv(unclassified_build, paste0("C:/Users/", username, "/Documents/NTS/classified_build.csv"))

if(version == "hb_trip_rates"){
  
  weighted_trip_rates <- classified_build_pre_weight %>% 
    filter(trip_purpose %in% 1:8) %>%
    mutate(trip_weight = W1 * sw_weight * W5 * W2) %>% 
    group_by(IndividualID, trip_purpose, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, W2) %>% 
    summarise(trip_weights = sum(trip_weight),
              weekly_trips = sum(W1 * sw_weight)) %>% 
    ungroup() %>% 
    mutate(trip_rate = trip_weights)
  
  classified_build_weight <- weighted_trip_rates %>%
    mutate(trip_purpose = as.integer(trip_purpose)) %>% 
    complete(nesting(IndividualID, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type),
             trip_purpose = 1:8,
             fill = list(weekly_trips = 0, trip_weights = 0, trip_rate = 0, W2 = 0))
  
  write_csv(classified_build_weight, paste0("C:/Users/", username, "/Documents/NTS/hb_trip_rates_build.csv"))
  
} else if(version == "car_availability"){
  
  car_availability <- classified_build_pre_weight %>% 
    select(EcoStat_B01ID, NumCarVan_B02ID, HHoldNumAdults, HHoldOSWard_B01ID, tfn_area_type, W1, W3) %>% 
    group_by(EcoStat_B01ID, HHoldOSWard_B01ID, HHoldNumAdults, NumCarVan_B02ID, tfn_area_type) %>% 
    summarise(weighted_count = sum(W1 * W3),
              count = n()) %>% 
    ungroup() %>% 
    arrange(EcoStat_B01ID, HHoldOSWard_B01ID, HHoldNumAdults, tfn_area_type)
  
  write_csv(car_availability, paste0("C:/Users/", username, "/Documents/NTS/hb_trip_rates_build.csv"))
  
}
  

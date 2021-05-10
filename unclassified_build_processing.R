define_nts_audit_params <- function(nts_dat){
  ###
  # Takes a fresh NTS import and defines params for audit
  # Just gives survey year as a starter
  ###
  
  unq_syear <- nts_dat %>%
    select(SurveyYear) %>%
    count(SurveyYear)
  
  return(unq_syear)
  
}

cb_preprocess <- function(ub){
  
  # Post codes
  ub <- ub %>% 
    mutate(PSUPSect = str_replace(PSUPSect, "^(.*\\s.).*", "\\1"),
           PSUPSect = str_replace_all(PSUPSect, " ", "")) 
  
  # NS SEC
  ub %>% 
    rename(ns = NSSec_B03ID) %>%
    mutate(ns = ifelse(ns == -9, 6, ns)) %>% 
    group_by(HouseholdID) %>%
    mutate(ns = min(ns)) %>%
    ungroup() %>%
    mutate(ns = ifelse(ns == 6, 5, ns))
  
}

classify_nts <- function(user,
                         cb_version,
                         build_type,
                         drive,
                         save_processed = FALSE){

  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr",
                    "purrr")
 
  library_c(library_list)

# Directories -------------------------------------------------------------
  
  # Imports
  y_dir <- "Y:/NTS/"
  c_dir <- str_c("C:/Users/", user, "/Documents/NTS_C/")
  nts_dir <- ifelse(drive == "Y", y_dir, c_dir)
  
  ub_dir <- str_c(nts_dir, "unclassified builds/ub_", cb_version, ".csv")
  cb_columns_dir <- str_c(nts_dir, "import/cb_columns/cb_vars_", cb_version, ".csv")
  
  # Exports
  export_dir <- str_c(nts_dir, "classified builds/")
  dir.create(export_dir, showWarnings = FALSE)
  
  out_cb_dir <- str_c(export_dir, "cb_", cb_version, ".csv")
  out_hb_tr_dir <- str_c(export_dir, "cb_hb trip rates.csv")
  out_ca_dir <- str_c(export_dir, "cb_ca.csv")
  
  # Unclassified build
  ub <- read_csv(ub_dir)
  
  # Columns subset
  cb_columns <- read_csv(cb_columns_dir) %>% pull()
  
  ub <- select(ub, all_of(cb_columns))
  
  # Define audit params
  # TODO: Expand this to cover more key variables, give acceptance parameters
  nts_audit <- define_nts_audit_params(ub)

 # Pre-processing ----------------------------------------------------------
  
  # All non-lookup pre processing
  ub <- cb_preprocess(ub)
  
  # TODO: Check function after steps, return nominal
  # audit1 <- check_nts_processing(ub, nts_audit)
  ub %>% count(SurveyYear)
  
  # Classify Purposes -------------------------------------------------------
  ub <- ub %>%
    lu_trip_origin() %>% 
    lu_hb_purpose() %>%
    lu_nhb_purpose() %>% 
    lu_nhb_purpose_hb_leg() %>% 
    mutate(trip_purpose = ifelse(trip_origin == "hb", hb_purpose, nhb_purpose))
  
  ub %>% 
    select(trip_purpose, TripPurpFrom_B01ID, TripPurpTo_B01ID, trip_origin, TripPurpose_B01ID) %>%
    filter(TripPurpFrom_B01ID == 17) %>% count(TripPurpTo_B01ID) %>% print(n=30) # %>% filter(TripPurpTo_B01ID == 17)
  
  # TODO: Checkfunction after steps return nominal
  ub %>% count(SurveyYear)
  
  # Classify Other variables ------------------------------------------------
  ub <- ub %>%
    lu_gender() %>%
    lu_age_work_status() %>%
    lu_hh_type() %>%
    lu_soc() %>%
    lu_traveller_type() %>%
    lu_main_mode() %>%
    lu_start_time() %>%
    lu_end_time() %>%
    lu_tfn_area_type() %>%
    lu_sw_weight()
  
  ub %>% count(SurveyYear)

  if(save_processed) write_csv(ub, out_cb_dir)
  
  if(build_type == "hb_trip_rates"){
    
    weighted_trip_rates <- ub %>% 
      filter(trip_purpose %in% 1:8) %>%
      mutate(trip_weight = W1 * sw_weight * W5 * W2) %>% 
      group_by(IndividualID, trip_purpose, SurveyYear, age_work_status, gender, hh_type, soc, ns, tfn_area_type, W2) %>% 
      summarise(trip_weights = sum(trip_weight),
                weekly_trips = sum(W1 * sw_weight)) %>% 
      ungroup() %>% 
      mutate(trip_rate = trip_weights)
    
    hb_trip_rates_build <- weighted_trip_rates %>%
      mutate(trip_purpose = as.integer(trip_purpose)) %>% 
      complete(nesting(IndividualID, SurveyYear, age_work_status, gender, hh_type, soc, ns, tfn_area_type),
               trip_purpose = 1:8,
               fill = list(weekly_trips = 0, trip_weights = 0, trip_rate = 0, W2 = 0))
    
    write_csv(hb_trip_rates_build, out_hb_tr_dir)
    
  } else if(build_type == "car_ownership"){
    
    # TODO: New method
    
    #car_availability <- ub %>% 
    #  select(EcoStat_B01ID, NumCarVan, HHoldNumAdults, HHoldOSWard_B01ID, tfn_area_type, W1, W3) %>% 
    #  group_by(EcoStat_B01ID, HHoldOSWard_B01ID, HHoldNumAdults, NumCarVan, tfn_area_type) %>% 
    #  summarise(weighted_count = sum(W1 * W3),
    #            count = n()) %>% 
    #  ungroup() %>% 
    #  arrange(EcoStat_B01ID, HHoldOSWard_B01ID, HHoldNumAdults, tfn_area_type)
    #
    #write_csv(car_availability, out_ca_dir)
    
  }
  
}

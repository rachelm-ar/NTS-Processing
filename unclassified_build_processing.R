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

classify_nts <- function(user,
                         ub_name,
                         cb_columns_name,
                         build_type,
                         drive,
                         out_cb_name,
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
  
  ub_dir <- str_c(nts_dir, "unclassified builds/", ub_name, ".csv")
  cb_columns_dir <- str_c(nts_dir, "import/cb_columns/", cb_columns_name, ".csv")
  
  # Exports
  export_dir <- str_c(nts_dir, "classified builds/")
  dir.create(export_dir, showWarnings = FALSE)
  
  out_cb_dir <- str_c(export_dir, out_cb_name, ".csv")
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

  # Reformat Postcode and short walk weighting
  ub <- ub %>% 
    mutate(PSUPSect = str_replace(PSUPSect, "^(.*\\s.).*", "\\1"),
           PSUPSect = str_replace_all(PSUPSect, " ", "")) %>% 
    mutate(sw_weight = ifelse(TripDisIncSW < 1 & MainMode_B04ID == 1, 7, 1))
  
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
  
  # TODO: Checkfunction after steps return nominal
  ub %>% count(SurveyYear)
  
  # Classify Other variables ------------------------------------------------
  ub <- ub %>%
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
  
  ub %>% count(SurveyYear)

  if(save_processed) write_csv(ub, out_cb_dir)
  
  if(build_type == "hb_trip_rates"){
    
    weighted_trip_rates <- ub %>% 
      filter(trip_purpose %in% 1:8) %>%
      mutate(trip_weight = W1 * sw_weight * W5 * W2) %>% 
      group_by(IndividualID, trip_purpose, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, W2) %>% 
      summarise(trip_weights = sum(trip_weight),
                weekly_trips = sum(W1 * sw_weight)) %>% 
      ungroup() %>% 
      mutate(trip_rate = trip_weights)
    
    hb_trip_rates_build <- weighted_trip_rates %>%
      mutate(trip_purpose = as.integer(trip_purpose)) %>% 
      complete(nesting(IndividualID, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type),
               trip_purpose = 1:8,
               fill = list(weekly_trips = 0, trip_weights = 0, trip_rate = 0, W2 = 0))
    
    write_csv(hb_trip_rates_build, out_hb_tr_dir)
    
  } else if(build_type == "car_ownership"){
    
    car_availability <- ub %>% 
      select(EcoStat_B01ID, NumCarVan_B02ID, HHoldNumAdults, HHoldOSWard_B01ID, tfn_area_type, W1, W3) %>% 
      group_by(EcoStat_B01ID, HHoldOSWard_B01ID, HHoldNumAdults, NumCarVan_B02ID, tfn_area_type) %>% 
      summarise(weighted_count = sum(W1 * W3),
                count = n()) %>% 
      ungroup() %>% 
      arrange(EcoStat_B01ID, HHoldOSWard_B01ID, HHoldNumAdults, tfn_area_type)
    
    write_csv(car_availability, out_ca_dir)
    
  }
  
}

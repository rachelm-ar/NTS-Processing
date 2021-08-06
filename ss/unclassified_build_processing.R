define_nts_audit_params <- function(nts_dat){
  ###
  # Takes a fresh NTS import and defines params for audit
  # Just gives survey year as a starter
  ###
  
  unq_syear <- nts_dat %>%
    count(SurveyYear)
  
  return(unq_syear)
  
}

cb_preprocess <- function(ub, cb_version){
  
  # Post codes
  ub <- ub %>% 
    mutate(PSUPSect = str_replace(PSUPSect, "^(.*\\s.).*", "\\1"),
           PSUPSect = str_replace_all(PSUPSect, " ", "")) 
  
  if(cb_version != "ntem"){
    
    # NS SEC
    ub <- ub %>% 
      rename(ns = NSSec_B03ID) %>%
      mutate(ns = ifelse(ns == -9, 6, ns)) %>% 
      group_by(HouseholdID) %>%
      mutate(ns = min(ns)) %>%
      ungroup() %>%
      mutate(ns = ifelse(ns == 6, 5, ns))
    
  }
  
  return(ub)
    
}

create_cb <- function(user,
                      ub_version,
                      cb_version,
                      lookups_version,
                      build_type = "",
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
  
  ub_dir <- str_c(nts_dir, "unclassified builds/ub_", ub_version, ".csv")
  cb_columns_dir <- str_c(nts_dir, "import/cb_columns/cb_vars_", cb_version, ".csv")
  
  # Exports
  export_dir <- str_c(nts_dir, "classified builds/")
  dir.create(export_dir, showWarnings = FALSE)
  
  out_cb_dir <- str_c(export_dir, "cb_", cb_version, ".csv")
  out_hb_tr_dir <- str_c(export_dir, "cb_hb trip rates_", lookups_version, ".csv")
  out_hb_weights_dir <- str_c(nts_dir, "outputs/hb/hb_trip_rates/hb_response_weights.csv")
  
  # Unclassified build
  ub <- read_csv(ub_dir)
  
  # Columns subset
  cb_columns <- read_csv(cb_columns_dir)
  cb_columns <- pull(cb_columns)
  
  ub <- select(ub, all_of(cb_columns))
  
  # Audit 1
  nts_audit <- define_nts_audit_params(ub)

 # Pre-processing ----------------------------------------------------------
  
  # All non-lookup pre processing
  ub <- cb_preprocess(ub, cb_version)
  
 # Classify Purposes -------------------------------------------------------
  
  # Remove Just-Walk trips 17, Other non-escort, and other escort 
  ub <- ub %>%
    filter(TripPurpose_B01ID != 17,
           !TripPurpTo_B01ID %in% c(16,22))
  
  # Remove trips which are home to 'escort home' and 'escort home' to home
  ub <- ub %>%
    filter(!(TripPurpFrom_B01ID == 23 & TripPurpTo_B01ID == 17), # 11,438 records
           !(TripPurpFrom_B01ID == 17 & TripPurpTo_B01ID == 23)) # 24,810 records
  
  # Define trip purposes
  ub <- ub %>%
    lu_trip_origin(lookups_version) %>%
    lu_hb_purpose(lookups_version) %>%
    lu_nhb_purpose(lookups_version) %>% 
    lu_nhb_purpose_hb_leg(lookups_version) %>%
    mutate(trip_purpose = ifelse(trip_origin == "hb", hb_purpose, nhb_purpose))
  
  # TODO: Checkfunction after steps return nominal
  ub %>% count(SurveyYear)
  
 # Classify Other variables ------------------------------------------------
  
  ub <- ub %>%
    lu_gender(lookups_version) %>%
    lu_age_work_status(lookups_version) %>%
    lu_hh_type(lookups_version) %>%
    lu_main_mode(lookups_version) %>%
    lu_start_time(lookups_version) %>%
    lu_end_time(lookups_version) %>%
    lu_sw_weight(lookups_version) %>% 
    lu_is_north(lookups_version)
  
  if(lookups_version == "ntem"){
    
    ub <- lu_ntem_tt(ub, lookups_version)
    
    ub <- ub %>%
      mutate(area_type = case_when(
        PSUAreaType1_B01ID == 1 ~ 1,
        PSUAreaType1_B01ID == 2 ~ 2,
        PSUAreaType1_B01ID %in% c(3:8) ~ 3,
        PSUAreaType1_B01ID == 9 ~ 4,
        PSUAreaType1_B01ID %in% c(10:12) ~ 5,
        PSUAreaType1_B01ID == 13 ~ 6,
        PSUAreaType1_B01ID == 14 ~ 7,
        PSUAreaType1_B01ID == 15 ~ 8
      ))
    
  } else {
    
    ub <- ub %>%
      lu_soc(lookups_version) %>%
      lu_tfn_tt(lookups_version) %>%
      lu_tfn_at(lookups_version) 
    
  }
  
  if(save_processed) write_csv(ub, out_cb_dir)
  
  if(build_type == "hb_trip_rates"){
   
    grouping_vars <- c("IndividualID", "trip_purpose", "SurveyYear", "age_work_status", "gender",
                       "hh_type", "soc", "ns", "tfn_area_type", "area_type")
    
    grouping_vars <- colnames(ub)[colnames(ub) %in% grouping_vars]
    
    # Remove Air trips
    ub <- filter(ub, main_mode != 8)
    
    # Weight trips by short walk and calculate weekly trips
    weighted_trips <- ub %>%
      filter(trip_purpose %in% 1:8,
             W1 == 1) %>%
      group_by_at(grouping_vars) %>%
      summarise(weekly_trips = sum(JJXSC),
                W2 = mean(W2),
                W5 = sum(W5),
                W5xHH = sum(W5xHH)) %>%
      ungroup()
      
    grouping_vars <- str_subset(grouping_vars, "trip_purpose", negate = TRUE)
    grouping_vars <- c(grouping_vars, "W2")
    
    # Every individual must have an observation for each trip purpose
    hb_trip_rates_out <- weighted_trips %>%
      complete(nesting(!!!dplyr::syms(grouping_vars)),
               trip_purpose = 1:8,
               fill = list(weekly_trips = 0, W5 = 0, W5xHH = 0))
    
    write_csv(hb_trip_rates_out, out_hb_tr_dir)
    
    response_weights <- ub %>%
      filter(trip_purpose %in% 1:8,
             W1 == 1) %>%
      select(IndividualID, trip_purpose, age_work_status,  W2, W5, W5xHH) %>%
      group_by(age_work_status, trip_purpose) %>%
      summarise(W5W2 = n()/sum(W2)) %>%
      ungroup()

    write_csv(response_weights, out_hb_weights_dir)
    
  }
  
}
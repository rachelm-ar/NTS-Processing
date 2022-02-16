define_nts_audit_params <- function(nts_dat){

  # Include more variable to check. i.e. trip purpose, age_work_status
  
  nts_dat %>%
    count(SurveyYear)
  
}

cb_preprocess <- function(ub, cb_version){
  
  # Post codes
  ub <- ub %>% 
    mutate(PSUPSect = str_replace(PSUPSect, "^(.*\\s.).*", "\\1"),
           PSUPSect = str_replace_all(PSUPSect, " ", "")) 
    
  ub %>% 
    rename(ns = NSSec_B03ID) %>%
    mutate(ns = ifelse(ns == -9, 6, ns)) %>% 
    group_by(HouseholdID) %>%
    mutate(ns = min(ns)) %>%
    ungroup() %>%
    mutate(ns = ifelse(ns == 6, 5, ns))
  
}

build_cb <- function(cb_input_csv_dir, tfn_or_ntem = "tfn", save_processed, extract_hb_trip_rates_inputs){
  
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr",
                    "purrr")
  
  library_c(library_list)
  
  # Directories -------------------------------------------------------------
   
  cb_input <- read_csv(cb_input_csv_dir)
  
  # Exports
  dir.create(cb_input$output_dir, showWarnings = FALSE)
  
  out_cb_dir <- str_c(cb_input$output_dir, cb_input$output_name)
  
  out_hb_tr_dir <- str_c(cb_input$hb_trip_rates_inputs_dir, "hb_tr_input_", sep = "\\")
  out_hb_tr_dir <- str_c(out_hb_tr_dir, tfn_or_ntem, ".csv")
  
  out_hb_weights_dir <- str_c(cb_input$hb_trip_rates_inputs_dir, "hb_tr_response_weights_", sep = "\\")
  out_hb_weights_dir <- str_c(out_hb_weights_dir, tfn_or_ntem, ".csv")
  
  # Unclassified build
  ub <- read_csv(cb_input$ub_csv_dir)
  
  # Audit 1
  nts_audit <- define_nts_audit_params(ub)
  
  # Pre-processing ----------------------------------------------------------
  
  # All non-lookup pre processing
  ub <- cb_preprocess(ub, version)
   
  # Classify Purposes -------------------------------------------------------

  # Remove Just-Walk trips 17, Other non-escort, and other escort 
  ub <- ub %>%
    filter(TripPurpose_B01ID != 17,
           !TripPurpTo_B01ID %in% c(16,22))
  
  # Remove trips which are home to 'escort home' and 'escort home' to home
  ub <- ub %>%
    filter(!(TripPurpFrom_B01ID == 23 & TripPurpTo_B01ID == 17), # 11,438 records
           !(TripPurpFrom_B01ID == 17 & TripPurpTo_B01ID == 23)) # 24,810 records
  
  # Redefine Escort Home trips as home
  ub <- ub %>%
    mutate(TripPurpTo_B01ID = ifelse(TripPurpTo_B01ID == 17, 23, TripPurpTo_B01ID),
           TripPurpFrom_B01ID = ifelse(TripPurpFrom_B01ID == 17, 23, TripPurpFrom_B01ID))
  
  # Define trip purposes
  ub <- ub %>%
    lu_trip_origin() %>%
    lu_hb_purpose() %>%
    lu_nhb_purpose() %>% 
    lu_nhb_purpose_hb_leg() %>%
    mutate(p = ifelse(trip_origin == "hb", hb_purpose, nhb_purpose))

  # Classify Other variables ------------------------------------------------
  
  cb <- ub %>%
    lu_gender() %>%
    lu_aws() %>%
    lu_hh_type() %>%
    lu_main_mode() %>%
    lu_start_time() %>%
    lu_end_time() %>%
    lu_sw_weight() %>% 
    lu_is_north() %>% 
    lu_soc() %>%
    lu_tfn_at() %>% 
    lu_tt()
  
  cb <- mutate(cb, weighted_trips = W5xHH * W2 * JJXSC)
  
  if(tfn_or_ntem == "ntem"){
    
    cb <- cb %>% 
      lu_ntem_at() %>%
      lu_ntem_aws() %>% 
      lu_ntem_main_mode()
    
  }
  
  if(save_processed) write_csv(cb, out_cb_dir)
  
  if(extract_hb_trip_rates_inputs == TRUE){
    
    if(tfn_or_ntem == "tfn"){
      
      grouping_vars <- c("IndividualID", "p", "SurveyYear", "aws", "gender",
                         "hh_type", "soc", "ns", "tfn_at")
      
      grouping_vars <- colnames(cb)[colnames(cb) %in% grouping_vars]
      
    } else if (tfn_or_ntem == "ntem"){
      
      grouping_vars <- c("IndividualID", "p", "SurveyYear", "aws", "gender",
                         "hh_type", "ntem_at")
      
      grouping_vars <- colnames(cb)[colnames(cb) %in% grouping_vars]
      
    }
    
    # Remove Air trips
    cb <- filter(cb, main_mode != 8)
    
    if(tfn_or_ntem == "ntem"){
      
      cb <- filter(cb, SurveyYear %in% 2002:2012)
      
    }
    
    # Weight trips by short walk and calculate weekly trips
    weighted_trips <- cb %>%
      filter(p %in% 1:8,
             W1 == 1) %>%
      group_by_at(grouping_vars) %>%
      summarise(weekly_trips = sum(JJXSC)) %>%
      ungroup()
    
    grouping_vars <- str_subset(grouping_vars, "^p$", negate = TRUE)
    
    # Every individual must have an observation for each trip purpose
    hb_trip_rates_out <- weighted_trips %>%
      complete(nesting(!!!dplyr::syms(grouping_vars)),
               p = 1:8,
               fill = list(weekly_trips = 0))
    
    write_csv(hb_trip_rates_out, out_hb_tr_dir)
    
    response_weights <- cb %>% 
      filter(p %in% 1:8,
             W1 == 1) %>%
      select(IndividualID, p, SurveyYear, W5xHH, JJXSC, W2) %>% 
      mutate(trips = 1) %>% 
      complete(nesting(IndividualID, SurveyYear),
               p = 1:8,
               fill = list(W5xHH = 0, trips = 0, JJXSC = 0, W2 = 0)) %>% 
      group_by(p, SurveyYear) %>% 
      summarise(r_weights = sum(W5xHH*JJXSC*W2)/sum(trips*JJXSC*W2),
                count = sum(trips)) %>% 
      ungroup()
      
    write_csv(response_weights, out_hb_weights_dir)
    
  }
  
}


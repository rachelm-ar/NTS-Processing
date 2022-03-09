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

build_cb <- function(input_csv){
  
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr",
                    "purrr")
  
  library_c(library_list)
  
  # Directories -------------------------------------------------------------
   
  # Read input csv
  input_csv <- read_csv(input_csv)
  input_csv <- transpose_input_csv(input_csv)
  
  # classified build write dir
  cb_output_dir <- str_c(input_csv$cb_save_dir,
                         "\\",
                         input_csv$cb_name,
                         ".csv")
  
  # hb trip rates build write dir
  hb_trip_rates_output_dir <- str_c(input_csv$hb_trip_rates_save_dir,
                                    "\\",
                                    input_csv$hb_trip_rates_build_name,
                                    ".csv")
  
  # hb trip rates response weights write dir
  hb_trip_rates_weights_output_dir <- str_c(input_csv$hb_trip_rates_save_dir,
                                            "\\",
                                            input_csv$hb_trip_rates_response_weights_name,
                                            ".csv")
  
  # Unclassified build
  ub_in <- read_csv(input_csv$ub_csv_dir)
  
  # Audit 1
  nts_audit <- define_nts_audit_params(ub_in)
  
  # Pre-processing ----------------------------------------------------------
  
  # All non-lookup pre processing
  ub <- cb_preprocess(ub_in, version)
  
  # Check classifications
  #  Audit report only - delete from prod]
  all_c <- ub %>%
    group_by(TripPurpFrom_B01ID, TripPurpTo_B01ID) %>%
    count()
  
  
  # Classify Purposes -------------------------------------------------------

  # Remove Just-Walk trips 17, Other non-escort, and other escort 
  # ub <- ub %>%
  #  filter(TripPurpose_B01ID != 17,
  #         !TripPurpTo_B01ID %in% c(16: 22))

  # Remove trips which are home to 'escort home' and 'escort home' to home
  # Out until we figure out how to remove them holistically
  ub <- ub %>%
    filter(!(TripPurpFrom_B01ID == 23 & TripPurpTo_B01ID == 17), # 11,438 records
           !(TripPurpFrom_B01ID == 17 & TripPurpTo_B01ID == 23)) # 24,810 records
  
  # Remove all escort trips, retain pure purpose only
  # escort_p = c(17:22)
  # ub <- ub %>%
  #   filter(!(TripPurpFrom_B01ID == 23 & TripPurpTo_B01ID %in% escort_p) &
  #          !(TripPurpFrom_B01ID %in% escort_p & TripPurpTo_B01ID == 23))
  
  # escort_p = c(17:22)
  # ub <- ub %>%
  #   filter(!(TripPurpFrom_B01ID %in% escort_p) &
  #           !(TripPurpTo_B01ID %in% escort_p))
  
  # TODO: Remove duplicate escort trips
  # TODO: Catch trips starting from non-home home
  
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
  
  # Trip Grouping -----------------------------------------------------------
  
  
  eighteensub <- cb %>%
    filter(SurveyYear == 2018)
  
  write_csv(eighteensub, '18_sub.csv')
  
  # Diary subset
  diary <- eighteensub %>%
    select(SurveyYear, IndividualID, TravelWeekDay_B01ID, TripID, TripPurpFrom_B01ID,
           TripPurpTo_B01ID, trip_origin, hb_purpose, nhb_purpose, nhb_purpose_hb_leg,
           main_mode, start_time, end_time, weighted_trips) %>%
    arrange(IndividualID, TripID) %>%
    group_by(IndividualID) %>%
    mutate(start_flag = case_when(TripPurpFrom_B01ID == 23 ~ 1,
                                  TRUE ~ 0)) %>%
    mutate(end_flag = case_when(TripPurpTo_B01ID == 23 ~ 1,
                                TRUE ~ 0)) %>%
    mutate(trip_group = cumsum(start_flag)) %>%
    ungroup()
 
  
  # Native mutant trips
  nmt <- diary %>%
    group_by(IndividualID, trip_group) %>%
    mutate(tour_count = sum(start_flag) + sum(end_flag)) %>%
    mutate(record = row_number()) %>%
    ungroup()
    
  nmt_summary <- nmt %>%
    group_by(tour_count) %>%
    count()
  
  write_csv(nmt_summary, 'nmt_summary.csv')
  
  # Purpose audits
  pfrom <- diary %>%
    filter(start_flag == 1) %>%
    group_by(TripPurpFrom_B01ID, start_time) %>%
    count()
  
  write_csv(pfrom, 'pfrom.csv')
  
  pto <- diary %>%
    filter(end_flag == 1) %>%
    group_by(TripPurpTo_B01ID, start_time) %>%
    count()
  
  write_csv(pto, 'pto.csv')
  
  offending_tours <- nmt %>%
    filter(tour_count != 2)
  
  write_csv(offending_tours, 'offending_tours.csv')
  
  
  # Main code - remove diary work
  
  # Discard finals trips in travel diary which are outbound
  
  # cb <- cb %>%
  #  group_by(IndividualID) %>% 
  #  filter(!(TripID == max(TripID) & TripPurpFrom_B01ID == 23)) %>% 
  #  ungroup()
  
  # Start a trip when from home and end when to home
  cb <- cb %>%
    arrange(IndividualID, TripID) %>%
    group_by(IndividualID) %>%
    mutate(start_flag = case_when(TripPurpFrom_B01ID == 23 ~ 1,
                                  TRUE ~ 0)) %>%
    mutate(end_flag = case_when(TripPurpTo_B01ID == 23 ~ 1,
                                TRUE ~ 0)) %>%
    mutate(trip_group = cumsum(start_flag)) %>%
    ungroup()
  
  test <- head(cb)
  
  # Allocate the purpose people left the house on, tour start time, last tour trip start time
  first_trips <- cb %>%
    filter(start_flag == 1) %>%
    group_by(IndividualID, trip_group) %>%
    mutate(frh_tp = start_time) %>%
    mutate(frh_p = hb_purpose) %>%
    ungroup() %>%
    select(IndividualID, trip_group, frh_p, frh_tp)
  
  # TODO: Last trips - classifiy to home purpose only
  
  cb <- cb %>%
    left_join(first_trips,
              by = c("IndividualID", "trip_group"))
  
  # Name trip types
  cb <- cb %>%
    mutate(trip_type = case_when(start_flag & !end_flag ~ 'frh',
                                 end_flag & !start_flag ~ 'toh',
                                 TRUE ~ 'nhb'))
  
  # No zero tour groups, start with first outbound
  # TODO: This is just a result of poor tour classification, fix that not this, it leaves too many loose trips
  # cb <- filter(cb, trip_group != 0)

  # Export
  write_csv(cb, cb_output_dir)
  write_csv(cb, 'C:/Users/genie/Downloads/cb_tfn.csv')
  

# HB Trip Rates inputs from CB --------------------------------------------
  
  grouping_vars <- c("IndividualID", "p", "SurveyYear", "aws", "gender",
                     "hh_type", "soc", "ns", "tfn_at")
  
  grouping_vars <- colnames(cb)[colnames(cb) %in% grouping_vars]
  
  # Remove Air and Van trips
  hb_trip_rates_input <- filter(cb, !main_mode %in% c(4, 8))
    
  # Weight trips by short walk and calculate weekly trips
  weighted_trips <- hb_trip_rates_input %>%
    filter(p %in% 1:8,
           W1 == 1) %>%
    group_by_at(grouping_vars) %>%
    summarise(weekly_trips = sum(JJXSC)) %>%
    ungroup()
  
  # Remove purpose from grouping vars
  grouping_vars <- str_subset(grouping_vars, "^p$", negate = TRUE)
    
  # Every individual must have an observation for each trip purpose
  hb_trip_rates_out <- weighted_trips %>%
    complete(nesting(!!!dplyr::syms(grouping_vars)),
             p = 1:8,
             fill = list(weekly_trips = 0)) %>% 
    ungroup()
    
  write_csv(hb_trip_rates_out, hb_trip_rates_output_dir)
    
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
      
    write_csv(response_weights, hb_trip_rates_weights_output_dir)
  
}
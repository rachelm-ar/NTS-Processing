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

classify_tours <- function(cb){

  cb <- cb %>%
    arrange(IndividualID, TripID) %>% # IndividualID, TripID
    group_by(IndividualID) %>%
    mutate(start_flag = case_when(TripPurpFrom_B01ID == 23 ~ 1,
                                  TRUE ~ 0)) %>%
    mutate(end_flag = case_when(TripPurpTo_B01ID == 23 ~ 1,
                                TRUE ~ 0)) %>%
    mutate(trip_group = cumsum(start_flag)) %>%
    mutate(end_group = cumsum(end_flag)) %>%
    ungroup()
  
}

count_broken_trips <- function(cb){
  bt <- cb %>%
    filter(TripPurpTo_B01ID == 23) %>%
    filter(end_group > trip_group) %>%
    count()
  
  return(bt)
}

build_cb <- function(input_csv, reclassify_escorts=FALSE){
  
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

  
  # Classify Purposes -------------------------------------------------------
  if(reclassify_escorts){
    # Remove trips which are home to 'escort home' and 'escort home' to home
    ub <- ub %>%
      filter(!(TripPurpFrom_B01ID == 23 & TripPurpTo_B01ID == 17), # 11,438 records
             !(TripPurpFrom_B01ID == 17 & TripPurpTo_B01ID == 23)) # 24,810 records

    # Redefine Escort Home trips as home
    ub <- ub %>%
      mutate(TripPurpTo_B01ID = ifelse(TripPurpTo_B01ID == 17, 23, TripPurpTo_B01ID),
             TripPurpFrom_B01ID = ifelse(TripPurpFrom_B01ID == 17, 23, TripPurpFrom_B01ID))
  }
  
  # Define trip purposes
  ub <- ub %>%
    lu_trip_origin() %>%
    lu_hb_purpose() %>%
    lu_nhb_purpose() %>%
    lu_nhb_purpose_hb_leg() %>%
    lu_agg_purpose() %>%
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
  
  before_to <- cb %>% group_by(TripPurpTo_B01ID) %>% count()
  before_from <- cb %>% group_by(TripPurpFrom_B01ID) %>% count()
  
  # Correct purpose
  # TODO: Needs to be tested thoroughly
  cb <- cb %>%
    arrange(SurveyYear, HouseholdID, IndividualID, DayID, TripID) %>%
    mutate(cr_indi = lead(IndividualID, n=1)) %>%
    mutate(cr_trip = lead(TripID, n=1)) %>%
    mutate(to_purp = lead(TripPurpTo_B01ID, n=1)) %>%
    mutate(TripPurpTo_B01ID = case_when(
      IndividualID==cr_indi & TripID+1==cr_trip ~ TripPurpTo_B01ID,
      TRUE ~ to_purp))
  
  after_to <- cb %>% group_by(TripPurpTo_B01ID) %>% count()
  after_from <- cb %>% group_by(TripPurpFrom_B01ID) %>% count()
  
  cb <- cb %>% select(-cr_indi, -cr_trip, -to_purp)
  
  # Reclassify trips to home to pick up multiple return home legs and remove them
  cb <- classify_tours(cb)
  broken_trips <- count_broken_trips(cb)
  
  cb <- classify_tours(cb)
  cb <- cb %>% filter(!(trip_group == 0 & end_group > 0))
  cb <- classify_tours(cb)
  cb <- cb %>% filter(!end_group > trip_group)
  broken_trips <- count_broken_trips(cb)
  
  # Find out what the allocated aggregate purpose is (masks escorts)
  tour_classifications <- cb %>%
    select(IndividualID, trip_group, agg_purpose) %>%
    # Remove to home as an option
    filter(agg_purpose != 99) %>%
    group_by(IndividualID, trip_group) %>%
    # Count records by allocated purpose
    mutate(purp_count = n()) %>%
    ungroup() %>%
    group_by(IndividualID, trip_group) %>%
    # Pick the most frequent
    filter(purp_count == max(purp_count)) %>%
    # This will leave lots tied on 1 or 2, add a count of records
    mutate(rnum = row_number()) %>%
    # Pick the later of the 2
    filter(rnum == max(rnum)) %>%
    mutate(tour_p = agg_purpose) %>%
    # Remove derived columns to clear for join
    select(-agg_purpose, -rnum) %>%
    # Should be 1 record per group
    ungroup()
  
  # TODO: Add tour correction
  #process tours

  cb <- cb %>%
    left_join(tour_classifications,
              by = c("IndividualID", "trip_group"))
  
  # Allocate the purpose people left the house on, tour start time, last tour trip start time
  first_trips <- cb %>%
    filter(start_flag == 1) %>%
    group_by(IndividualID, trip_group) %>%
    mutate(frh_tp = start_time) %>%
    mutate(frh_p = hb_purpose) %>%
    mutate(frh_mode = main_mode) %>%
    ungroup() %>%
    select(IndividualID, trip_group, frh_mode, frh_p, frh_tp)
  
  cb <- cb %>%
    left_join(first_trips,
              by = c("IndividualID", "trip_group"))
  
  # Count tours
  cb <- cb %>%
    group_by(IndividualID, trip_group) %>%
    mutate(tour_count = sum(start_flag) + sum(end_flag)) %>%
    mutate(tour_record = row_number()) %>%
    ungroup()
  
  # Name trip types
  cb <- cb %>%
    mutate(trip_type = case_when(start_flag & !end_flag ~ 'frh',
                                 end_flag & !start_flag ~ 'toh',
                                 TRUE ~ 'nhb'))

  # Export
  write_csv(cb, cb_output_dir)
  

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
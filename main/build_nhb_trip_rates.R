#TODO:Look at getting in SIC & SOC (can be in time split, not here)

extract_nhb <- function(drive, user, trip_rate, time_split){
  
  library_list <- c("dplyr",
                    "stringr",
                    "readr")
  
  library_c(library_list)
  
  # Directories -------------------------------------------------------------
  
  y_dir <- "Y:/NTS/"
  c_dir <- str_c("C:/Users/", user, "/Documents/NTS_C/")
  nts_dir <- ifelse(drive == "Y", y_dir, c_dir)
  
  ctripend_dir <- str_c(nts_dir, "import/ctripend/")
  cb_dir <- str_c(nts_dir, "classified builds/", cb_name, ".csv")
  
  export_dir <- str_c(nts_dir, "outputs/nhb/")
  
  # Read in -----------------------------------------------------------------
  
  # Classified build
  cb <- read_csv(cb_dir)
  
  # NTEM nhb trip rates
  tr_control <- read_csv(str_c(ctripend_dir, "/IgammaNMHM/IgammaNMHM.csv"))
  
  # NTEM nhb time split
  ts_control <- read_csv(str_c(ctripend_dir, "/IRHOdmnr/IRHOdmnr_Final.csv"))
  
  # Pre-processing ----------------------------------------------------------
  
  # Classified build columns and weights
  retain_cols <- c("IndividualID", 
                   "soc_cat", 
                   "ns_sec", 
                   "TripID", 
                   "TravelWeekDay_B01ID", 
                   "main_mode", 
                   "TripPurpFrom_B01ID",
                   "TripPurpTo_B01ID", 
                   "TripStart_B01ID", 
                   "TripEnd_B01ID",
                   "hb_purpose", 
                   "nhb_purpose", 
                   "trip_weights",
                   "tfn_area_type",
                   "start_time")
  
  cb <- cb %>%
    mutate(trip_weights = W1 * W5 * W2 * sw_weight) %>%
    select(one_of(retain_cols))
  
  # NHB trip rates renaming
  tr_old_vars <- c("N", "M", "H", "HBM", "Gamma")
  tr_new_vars <- c("nhb_purpose", "nhb_mode", "hb_purpose", "hb_mode", "trip_rate_ntem")
  
  tr_control <- rename_at(tr_control, all_of(tr_old_vars), ~ tr_new_vars)
  
  # NHB trip rates - Merge Business/Commute & Driving/Passenger
  tr_control <- tr_control %>%
    mutate(nhb_mode = ifelse(nhb_mode == 4, 3, nhb_mode),
           hb_mode = ifelse(hb_mode == 4, 3, hb_mode),
           nhb_purpose = ifelse(nhb_purpose == 11, 12, nhb_purpose)) %>% 
    group_by(nhb_purpose, nhb_mode, hb_purpose, hb_mode) %>%
    summarise(trip_rate_ntem = sum(trip_rate_ntem))
  
  # Time Split pre processing
  ts_old_vars <- c("n", "m", "r", "d", "rho2")
  ts_new_vars <- c("nhb_purpose", "nhb_mode", "tfn_area_type", "start_time", "time_split")
  
  ts_control <- rename_at(ts_control, all_of(ts_old_vars), ~ ts_new_vars)
  
  
  # Trip Grouping -----------------------------------------------------------
  # Why unique?
  tour_groups_week <- cb %>%
    unique() %>%
    arrange(IndividualID, TripID) %>%
    group_by(IndividualID) %>%
    mutate(start_flag = case_when(TripPurpFrom_B01ID == 23 ~ 1,
                                  TRUE ~ 0)) %>%
    mutate(end_flag = case_when(TripPurpTo_B01ID == 23 ~ 1,
                                TRUE ~ 0)) %>%
    mutate(trip_group = cumsum(start_flag)) %>%
    ungroup()
  
  tour_groups <- filter(tour_groups_week, TravelWeekDay_B01ID %in% 1:5)
  
  
  # NHB Trip Rates ----------------------------------------------------------
  
  if(trip_rate){
    
    # HB trips
    hb_trips <- tour_groups %>%
      select(IndividualID, hb_purpose, main_mode, trip_group, start_flag, trip_weights) %>%
      filter(start_flag == 1) %>%
      rename(hb_mode = main_mode)
    
    # NHB trips
    nhb_trips <- tour_groups %>%
      select(IndividualID, nhb_purpose, main_mode, trip_group, start_flag, end_flag, trip_weights) %>%
      filter(start_flag == 0 & end_flag == 0) %>%
      rename(nhb_mode = main_mode)
    
    # HB outwards
    hb_leg_info <- hb_trips %>%
      select(IndividualID, hb_purpose, hb_mode, trip_group)
    
    # NHB trips connected with HB outward
    nhb_w_hb <- nhb_trips %>%
      left_join(hb_leg_info, by=c("IndividualID", "trip_group"))
    
    # Group and sum out the identifiers and the people
    hb_totals <- hb_trips %>%
      group_by(hb_purpose, hb_mode) %>%
      summarise(hb_trips = sum(trip_weights, na.rm=TRUE)) %>%
      ungroup()
    
    nhb_totals <- nhb_w_hb %>%
      group_by(nhb_purpose, nhb_mode, hb_purpose, hb_mode) %>%
      summarise(nhb_trips = sum(trip_weights, na.rm=TRUE)) %>%
      ungroup()
    
    # Turn into trip rate (nhb by p/m/nhbp/nhbm divided by hb by p/m)
    nhb_trip_rates <- nhb_totals %>%
      left_join(hb_totals, by=c('hb_purpose', 'hb_mode')) %>%
      mutate(nhb_trip_rate = nhb_trips/hb_trips) %>% 
      filter(!is.na(hb_purpose))
    
    # Check against NTEM rates (IgammaNHBH)
    ntem_comparison <- nhb_trip_rates %>%
      left_join(tr_control, by=c('hb_purpose', 'hb_mode', 'nhb_purpose', 'nhb_mode'))
    
    lm(nhb_trip_rate ~ trip_rate_ntem, data = ntem_comparison) %>%
      summary()
    
    # Write out
    write_csv(nhb_trip_rates, str_c(export_dir, "nhnhb_ave_wday_ntem.csv"))
    
    write_csv(nhb_trip_rates, str_c(export_dir, "nhb_ave_wday_enh_trip_rates.csv"))
    
  }  
  
  if(time_split){
    
    # Weekdays ---------------------------------------------------------
    
    # NHB trips with start time
    ts_nhb_trips <- tour_groups %>%
      select(IndividualID, nhb_purpose, tfn_area_type, start_time, main_mode, trip_group, start_flag, end_flag, trip_weights) %>%
      filter(start_flag == 0 & end_flag == 0) %>%
      rename(nhb_mode = main_mode)
    
    # Calculate time splits
    ts_factors <- ts_nhb_trips %>% 
      group_by(nhb_purpose, nhb_mode, tfn_area_type, start_time) %>%
      summarise(trips = sum(trip_weights)) %>%
      na.omit() %>% 
      group_by(nhb_purpose, nhb_mode, tfn_area_type) %>%
      mutate(prop = trips/sum(trips)) %>%
      rename(tfn_time_split = prop)
    
    ts_comparison <- ts_factors %>% 
      left_join(ts_control)
    
    lm(tfn_time_split ~ time_split, data = ts_comparison) %>% 
      summary()
    
    write_csv(ts_comparison, str_c(export_dir, "nhb_ave_wday_time_split_ntem.csv"))
    
    write_csv(ts_factors, str_c(export_dir, "tfn_nhb_ave_wday_time_split_18.csv"))
    
    
    # Full Week ---------------------------------------------------------------
    
    ts_nhb_trips_week <- tour_groups_week %>%
      select(IndividualID, nhb_purpose, tfn_area_type, start_time, main_mode, trip_group, start_flag, end_flag, trip_weights) %>%
      filter(start_flag == 0 & end_flag == 0) %>%
      rename(nhb_mode = main_mode)
    
    ts_factors_week <- ts_nhb_trips_week %>% 
      group_by(nhb_purpose, nhb_mode, tfn_area_type, start_time) %>%
      summarise(trips = sum(trip_weights)) %>%
      na.omit() %>% 
      group_by(nhb_purpose, nhb_mode, tfn_area_type) %>%
      mutate(prop = trips/sum(trips)) %>%
      rename(tfn_time_split = prop)
    
    
    write_csv(ts_factors_week, str_c(export_dir, 'tfn_nhb_ave_week_time_split_18.csv'))
    
  }
  
}
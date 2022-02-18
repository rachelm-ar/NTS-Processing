build_nhb_outputs <- function(input_csv, trip_rate, time_split){
  
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr")
  
  library_c(library_list)
  
# Directories and read in -------------------------------------------------
  
  # Read and transpose input csv
  input_csv <- read_csv(input_csv)
  input_csv <- transpose_input_csv(input_csv)
  
  # Classified build
  cb <- read_csv(input_csv$cb_csv_dir)
  
  # NTEM nhb trip rates
  tr_control <- read_csv(str_c(input_csv$ctripend_dir, "/IgammaNMHM/IgammaNMHM.csv"))
  
  # NTEM nhb time split
  ts_control <- read_csv(str_c(input_csv$ctripend_dir, "/IRHOdmnr/IRHOdmnr_Final.csv"))
  
  # write nhb trip rates dir
  trip_rates_output_dir <- str_c(input_csv$nhb_trip_rates_save_dir,
                                 "\\",
                                 input_csv$nhb_trip_rates_name,
                                 "_",
                                 input_csv$nhb_trip_rates_version,
                                 ".csv")
  
  # write nhb trip rates dir
  time_splits_output_dir <- str_c(input_csv$nhb_time_splits_save_dir, 
                                  "\\",
                                  input_csv$nhb_time_splits_name,
                                  "_",
                                  input_csv$nhb_time_splits_version,
                                  ".csv")
  
  # Time splits report
  time_splits_report_dir <- str_c(input_csv$nhb_trip_rates_save_dir, 
                                  "\\Reports\\nhb_time_split_report",
                                  "_",
                                  input_csv$nhb_time_splits_version,
                                  ".csv")
  
  dir.create(str_c(input_csv$nhb_trip_rates_save_dir, "\\Reports\\"), showWarnings = FALSE, recursive = TRUE)

  # Pre-processing ----------------------------------------------------------

  # Classified build columns and weights
  retain_cols <- c("IndividualID", 
                   "soc", 
                   "ns", 
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
                   "tfn_at",
                   "start_time",
                   "trip_origin",
                   "p")
  
  cb <- cb %>%
    filter(W1 == 1) %>% 
    mutate(trip_weights = W5xHH * W2 * JJXSC) %>%
    select(one_of(retain_cols))
  
  # Remove visiting friends
  cb <- cb %>% 
    filter(hb_purpose != 7, nhb_purpose != 17)
  
  # Remove air
  cb <- filter(cb, main_mode != 8)
  
  # Remove van
  cb <- filter(cb, main_mode != 4)
  
  # Aggregate light rail and surface rail
  cb <- mutate(cb, main_mode = ifelse(main_mode == 7, 6, main_mode))
  
  ### CTripEnd preprocessing
  
  # NHB trip rates renaming
  tr_old_vars <- c("N", "M", "H", "HBM", "Gamma")
  tr_new_vars <- c("nhb_purpose", "nhb_mode", "hb_purpose", "hb_mode", "cte_tr")
  
  tr_control <- rename_at(tr_control, all_of(tr_old_vars), ~ tr_new_vars)
  
  # NHB trip rates remove car driver and passenger
  tr_control <- tr_control %>%
    filter(!nhb_mode %in% c(3,4),
           !hb_mode %in% c(3,4))
  
  # Time Split pre processing
  ts_old_vars <- c("n", "m", "r", "d", "rho2")
  ts_new_vars <- c("nhb_p", "m", "tfn_at", "tp", "cte_ts")
  
  ts_control <- rename_at(ts_control, all_of(ts_old_vars), ~ ts_new_vars)
  
  ts_control <- ts_control %>%
    filter(!m %in% c(3,4))
    
  # Trip Grouping -----------------------------------------------------------
  
  # Discard finals trips in travel diary which are outbound
  cb <- cb %>%
    group_by(IndividualID) %>% 
    filter(!(TripID == max(TripID) & TripPurpFrom_B01ID == 23)) %>% 
    ungroup()
  
  # Start a trip when from home and end when to home
  tour_groups <- cb %>%
    arrange(IndividualID, TripID) %>%
    group_by(IndividualID) %>%
    mutate(start_flag = case_when(TripPurpFrom_B01ID == 23 ~ 1,
                                  TRUE ~ 0)) %>%
    mutate(end_flag = case_when(TripPurpTo_B01ID == 23 ~ 1,
                                TRUE ~ 0)) %>%
    mutate(trip_group = cumsum(start_flag)) %>%
    ungroup()
  
  # Trip chaining must start at first HB outbound trip
  tour_groups <- filter(tour_groups, trip_group != 0)
  
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
    
    # Manual fix - why?
    nhb_trip_rates <- nhb_trip_rates %>%
      filter(hb_purpose != 99)
    
    # Infill 0 for missing segments
    nhb_trip_rates <- nhb_trip_rates %>%
      complete(nhb_purpose = c(11, 12, 13, 14, 15, 16, 18),
               nhb_mode = c(1, 2, 3, 5, 6),
               hb_purpose = c(1:6, 8),
               hb_mode = c(1, 2, 3, 5, 6),
               fill = list(nhb_trips = 0, hb_trips = 0, nhb_trip_rate = 0))
    
    # Check against NTEM rates (IgammaNHBH)
    ntem_comparison <- nhb_trip_rates %>%
      left_join(tr_control, by=c('hb_purpose', 'hb_mode', 'nhb_purpose', 'nhb_mode')) %>% 
      filter(hb_mode !=3, nhb_mode != 3)
    
    lm(nhb_trip_rate ~ cte_tr, data = ntem_comparison) %>%
      summary()
    
    nhb_trip_rates <- nhb_trip_rates %>% 
      rename(nhb_p = nhb_purpose,
             nhb_m = nhb_mode,
             p = hb_purpose,
             m = hb_mode)
    
    # Write out
    write_csv(nhb_trip_rates, trip_rates_output_dir)
    
  }  
  
  if(time_split){
    
    # Filter for nhb trips
    ts_nhb_trips <- cb %>% 
      filter(trip_origin == "nhb", p != 99)
    
    # Modes
    modes_list <- cb %>% 
      distinct(main_mode) %>% 
      pull() %>% 
      sort()
    
    # Area Types
    ats_list <- cb %>% 
      distinct(tfn_at) %>%
      pull() %>%
      sort()
    
    # Aggregate 
    agg_all <- ts_nhb_trips %>%
      select(p, tfn_at, start_time, main_mode, trip_weights) %>%
      group_by(p, tfn_at, start_time, main_mode) %>%
      summarise(trips = sum(trip_weights)) %>%
      ungroup()
    
    # Infill for all combinations
    agg_all <- agg_all %>% 
      complete(p = c(11, 12, 13, 14, 15, 16, 18),
               tfn_at = ats_list,
               start_time = 1:6,
               main_mode = modes_list,
               fill = list(trips = 0)) %>%
      select(p, tfn_at, main_mode, start_time, trips) %>% 
      arrange(p, tfn_at, main_mode, start_time) 
    
    # Find sample sizes of proposed segmentation
    counts <- agg_all %>%
      group_by(p, tfn_at, main_mode) %>%
      mutate(count_seg1 = sum(trips)) %>%
      ungroup() %>%
      group_by(p, main_mode) %>%
      mutate(count_seg2 = sum(trips)) %>%
      ungroup()
    
    # First seg calculation
    seg1_split <- counts %>%
      filter(count_seg1 >= 300) %>%
      group_by(p, tfn_at, main_mode) %>%
      mutate(split = trips/sum(trips)) %>%
      ungroup() %>% 
      select(p, tfn_at, main_mode, start_time, split) %>% 
      arrange(p, tfn_at, main_mode)
    
    # 2nd Seg average infill
    seg2_infill <- agg_all %>% 
      group_by(p, main_mode, start_time) %>%
      summarise(trips = sum(trips)) %>%
      ungroup()
    
    seg2_infill_split <- seg2_infill %>% 
      group_by(p, main_mode) %>% 
      mutate(split = trips/sum(trips)) %>% 
      ungroup()

    # Seg 2 filter and join infill
    seg2_split <- counts %>%
      filter(count_seg1 < 300, count_seg2 >= 300) %>%
      distinct(p, tfn_at, main_mode) %>%
      left_join(seg2_infill_split) %>% 
      select(p, tfn_at, main_mode, start_time, split) %>% 
      arrange(p, tfn_at, main_mode, start_time)
    
    nhb_time_splits <- bind_rows(seg1_split, seg2_split)
    
    # Post process
    nhb_time_splits <- nhb_time_splits %>%
      rename(nhb_p = p,
             m = main_mode,
             tp = start_time)
    
    # Write out
    write_csv(nhb_time_splits, time_splits_output_dir)
    
    ts_comparison <- nhb_time_splits %>% 
      filter(m != 3) %>% 
      left_join(ts_control)
    
    lm(split ~ cte_ts, data = ts_comparison) %>% 
      summary()
    
    # Counts report
    c_report <- counts %>% 
      filter(tfn_at %in% c(2, 7)) %>% 
      mutate(tfn_at = case_when(
        tfn_at == 2 ~ 1,
        tfn_at == 7 ~ 8,
        TRUE ~ as.double(tfn_at)
      )) %>%
      bind_rows(counts) %>% 
      arrange(p, tfn_at, main_mode, start_time)
    
    c_report_out <- c_report %>% 
      distinct(p, tfn_at, main_mode, count_seg1, count_seg2) %>% 
      group_by(p) %>% 
      summarise(seg1 = sum(count_seg1 > 300),
                seg2 = sum((count_seg1 < 300 & count_seg2 > 300))) %>% 
      ungroup() %>% 
      mutate(total = seg1 + seg2) %>% 
      mutate(seg1_prop = seg1/total * 100,
             seg2_prop = seg2/total * 100)
    
    write_csv(c_report_out, time_splits_report_dir)
    
  }
  
}

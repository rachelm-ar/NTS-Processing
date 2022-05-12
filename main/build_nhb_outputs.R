# Main function ----------------------------------------------------------------
build_nhb_outputs <- function(input_csv, trip_rate, time_split, 
                              seg_max = 300){
  
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr")
  
  library_c(library_list)
  
  # Directories and read in ----------------------------------------------------
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
                                  "\\reports\\nhb_time_split_report",
                                  "_",
                                  input_csv$nhb_time_splits_version,
                                  ".csv")
  
  dir.create(str_c(input_csv$nhb_trip_rates_save_dir, "\\reports\\"), showWarnings = FALSE, recursive = TRUE)

  # Pre-processing -------------------------------------------------------------
  # set purpose [p] for nhb_trip_rate calculation
  cb <- escort_trips(cb, 1) # embed escort trips to main purpose

  # combine light & heavy rail
  cb <- data_filter(cb, hb_only = FALSE, remove_van = FALSE, remove_air = FALSE, 
                    aggregate_rail = TRUE)

  # area Types
  ats_list <- cb %>% 
    distinct(tfn_at) %>%
    pull() %>%
    sort()
  
  #set mode & purpose list for nhb_output
  modes_list <- c(1:3,5,6)
  p_list_hb  <- c(1:6,8)
  p_list_nhb <- c(11:16,18)
  
  ### CTripEnd pre-processing ###
  
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
  
  ### end of CTripEnd section ###
    
  # NHB Trip Rates -------------------------------------------------------------
  if(trip_rate){
    
    # HB_fr trips
    hb_trips <- cb %>%
      filter(trip_direction == "hb_fr", start_time %in% c(1:6)) %>%
      select(IndividualID, p, main_mode, trip_group, weighted_trips) %>%
      rename(hb_mode = main_mode, hb_purpose = p)
    
    # NHB trips
    nhb_trips <- cb %>%
      filter(trip_direction == "nhb", , start_time %in% c(1:6)) %>%
      select(IndividualID, p, main_mode, trip_group, weighted_trips) %>%
      rename(nhb_mode = main_mode, nhb_purpose = p)
    
    # link NHB trips to HB outward
    nhb_trips <- nhb_trips %>%
      left_join(hb_trips, by=c("IndividualID", "trip_group"), suffix = c("","_hb")) %>%
      select(!c("weighted_trips_hb"))
    
    # Group and sum out the identifiers and the people
    hb_trips <- hb_trips %>%
      group_by(hb_purpose, hb_mode) %>%
      summarise(hb_trips = sum(weighted_trips, na.rm=TRUE)) %>%
      ungroup()
    
    nhb_trips <- nhb_trips %>%
      group_by(nhb_purpose, nhb_mode, hb_purpose, hb_mode) %>%
      summarise(nhb_trips = sum(weighted_trips, na.rm=TRUE)) %>%
      ungroup()
    
    # Turn into trip rate (nhb by p/m/nhbp/nhbm divided by hb by p/m)
    nhb_trip_rates <- nhb_trips %>%
      left_join(hb_trips, by=c('hb_purpose', 'hb_mode')) %>%
      mutate(nhb_trip_rate = nhb_trips/hb_trips) %>% 
      filter(!is.na(hb_purpose))
    
    # remove invalid purposes (p7 & p17 are excluded)
    nhb_trip_rates <- nhb_trip_rates %>%
      filter(hb_purpose %in% p_list_hb, hb_mode %in% modes_list,
             nhb_purpose %in% p_list_nhb, nhb_mode %in% modes_list)
    
    # Infill 0 for missing segments
    nhb_trip_rates <- nhb_trip_rates %>%
      complete(nhb_purpose = p_list_nhb, nhb_mode = modes_list,
               hb_purpose = p_list_hb, hb_mode = modes_list,
               fill = list(nhb_trips = 0, hb_trips = 0, nhb_trip_rate = 0))
    
    # Check against NTEM rates (IgammaNHBH)
    ntem_comparison <- nhb_trip_rates %>%
      left_join(tr_control, by=c('hb_purpose', 'hb_mode', 'nhb_purpose', 'nhb_mode')) %>% 
      filter(hb_mode !=3, nhb_mode != 3)
    
    lm(nhb_trip_rate ~ cte_tr, data = ntem_comparison) %>%
      summary()
    
    nhb_trip_rates <- nhb_trip_rates %>% 
      rename(nhb_p = nhb_purpose, nhb_m = nhb_mode,
             p = hb_purpose, m = hb_mode) %>%
      arrange(nhb_p, nhb_m, p, m)
    
    # Write out
    write_csv(nhb_trip_rates, trip_rates_output_dir)
    
  }  
  
  # NHB Time split -------------------------------------------------------------
  if(time_split){
    # filter for nhb trips
    ts_nhb_trips <- cb %>% 
      filter(trip_direction == "nhb", start_time %in% c(1:6), 
             p %in% p_list_nhb, tfn_at %in% ats_list, main_mode %in% modes_list)

    # aggregate 
    agg_all <- ts_nhb_trips %>%
      select(p, tfn_at, start_time, main_mode, weighted_trips) %>%
      group_by(p, tfn_at, start_time, main_mode) %>%
      summarise(trips = sum(weighted_trips, na.rm=TRUE)) %>%
      ungroup()
    
    # infill for all combinations
    agg_all <- agg_all %>% 
      complete(p = p_list_nhb, tfn_at = ats_list,
               start_time = c(1:6), main_mode = modes_list,
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
      filter(count_seg1 >= seg_max) %>%
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
      filter(count_seg1 < seg_max, count_seg2 >= seg_max) %>%
      distinct(p, tfn_at, main_mode) %>%
      left_join(seg2_infill_split) %>% 
      select(p, tfn_at, main_mode, start_time, split) %>% 
      arrange(p, tfn_at, main_mode, start_time)
    
    nhb_time_splits <- bind_rows(seg1_split, seg2_split)
    
    # Post process
    nhb_time_splits <- nhb_time_splits %>%
      rename(nhb_p = p, nhb_m = main_mode, tp = start_time) %>%
      arrange(nhb_p, tfn_at, nhb_m, tp)
    
    # Write out
    write_csv(nhb_time_splits, time_splits_output_dir)
    
    ts_comparison <- nhb_time_splits %>% 
      filter(nhb_m != 3) %>% 
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
      summarise(seg1 = sum(count_seg1 >= seg_max),
                seg2 = sum((count_seg1 < seg_max & count_seg2 >= seg_max))) %>% 
      ungroup() %>% 
      mutate(total = seg1 + seg2) %>% 
      mutate(seg1_prop = seg1/total * 100,
             seg2_prop = seg2/total * 100)
    
    write_csv(c_report_out, time_splits_report_dir)
    
  }
  
}

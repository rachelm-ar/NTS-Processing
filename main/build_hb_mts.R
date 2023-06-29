# supporting functions ---------------------------------------------------------
data_filter <- function(cb, hb_only = FALSE, remove_van = FALSE, 
                        remove_air = FALSE, aggregate_rail = FALSE){
# filter for hb trip purpose
  if (hb_only) {
    cb <- filter(cb, p %in% c(1:8))
  }

# remove van and air
  if(remove_van){
    cb <- filter(cb, !main_mode %in% c(4))
  }

  if(remove_air){
    cb <- filter(cb, !main_mode %in% c(8))
  }
  
# aggregate modes: Light & Surface Rail
  if(aggregate_rail){
    cb <- mutate(cb, main_mode = ifelse(main_mode == 7, 6, main_mode))
  }
  
  # return output
  return(cb)  
}

# Phi factor calculation -------------------------------------------------------
phi_factors <- function(cb, hb_phi_output_dir){
  # filter for tour purpose
  hb_fr <- cb %>%
    filter(trip_direction == "hb_fr") %>%
    select(IndividualID, TripID, DayID, main_mode, start_time, 
           trip_purpose, p, trip_group, W5xHH, W2, JJXSC, weighted_trips) %>%
    arrange(IndividualID, DayID, TripID)

  hb_to <- cb %>%
    filter(trip_direction == "hb_to") %>%
    select(IndividualID, TripID, DayID, main_mode, start_time, 
           trip_purpose, p, trip_group, weighted_trips, W5xHH, W2, JJXSC) %>%
    arrange(IndividualID, DayID, TripID)  
  
  tour <- hb_fr %>%
    left_join(hb_to, by = c("IndividualID","trip_group"), suffix = c("","_return")) %>%
    filter(p != 0, !is.na(start_time), !is.na(start_time_return))

  # filter hb, remove Van & air and combine light & heavy rail
  tour <- data_filter(tour, hb_only = TRUE, remove_van = TRUE, remove_air = TRUE, 
                      aggregate_rail = TRUE)
  
  tour <- tour %>%
    group_by(main_mode, p, start_time, start_time_return) %>%
    summarise(phis = sum(weighted_trips, na.rm = TRUE)) %>% #, trips_to = sum(weighted_trips_return)) %>%
    rename(purpose_from_home = p, time_from_home = start_time, time_to_home = start_time_return)
  
  # calculate phis
  phis <- tour %>%
    complete(time_to_home = c(1:6), fill = list(phis = 0)) %>%
    group_by(main_mode, purpose_from_home, time_from_home) %>%
    mutate(phis = phis/sum(phis, na.rm = TRUE)) %>% 
    ungroup()
    
  # write to csv
  modes_list <- phis %>% 
    distinct(main_mode) %>% 
    pull() %>% 
    sort()
  
  for (m in modes_list) {
    phi_m <- phis %>%
      filter(main_mode == m) %>%
      select(!c("main_mode")) %>%
      arrange(purpose_from_home, time_from_home, time_to_home) %>%
      rename(direction_factor = phis)
    
    write_csv(phi_m, str_replace(hb_phi_output_dir, "m.csv", paste("m", m, ".csv", sep="")))
  }
  
  return (tour)
}


# Main function ----------------------------------------------------------------
build_hb_mts <- function(input_csv, seg_max = 300){
  # Load packages
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr")
  
  library_c(library_list)
  
  # Read input csv
  input_csv <- read_csv(input_csv)
  input_csv <- transpose_input_csv(input_csv)
  
  # Read Classified build
  cb <- read_csv(input_csv$cb_csv_dir)
  
  # hb mts long dir
  hb_mts_long_output_dir <- str_c(input_csv$hb_mts_save_dir,
                                  "\\",
                                  input_csv$hb_mts_long_name,
                                  "_",
                                  input_csv$hb_mts_version,
                                  ".csv")
  
  # hb mts wide dir
  hb_mts_wide_output_dir <- str_c(input_csv$hb_mts_save_dir,
                                  "\\",
                                  input_csv$hb_mts_wide_name,
                                  "_",
                                  input_csv$hb_mts_version,
                                  ".csv")
  
  # hb mts count report dir
  hb_mts_report_dir <- str_c(input_csv$hb_mts_save_dir, 
                             "\\reports\\hb_mts_report",
                             "_",
                             input_csv$hb_mts_version,
                             ".csv")
  
  # hb phi output dir
  hb_phi_output_dir <- str_c(input_csv$hb_phi_save_dir, 
                             "\\phi_factors_m.csv")
  
  #create report folder
  dir.create(str_c(input_csv$hb_mts_save_dir, "\\reports\\"), showWarnings = FALSE, recursive = TRUE)
  
  #create phi_factor folder
  dir.create(str_c(input_csv$hb_phi_save_dir), showWarnings = FALSE, recursive = TRUE)
  
  tfn_lu <- read_csv(input_csv$tfn_tt_lu_csv_dir)

  # Phi factors calculation ----------------------------------------------------
  # will confirm if escort trips are included in phi factors
  phi <- escort_trips(cb, 3) # exclude escort trips for phi factors
  
  # calculate phi factor
  phi <- phi_factors(phi, hb_phi_output_dir)
  
  # Mode Time Split Methodology ------------------------------------------------
  # filter hb, remove van, air, aggregate rail
  cb <- data_filter(cb, hb_only = TRUE, remove_van = TRUE, remove_air = TRUE, 
                    aggregate_rail = TRUE)

  
  # rename mode and time period
  cb <- rename(cb, m = main_mode, tp = start_time)
  
  # ntem tt to tfn tt lookup
  ntem_to_tfn_tt_lu <- tfn_lu %>% 
    select(tfn_tt, ntem_tt)
  
  # select only necessary columns of lookup
  ntem_to_hh_type_lu <- tfn_lu %>%
    select(ntem_tt, hh_type) %>%
    distinct()
  
  # modes depending on ntem or tfn
  modes_list <- cb %>% 
    distinct(m) %>% 
    pull() %>% 
    sort()
  
  # ntem_tt & hh_type list
  tts_list <- c(1:88)
  hhs_list <- c(1:8)
  
  # aggregate 
  agg_all <- cb %>%
    select(p, ntem_tt, tp, m, weighted_trips) %>%
    group_by(p, ntem_tt, tp, m) %>%
    summarise(trips = sum(weighted_trips)) %>%
    ungroup()
  
  # infill for all combinations
  agg_all <- agg_all %>% 
    complete(p = c(1:8),
             ntem_tt = tts_list, tp = c(1:6),
             m = modes_list, fill = list(trips = 0)) %>%
    left_join(ntem_to_hh_type_lu) %>% 
    select(p, ntem_tt, hh_type, tp, m, trips) %>% 
    arrange(p, ntem_tt, hh_type)
    # no filter at this stage (i.e. include NA) for better sample

  # level 1: purpose, area_type, hh_type, ntem_tt
  # level 2: purpose, area_type, hh_type
  # level 3: purpose, area_type
  # level 4: purpose

  # find sample sizes of proposed segmentation
  counts <- agg_all %>%
    group_by(p, hh_type, ntem_tt) %>%
    mutate(count_seg1 = sum(trips)) %>%
    ungroup() %>%
    group_by(p, hh_type) %>%
    mutate(count_seg2 = sum(trips)) %>%
    ungroup() %>%
    group_by(p) %>%
    mutate(count_seg3 = sum(trips)) %>%
    ungroup()

  # seg2 average infill
  seg2_infill <- agg_all %>% 
    group_by(p, hh_type, tp, m) %>%
    summarise(trips = sum(trips)) %>%
    ungroup()
  
  seg2_infill_split <- seg2_infill %>% 
    group_by(p, hh_type) %>% 
    mutate(split = trips/sum(trips)) %>% 
    ungroup() %>% 
    left_join(ntem_to_hh_type_lu) %>% 
    select(p, ntem_tt, tp, m, split) %>%
    arrange(p, ntem_tt, tp, m)
  
  # seg3 average infill
  seg3_infill <- agg_all %>%
    group_by(p, tp, m) %>%
    summarise(trips = sum(trips)) %>%
    ungroup()
  
  seg3_infill_split <- seg3_infill %>%
    group_by(p) %>%
    mutate(split = trips/sum(trips)) %>% 
    ungroup() %>% 
    mutate(hh_type = 1) %>% #set hh_type to 1 then make a copy to other hhs
    complete(nesting(p, tp, m), hh_type = hhs_list) %>% 
    fill(split) %>% 
    left_join(ntem_to_hh_type_lu) %>% 
    select(p, ntem_tt, tp, m, split) %>%
    arrange(p, ntem_tt, tp, m) %>% 
    ungroup()

  #write_csv(seg4_infill,"D:/NTS/NTS_weighted_trips_seg4.csv")

  # seg1 split calculation
  seg1_split <- counts %>%
    filter(count_seg1 >= seg_max) %>%
    group_by(p, hh_type, ntem_tt) %>%
    mutate(split = trips/sum(trips)) %>%
    ungroup() %>% 
    select(p, ntem_tt, tp, m, split) %>% 
    arrange(p, ntem_tt, tp, m)
  
  # seg2 filter and join infill
  seg2_split <- counts %>%
    filter(count_seg1 < seg_max & count_seg2 >= seg_max) %>%
    distinct(p, ntem_tt) %>%
    left_join(seg2_infill_split)
  
  # seg3 filter and join infill
  seg3_split <- counts %>%
    filter(count_seg1 < seg_max & count_seg2 < seg_max & count_seg3 >= seg_max) %>%
    distinct(p, ntem_tt) %>%
    left_join(seg3_infill_split)

  # combine finished dataframes
  mts <- bind_rows(seg1_split,
                   seg2_split,
                   seg3_split) %>%
    arrange(p, ntem_tt, tp, m)
  
  mts_long <- mts %>%
    filter(!is.na(ntem_tt), !is.na(tp)) 
  
  # long format for tms
  mts_output <- mts_long %>% 
    left_join(ntem_to_tfn_tt_lu) %>% 
    select(p, tfn_tt, ntem_tt, tp, m, split)
  
  write_csv(mts_output, hb_mts_long_output_dir)
  
  # pivot to wide format
  mts_wide <- mts_long %>%
    mutate(tp = str_c("tp", tp),
           m = str_c("m", m)) %>%
    unite(tpm, tp, m, sep = "_") %>% 
    pivot_wider(names_from = tpm, values_from = split)

  write_csv(mts_wide, hb_mts_wide_output_dir)
  
  # counts report -----------------------------------------------------------
  c_report <- counts %>% 
    bind_rows(counts) %>% 
    arrange(p, ntem_tt, hh_type, tp, m) %>%
    filter(!is.na(ntem_tt) & !is.na(hh_type))

  c_report_out <- c_report %>% 
    distinct(p, ntem_tt, hh_type, count_seg1, count_seg2, count_seg3) %>% 
    group_by(p, ntem_tt) %>% 
    summarise(seg1 = sum(count_seg1 >= seg_max),
              seg2 = sum((count_seg1 < seg_max & count_seg2 >= seg_max)),
              seg3 = sum((count_seg1 < seg_max & count_seg2 < seg_max & count_seg3 >= seg_max))) %>% 
    ungroup() %>% 
    mutate(total = seg1 + seg2 + seg3) %>% 
    mutate(seg1_prop = seg1/total * 100,
           seg2_prop = seg2/total * 100,
           seg3_prop = seg3/total * 100)

  write_csv(c_report_out, hb_mts_report_dir)

  #cleanse database
  gc()  
}

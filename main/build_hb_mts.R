# Process:
#' 1. Group by p, tfn_at, ntem_tt. If sum(trips) for ith m(i) and jth tp(j) > 300 then calculate split
#' 2. Else group by p, tfn_at, hh_type. If sum(trips) for ith m(i) and jth tp(j) > 300 then calculate split
#' 3. Else group by p, hh_type and calculate split
#' Work backwards for each segment and combine together

build_hb_mts <- function(user, drive, tfn_or_ntem){
  
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr")
  
  library_c(library_list)
  
  # NTS Directories
  y_dir <- "Y:/NTS/"
  c_dir <- str_c("C:/Users/", user, "/Documents/NTS_C/")
  nts_dir <- ifelse(drive == "Y", y_dir, c_dir)
  
  # CB Directory
  cb_dir <- str_c(nts_dir, "classified builds/cb_", tfn_or_ntem, ".csv")
  
  # Export dir
  export_dir <- str_c(nts_dir, "outputs/hb/hb_mode_time_split/hb_mts_")
  wide_dir <- str_c(export_dir, tfn_or_ntem, "_wide.csv")
  tms_dir <- str_c(export_dir, "tms.csv")
  segments_out <- str_c(nts_dir, "outputs/hb/hb_mode_time_split/segments_report.csv")
  
  # Tfn lu read
  tfn_lu_dir <- str_c(nts_dir, "lookups/tfn_traveller_type.csv")
  
  # Read
  cb <- read_csv(cb_dir)
  tfn_lu <- read_csv(tfn_lu_dir)
  
  # Pre Processing ---------------------------------------------------------
  
  if(tfn_or_ntem == "ntem"){
    
    cb <- cb %>%
      rename(m = ntem_main_mode,
             area_type = ntem_at)
    
  } else if (tfn_or_ntem == "tfn"){
    
    cb <- cb %>% 
      rename(m = main_mode,
             area_type = tfn_at)
    
  }
  
  cb <- rename(cb, tp = start_time)
  
  # Filter for hb trip purpose
  cb <- filter(cb, p %in% 1:8)
  
  # Remove Air
  cb <- filter(cb, m != 8)
  
  # Diary sample only
  cb <- filter(cb, W1 == 1)
  
  # Add weights
  cb <- mutate(cb, trip_weights = W5xHH * JJXSC)
  
  if(tfn_or_ntem == "tfn"){
    
    # Temporary solution - AT aggregate 1 & 2
    cb <- mutate(cb, area_type = ifelse(area_type == 2, 1 , area_type))
    
    # Temporary solution - AT aggregate 
    cb <- mutate(cb, area_type = ifelse(area_type == 7, 8, area_type))
    
    # Aggregate modes: Car & Van
    cb <- mutate(cb, m = ifelse(m == 4, 3, m))
    
    # Aggregate modes: Light & Surface Rail
    cb <- mutate(cb, m = ifelse(m == 7, 6, m))
    
  }
  
  # Remove na area type
  cb <- filter(cb, !is.na(area_type))
  
  # Ntem tt to tfn tt lookup
  ntem_to_tfn_tt_lu <- tfn_lu %>% 
    select(tfn_tt, ntem_tt)
  
  # Select only necessary columns of lookup
  ntem_to_hh_type_lu <- tfn_lu %>%
    select(ntem_tt, hh_type) %>%
    distinct()
  
  # Mode Time Split Methodology ---------------------------------------------
  
  # Modes depending on ntem or tfn
  modes_list <- cb %>% 
    distinct(m) %>% 
    pull() %>% 
    sort()
  
  # Area type depending on ntem or tfn
  ats_list <- cb %>% 
    distinct(area_type) %>%
    pull() %>%
    sort()
  
  # Aggregate 
  agg_all <- cb %>%
    select(p, area_type, ntem_tt, tp, m, trip_weights) %>%
    group_by(p, area_type, ntem_tt, tp, m) %>%
    summarise(trips = sum(trip_weights)) %>%
    ungroup()
  
  # Infill for all combinations
  agg_all <- agg_all %>% 
    complete(p = 1:8,
             area_type = ats_list,
             ntem_tt = 1:88,
             tp = 1:6,
             m = modes_list,
             fill = list(trips = 0)) %>%
    left_join(ntem_to_hh_type_lu) %>% 
    select(p, area_type, ntem_tt, hh_type, tp, m, trips) %>% 
    arrange(p, area_type, ntem_tt, hh_type) 
  
  # Find sample sizes of proposed segmentation
  counts <- agg_all %>%
    group_by(p, area_type, ntem_tt) %>%
    mutate(count_seg1 = sum(trips)) %>%
    ungroup() %>%
    group_by(p, area_type, hh_type) %>%
    mutate(count_seg2 = sum(trips)) %>%
    ungroup() %>%
    group_by(p, hh_type) %>%
    mutate(count_seg3 = sum(trips)) %>%
    ungroup()
  
  # First seg calculation
  seg1_split <- counts %>%
    filter(count_seg1 >= 300) %>%
    group_by(p, area_type, ntem_tt) %>%
    mutate(split = trips/sum(trips)) %>%
    ungroup() %>% 
    select(p, area_type, ntem_tt, tp, m, split) %>% 
    arrange(p, area_type, ntem_tt, tp, m)
  
  # 2nd Seg average infill
  seg2_infill <- agg_all %>% 
    group_by(p, area_type, hh_type, tp, m) %>%
    summarise(trips = sum(trips)) %>%
    ungroup()
  
  seg2_infill_split <- seg2_infill %>% 
    group_by(p, area_type, hh_type) %>% 
    mutate(split = trips/sum(trips)) %>% 
    ungroup() %>% 
    left_join(ntem_to_hh_type_lu) %>% 
    select(p, area_type, ntem_tt, tp, m, split) %>%
    arrange(p, area_type, ntem_tt, tp, m)
  
  # 3rd Seg average infill
  seg3_infill <- agg_all %>%
    group_by(p, hh_type, tp, m) %>%
    summarise(trips = sum(trips)) %>%
    ungroup()
  
  seg3_infill_split <- seg3_infill %>%
    group_by(p, hh_type) %>%
    mutate(split = trips/sum(trips)) %>% 
    ungroup() %>% 
    mutate(area_type = 1) %>%
    complete(nesting(p, hh_type, tp, m),
             area_type = ats_list) %>% 
    fill(split) %>% 
    left_join(ntem_to_hh_type_lu) %>% 
    select(p, area_type, ntem_tt, tp, m, split) %>%
    arrange(p, area_type, ntem_tt, tp, m) %>% 
    ungroup()
  
  # Seg 2 filter and join infill
  seg2_split <- counts %>%
    filter(count_seg1 < 300, count_seg2 >= 300) %>%
    distinct(p, area_type, ntem_tt) %>%
    left_join(seg2_infill_split)
  
  # Seg 3 filter and join infill
  seg3_split <- counts %>%
    filter(count_seg2 < 300) %>%
    distinct(p, area_type, ntem_tt) %>%
    left_join(seg3_infill_split)
  
  # Combine finished dataframes
  mts <- bind_rows(seg1_split,
                   seg2_split,
                   seg3_split) %>%
    arrange(p, area_type, ntem_tt, tp, m)
  
  if(tfn_or_ntem == "tfn"){
    
    mts <- mts %>%
      filter(area_type == 8) %>%
      mutate(area_type = 7) %>%
      bind_rows(mts)
    
    mts <- mts %>%
      filter(area_type == 1) %>%
      mutate(area_type = 2) %>%
      bind_rows(mts) %>%
      arrange(p, area_type, ntem_tt, tp, m)
    
    
  }
  
  mts_long <- mts %>%
    filter(!is.na(area_type)) 
  
  tms_output <- mts_long %>% 
    left_join(ntem_to_tfn_tt_lu) %>% 
    select(p, tfn_tt, ntem_tt, area_type, tp, m, split)
  
  # Pivot to Wide format
  mts_wide <- mts_long %>%
    mutate(tp = str_c("tp", tp),
           m = str_c("m", m)) %>%
    unite(tpm, tp, m, sep = "_") %>% 
    pivot_wider(names_from = tpm, values_from = split)
  
  if(tfn_or_ntem == "tfn"){
    
    mts_wide %>% 
      rename(tfn_at = area_type) %>% 
      write_csv(wide_dir)
    
    write_csv(tms_output, tms_dir)
    
  } else if(tfn_or_ntem == "ntem") {
    
    mts_wide %>% 
      rename(ntem_at = area_type) %>% 
      write_csv(wide_dir)
    
  }
  
# Counts Report -----------------------------------------------------------
  
  c_report <- counts %>% 
    filter(area_type %in% c(1, 8)) %>% 
    mutate(area_type = case_when(
      area_type == 1 ~ 2,
      area_type == 8 ~ 7,
      TRUE ~ as.double(area_type)
    )) %>%
    bind_rows(counts) %>% 
    arrange(p, area_type, ntem_tt, hh_type, tp, m)
  
  c_report_out <- c_report %>% 
    distinct(p, area_type, ntem_tt, hh_type, count_seg1, count_seg2, count_seg3) %>% 
    group_by(p) %>% 
    summarise(seg1 = sum(count_seg1 > 300),
              seg2 = sum((count_seg1 < 300 & count_seg2 > 300)),
              seg3 = sum((count_seg2 < 300 & count_seg3 > 300))) %>% 
    ungroup() %>% 
    mutate(total = seg1 + seg2 + seg3) %>% 
    mutate(seg1_prop = seg1/total * 100,
           seg2_prop = seg2/total * 100,
           seg3_prop = seg3/total * 100)
  
  write_csv(c_report_out, segments_out)
  
  # counts %>% filter(area_type == 6, ntem_tt == 6) %>% filter(trips != 0)
  
}
build_hb_mts <- function(input_csv, seg_max = 300){
  
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
                                  "\\Reports\\hb_mts_report",
                                  "_",
                                  input_csv$hb_mts_version,
                                  ".csv")
  
  dir.create(str_c(input_csv$hb_mts_save_dir, "\\Reports\\"), showWarnings = FALSE, recursive = TRUE)
  
  tfn_lu <- read_csv(input_csv$tfn_tt_lu_csv_dir)
  
  # Pre Processing ---------------------------------------------------------
  
  cb <- cb %>%
    rename(m = main_mode,
           tp = start_time)
  
  # Filter for hb trip purpose
  cb <- filter(cb, p %in% 1:8)
  
  # Remove van and air
  cb <- filter(cb, !m %in% c(4, 8))
  
  # Diary sample only
  cb <- filter(cb, W1 == 1)
  
  # Aggregate modes: Light & Surface Rail
  cb <- mutate(cb, m = ifelse(m == 7, 6, m))
  
  # Remove na area type
  cb <- filter(cb, !is.na(tfn_at))
  
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
    distinct(tfn_at) %>%
    pull() %>%
    sort()
  
  # Aggregate 
  agg_all <- cb %>%
    select(p, tfn_at, ntem_tt, tp, m, weighted_trips) %>%
    group_by(p, tfn_at, ntem_tt, tp, m) %>%
    summarise(trips = sum(weighted_trips)) %>%
    ungroup()
  
  # Infill for all combinations
  agg_all <- agg_all %>% 
    complete(p = 1:8,
             tfn_at = ats_list,
             ntem_tt = 1:88,
             tp = 1:6,
             m = modes_list,
             fill = list(trips = 0)) %>%
    left_join(ntem_to_hh_type_lu) %>% 
    select(p, tfn_at, ntem_tt, hh_type, tp, m, trips) %>% 
    arrange(p, tfn_at, ntem_tt, hh_type) 
  
  # level 1 : purpose, area_type, ntem_tt
  # level 2: purpose, area_type, hh_type
  # level 3: purpose, hh_type
  
  # Find sample sizes of proposed segmentation
  counts <- agg_all %>%
    group_by(p, tfn_at, ntem_tt) %>%
    mutate(count_seg1 = sum(trips)) %>%
    ungroup() %>%
    group_by(p, tfn_at, hh_type) %>%
    mutate(count_seg2 = sum(trips)) %>%
    ungroup() %>%
    group_by(p, hh_type) %>%
    mutate(count_seg3 = sum(trips)) %>%
    ungroup()
  
  # First seg calculation
  seg1_split <- counts %>%
    filter(count_seg1 >= seg_max) %>%
    group_by(p, tfn_at, ntem_tt) %>%
    mutate(split = trips/sum(trips)) %>%
    ungroup() %>% 
    select(p, tfn_at, ntem_tt, tp, m, split) %>% 
    arrange(p, tfn_at, ntem_tt, tp, m)
  
  # 2nd Seg average infill
  seg2_infill <- agg_all %>% 
    group_by(p, tfn_at, hh_type, tp, m) %>%
    summarise(trips = sum(trips)) %>%
    ungroup()
  
  seg2_infill_split <- seg2_infill %>% 
    group_by(p, tfn_at, hh_type) %>% 
    mutate(split = trips/sum(trips)) %>% 
    ungroup() %>% 
    left_join(ntem_to_hh_type_lu) %>% 
    select(p, tfn_at, ntem_tt, tp, m, split) %>%
    arrange(p, tfn_at, ntem_tt, tp, m)
  
  # 3rd Seg average infill
  seg3_infill <- agg_all %>%
    group_by(p, hh_type, tp, m) %>%
    summarise(trips = sum(trips)) %>%
    ungroup()
  
  seg3_infill_split <- seg3_infill %>%
    group_by(p, hh_type) %>%
    mutate(split = trips/sum(trips)) %>% 
    ungroup() %>% 
    mutate(tfn_at = 1) %>%
    complete(nesting(p, hh_type, tp, m),
             tfn_at = ats_list) %>% 
    fill(split) %>% 
    left_join(ntem_to_hh_type_lu) %>% 
    select(p, tfn_at, ntem_tt, tp, m, split) %>%
    arrange(p, tfn_at, ntem_tt, tp, m) %>% 
    ungroup()
  
  # Seg 2 filter and join infill
  seg2_split <- counts %>%
    filter(count_seg1 < seg_max, count_seg2 >= seg_max) %>%
    distinct(p, tfn_at, ntem_tt) %>%
    left_join(seg2_infill_split)
  
  # Seg 3 filter and join infill
  seg3_split <- counts %>%
    filter(count_seg2 < seg_max) %>%
    distinct(p, tfn_at, ntem_tt) %>%
    left_join(seg3_infill_split)
  
  # Combine finished dataframes
  mts <- bind_rows(seg1_split,
                   seg2_split,
                   seg3_split) %>%
    arrange(p, tfn_at, ntem_tt, tp, m)
  
  mts_long <- mts %>%
    filter(!is.na(tfn_at)) 
  
  # Long format for tms
  
  tms_output <- mts_long %>% 
    left_join(ntem_to_tfn_tt_lu) %>% 
    select(p, tfn_tt, ntem_tt, tfn_at, tp, m, split)
  
  write_csv(tms_output, hb_mts_long_output_dir)
  
  # Pivot to Wide format
  mts_wide <- mts_long %>%
    mutate(tp = str_c("tp", tp),
           m = str_c("m", m)) %>%
    unite(tpm, tp, m, sep = "_") %>% 
    pivot_wider(names_from = tpm, values_from = split)

  write_csv(mts_wide, hb_mts_wide_output_dir)
  
  # Counts Report -----------------------------------------------------------
  
  c_report <- counts %>% 
    filter(tfn_at %in% c(1, 8)) %>% 
    mutate(tfn_at = case_when(
      tfn_at == 1 ~ 2,
      tfn_at == 8 ~ 7,
      TRUE ~ as.double(tfn_at)
    )) %>%
    bind_rows(counts) %>% 
    arrange(p, tfn_at, ntem_tt, hh_type, tp, m)
  
  c_report_out <- c_report %>% 
    distinct(p, tfn_at, ntem_tt, hh_type, count_seg1, count_seg2, count_seg3) %>% 
    group_by(p) %>% 
    summarise(seg1 = sum(count_seg1 > seg_max),
              seg2 = sum((count_seg1 < seg_max & count_seg2 > seg_max)),
              seg3 = sum((count_seg2 < seg_max & count_seg3 > seg_max))) %>% 
    ungroup() %>% 
    mutate(total = seg1 + seg2 + seg3) %>% 
    mutate(seg1_prop = seg1/total * 100,
           seg2_prop = seg2/total * 100,
           seg3_prop = seg3/total * 100)
  
  write_csv(c_report_out, hb_mts_report_dir)
  
}

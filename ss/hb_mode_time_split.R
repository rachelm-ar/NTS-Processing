# TODO:
# 1. Compare against NTEM - Align AT and mode differences in comparison
cb_name <- "ntem"
drive = "C"
user <- Sys.info()[[6]]
repo_dir <- paste0("C:/Users/", user, "/Documents/GitHub/NTS-Processing/")

# Load custom functions
source(paste0(repo_dir, "utils.R"))
source(paste0(repo_dir, "lookups.R"))

extract_hb_mts <- function(cb_name,
                           drive, 
                           user){
  
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr")
  
  library_c(library_list)
  
  # Directories -------------------------------------------------------------
  
  y_dir <- "Y:/NTS/"
  c_dir <- str_c("C:/Users/", user, "/Documents/NTS_C/")
  nts_dir <- ifelse(drive == "Y", y_dir, c_dir)
  
  cb_dir <- str_c(nts_dir, "classified builds/cb_", cb_name, ".csv")
  
  export_dir <- str_c(nts_dir, "outputs/hb/")
  dir.create(export_dir, showWarnings = FALSE)
  
  # Read classified_build
  cb <- read_csv(cb_dir)
  
  # Pre Processing ---------------------------------------------------------
  
  if(cb_name == "ntem"){
    
    cb <- cb %>%
      mutate(main_mode = ntem_main_mode)
    
    cb <- cb %>%
      mutate(tfn_at = ntem_at)
    
  }
  
  # Filter for hb trip purpose
  cb <- filter(cb, trip_purpose %in% 1:8)
  
  # Remove Air
  cb <- filter(cb, main_mode != 8)
  
  # Diary sample only
  cb <- filter(cb, W1 == 1)
  
  # Add weights
  cb <- mutate(cb, trip_weights = W5xHH * JJXSC)
  
  if(cb_name == "tfn"){
    
    # Temporary solution - AT aggregate 1 & 2
    cb <- mutate(cb, tfn_area_type = ifelse(tfn_area_type == 2, 1 , tfn_area_type))
    
    # Aggregate modes: Car & Van
    cb <- mutate(cb, main_mode = ifelse(main_mode == 4, 3, main_mode))
    
    # Aggregate modes: Light & Surface Rail
    cb <- mutate(cb, main_mode = ifelse(main_mode == 7, 6, main_mode))
    
  }
  
  # Renaming
  cb <- cb %>%
    rename(p = trip_purpose,
           tp = start_time,
           m = main_mode)
  
  # temp
  # cb <- lu_ntem_tt(cb)
  
  # Calculate mode/time split
  tpm_long <- cb %>%
    select(p, ntem_tt, tfn_at, tp, m, trip_weights) %>%
    group_by(p, ntem_tt, tfn_at, tp, m) %>%
    summarise(total_trips = sum(trip_weights, na.rm = TRUE)) %>%
    group_by(p, ntem_tt, tfn_at) %>%
    mutate(split = total_trips/sum(total_trips),
           count = n()) %>% 
    ungroup()

  if(cb_name == "tfn") {
    
    # Add area type 2 back in
    tpm_long <- tpm_long %>%
      filter(tfn_at == 1) %>%
      mutate(tfn_at = 2) %>%
      bind_rows(tpm_long) %>%
      select(p, tfn_at, ntem_tt, tp, m, total_trips, split) %>%
      arrange(p, tfn_at, ntem_tt, tp, m)
    
  }
  
  modes_list <- cb %>% distinct(m) %>% pull() %>% sort()
  
  # Fill in missing tp's and m's for available combination
  tpm_long <- tpm_long %>%
    complete(nesting(p, tfn_at, ntem_tt),
             tp = 1:6,
             m = modes_list,
             fill = list(total_trips = 0,
                         split = 0))
  
  # Calculate averages by trip purpose and tfn area type
  tpm_infill <- tpm_long %>%
    group_by(p, tfn_at, tp, m) %>%
    summarise(total_trips = sum(total_trips)) %>% 
    group_by(p, tfn_at) %>%
    mutate(split = total_trips/sum(total_trips)) %>% 
    ungroup() %>%
    select(-total_trips)
  
  # Fill in any missing zeros with averages and add ntem traveller type back in
  tpm_infill <- tpm_infill %>%
    complete(p = 1:8, tfn_at = 1:8, tp = 1:6, m = modes_list, fill = list(split = 0)) %>%
    mutate(ntem_tt = 1) %>%
    complete(nesting(p, tfn_at, tp, m), ntem_tt = 1:88) %>%
    fill(split) %>%
    select(p, tfn_at, ntem_tt, tp, m, split) %>%
    arrange(p, tfn_at, ntem_tt, tp, m)
  
  # Find which rows are missing
  cur_combos <- tpm_long %>%
    select(p, tfn_at, ntem_tt) %>%
    distinct()
  
  all_combos <- expand_grid(p = 1:8,
                            tfn_at = 1:8,
                            ntem_tt = 1:88)
    
  missing_combos <- setdiff(all_combos, cur_combos)
  
  tpm_infill <- tpm_infill %>%
    inner_join(missing_combos)
    
  tpm_long <- tpm_long %>%
    bind_rows(tpm_infill) %>%
    select(-total_trips)
  
  # Extra step to add in: ntem to tfn tt lookup
  ntem_to_tfn_tt <- read_csv("C:/Users/Pluto/Documents/NTS_C/lookups/ntem to tfn tt.csv")
  
  # Add in tfn_tt
  tpm_long_out <- tpm_long %>%
    left_join(ntem_to_tfn_tt, by = c('ntem_tt' = 'ntem_traveller_type')) %>%
    rename(tfn_tt = tfn_traveller_type) %>%
    select(p, tfn_tt, tfn_at, tp, m, split) %>%
    arrange(p, tfn_tt, tfn_at, tp, m)

  write_csv(tpm_long_out, str_c(export_dir, "hb_mode_time_split_v1.9.csv"))
  
  # Time Split - Week -------------------------------------------------------
  tpm_out <- tpm_long %>%
    mutate(tp = str_c("tp", tp),
           m = str_c("m", m)) %>%
    unite(tpm, tp, m, sep = "_") %>% 
    pivot_wider(names_from = tpm, values_from = split)
  
  write_csv(tpm_out, str_c(export_dir, "hb_mode_time_split_v1.9_ha.csv"))
  
}

# Quick check to see if ntem mode time split any better
# Filter for hb trip purpose
cb <- filter(cb, trip_purpose %in% 1:8)

# NTEM area type
cb <- cb %>%
  mutate(ntem_at = case_when(
    PSUAreaType2_B01ID == 1 ~ 1,
    PSUAreaType2_B01ID == 2 ~ 2,
    PSUAreaType2_B01ID %in% 3:9 ~ 3,
    PSUAreaType2_B01ID == 10 ~ 4,
    PSUAreaType2_B01ID %in% 11:13 ~ 5,
    PSUAreaType2_B01ID == 14 ~ 6,
    PSUAreaType2_B01ID == 15 ~ 7,
    PSUAreaType2_B01ID == 16 ~ 8
  ))

# remove na
cb <- cb %>% filter(!is.na(ntem_at))

# Remove Air
cb <- filter(cb, main_mode != 8)

# Temporary solution - AT aggregate 1 & 2
cb <- mutate(cb, tfn_area_type = ifelse(tfn_area_type == 2, 1 , tfn_area_type))

# Diary sample only
cb <- filter(cb, W1 == 1)

# Add weights
cb <- mutate(cb, trip_weights = W2 * W5 * JJXSC)

# Aggregate modes: Car & Van
cb <- mutate(cb, main_mode = ifelse(main_mode == 4, 3, main_mode))

# Aggregate modes: Light & Surface Rail
cb <- mutate(cb, main_mode = ifelse(main_mode == 7, 6, main_mode))

# Renaming
cb <- cb %>%
  rename(p = trip_purpose,
         tp = start_time,
         m = main_mode,
         tfn_at = ntem_at,
         ntem_tt = ntem_traveller_type)

# Calculate mode/time split
tpm_long <- cb %>%
  select(p, ntem_tt, tfn_at, tp, m, trip_weights) %>%
  group_by(p, ntem_tt, tfn_at, tp, m) %>%
  summarise(total_trips = sum(trip_weights, na.rm = TRUE)) %>%
  group_by(p, ntem_tt, tfn_at) %>%
  mutate(split = total_trips/sum(total_trips),
         count = n()) %>% 
  ungroup()

# Add area type 2 back in
tpm_long <- tpm_long %>%
  select(p, tfn_at, ntem_tt, tp, m, total_trips, split) %>%
  arrange(p, tfn_at, ntem_tt, tp, m)

# Fill in missing tp's and m's for available combination
tpm_long <- tpm_long %>%
  complete(nesting(p, tfn_at, ntem_tt),
           tp = 1:6,
           m = c(1:3, 5, 6),
           fill = list(total_trips = 0,
                       split = 0))

# Calculate averages by trip purpose and tfn area type
tpm_infill <- tpm_long %>%
  group_by(p, tfn_at, tp, m) %>%
  summarise(total_trips = sum(total_trips)) %>% 
  group_by(p, tfn_at) %>%
  mutate(split = total_trips/sum(total_trips)) %>% 
  ungroup() %>%
  select(-total_trips)

# Fill in any missing zeros with averages and add ntem traveller type back in
tpm_infill <- tpm_infill %>%
  complete(p = 1:8, tfn_at = 1:8, tp = 1:6, m = c(1:3, 5, 6), fill = list(split = 0)) %>%
  mutate(ntem_tt = 1) %>%
  complete(nesting(p, tfn_at, tp, m), ntem_tt = 1:88) %>%
  fill(split) %>%
  select(p, tfn_at, ntem_tt, tp, m, split) %>%
  arrange(p, tfn_at, ntem_tt, tp, m)

# Find which rows are missing
cur_combos <- tpm_long %>%
  select(p, tfn_at, ntem_tt) %>%
  distinct()

all_combos <- expand_grid(p = 1:8,
                          tfn_at = 1:8,
                          ntem_tt = 1:88)

missing_combos <- setdiff(all_combos, cur_combos)

tpm_infill <- tpm_infill %>%
  inner_join(missing_combos)

tpm_long <- tpm_long %>%
  bind_rows(tpm_infill) %>%
  select(-total_trips)

tpm_long <- tpm_long %>%
  mutate(tp = str_c("tp", tp),
         m = str_c("m", m))

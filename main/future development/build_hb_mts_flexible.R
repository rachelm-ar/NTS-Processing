drive = "C"
tfn_or_ntem = "tfn"
seg_max = 300
segments = list(c("p", "area_type", "ntem_tt"),
                c("p", "area_type", "hh_type"),
                c("p", "hh_type"))
final_segments = c("p", "area_type", "ntem_tt", "tp", "m")

library_list <- c("dplyr",
                  "stringr",
                  "readr",
                  "tidyr",
                  "purrr",
                  "tibble")

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

# Infill zero for all combinations
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

count_levels <- function(seg_levels, n, df){
  
  df %>%
    group_by(across(all_of(seg_levels))) %>%
    mutate("count_seg{n}" := sum(trips)) %>%
    ungroup() %>%
    select(str_c("count_seg", n))
  
}

counts <- map2(segments, seq_along(segments), count_levels, agg_all)
counts <- bind_cols(agg_all, counts)


seg_infill <- function(seg_levels, df){
  
  updated_seg <- c(seg_levels, "tp", "m")
  
  df_infill <- df %>% 
    group_by(across(all_of(updated_seg))) %>%
    summarise(trips = sum(trips)) %>%
    ungroup()
  
  df_infill <- df_infill %>%
    group_by(across(all_of(seg_levels))) %>% 
    mutate(split = trips/sum(trips)) %>%
    ungroup()
    
  infill_colnames <- colnames(df_infill)  
  ntem_tt_detect <- any(str_detect(infill_colnames, "ntem_tt"))
  
  if(!ntem_tt_detect){
    
    df_infill <- df_infill %>%
      left_join(ntem_to_hh_type_lu)
    
  }
  
  updated_colnames <- colnames(df_infill)  
  
  add_columns <- final_segments[!final_segments %in% updated_colnames]
  
  add_columns = c(add_columns)
  
  add_columns <- map(seq_along(add_columns), function(n) tibble("{add_columns[n]}" := rep(1, nrow(df_infill))))
  add_columns <- bind_cols(add_columns)
  
  df_infill <- bind_cols(df_infill, add_columns)
  
  df_infill %>%
    complete(p = 1:8,
             ntem_tt = 1:88,
             area_type = ats_list,
             tp = 1:6,
             m = modes_list) %>% 
    filter(p ==1, ntem_tt == 1, tp ==1, m == 1)
  
  
}

level_trips <- map(segments, seg_infill, agg_all)

segments

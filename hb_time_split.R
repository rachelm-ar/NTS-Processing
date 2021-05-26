cb_version = "tfn"
drive = "C"
weekday = TRUE
week = TRUE

extract_hb_ts <- function(cb_version,
                          drive, 
                          user, 
                          weekday, 
                          week){
  
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr")
  
  library_c(library_list)
  
 # Directories -------------------------------------------------------------
  
  y_dir <- "Y:/NTS/"
  c_dir <- str_c("C:/Users/", user, "/Documents/NTS_C/")
  nts_dir <- ifelse(drive == "Y", y_dir, c_dir)
  
  cb_dir <- str_c(nts_dir, "classified builds/cb_", cb_version, ".csv")
  
  export_dir <- str_c(nts_dir, "outputs/hb/hb_time_split/")
  dir.create(export_dir, showWarnings = FALSE)
  
  # Read classified_build
  cb <- read_csv(cb_dir)
  
 # Pre Processing ---------------------------------------------------------
  
  # Why redefinition of tfn area type?
  cb <- cb %>% 
    filter(trip_purpose %in% 1:8) %>% 
    mutate(tfn_area_type = ifelse(tfn_area_type == 2, 1 , tfn_area_type),
           gender = ifelse(age_work_status == 1, 2, gender))
  
  # Add this to classified build!
  cb <- cb %>%
    lu_traveller_type()
  
  # Add weights
  cb <- mutate(cb, trip_weights = W1 * W2 * W5 * sw_weight)
  
  cb %>%
    select(trip_purpose, TripPurpTo_B01ID, TripPurpFrom_B01ID, TripPurpose_B01ID)
  
 # Time Split - Week -------------------------------------------------------
  
  if(week){
    
    # Derive factors
    ts_week <- cb %>% 
      filter(!is.na(start_time)) %>% 
      select(hb_purpose, traveller_type, tfn_area_type, start_time, trip_weights) %>% 
      group_by(hb_purpose, traveller_type, tfn_area_type, start_time) %>%
      summarise(total_trips = sum(trip_weights, na.rm = TRUE)) %>% 
      group_by(hb_purpose, traveller_type, tfn_area_type) %>% 
      mutate(time_split = total_trips/sum(total_trips)) %>% 
      arrange(hb_purpose, traveller_type, tfn_area_type, start_time) %>% 
      ungroup()
    
    # Reformat
    ts_week <- ts_week %>%
      rename(p = hb_purpose,
             area_type = tfn_area_type) %>% 
      select(p, traveller_type, area_type, start_time, time_split) %>%
      mutate_at(.vars = vars("p", "traveller_type", "area_type", "start_time"),
                .funs = as.integer)
    
    # Fill in missing and transform to wide
    ts_week_wide <- ts_week %>%
      complete(p = 1:8, traveller_type = 1:88, start_time = 1:6, area_type = 1:8, fill = list(time_split = 0)) %>% 
      pivot_wider(names_from = start_time, values_from = time_split, names_prefix = "tp") %>% 
      arrange(p, traveller_type, area_type) %>%
      select(p, traveller_type, area_type, tp1, tp2, tp3, tp4, tp5, tp6)
    
    write_csv(ts_week_wide, str_c(export_dir, "hb_time_split_week.csv"))
    
  }
  
 # Time Split - Weekday ----------------------------------------------------
  
  if(weekday){
    
    # Derive factors
    ts_weekday <- cb %>% 
      filter(!is.na(start_time),
             start_time %in% 1:4) %>% 
      select(hb_purpose, traveller_type, tfn_area_type, start_time, trip_weights) %>% 
      group_by(hb_purpose, traveller_type, tfn_area_type, start_time) %>%
      summarise(total_trips = sum(trip_weights, na.rm = TRUE)) %>% 
      group_by(hb_purpose, traveller_type, tfn_area_type) %>% 
      mutate(time_split = total_trips/sum(total_trips)) %>% 
      arrange(hb_purpose, traveller_type, tfn_area_type, start_time) %>% 
      ungroup()
    
    # Reformat
    ts_weekday <- ts_weekday %>%
      rename(p = hb_purpose,
             area_type = tfn_area_type) %>% 
      select(p, traveller_type, area_type, start_time, time_split) %>%
      mutate_at(.vars = vars("p", "traveller_type", "area_type", "start_time"),
                .funs = as.integer)
    
    # Fill in missing and transform to wide
    ts_weekday_wide <- ts_weekday %>%
      complete(p = 1:8, traveller_type = 1:88, start_time = 1:4, area_type = 1:8, fill = list(time_split = 0)) %>% 
      pivot_wider(names_from = start_time, values_from = time_split, names_prefix = "tp") %>% 
      arrange(p, traveller_type, area_type) %>%
      select(p, traveller_type, area_type, tp1, tp2, tp3, tp4)
    
    write_csv(ts_weekday_wide, str_c(export_dir, "hb_time_split_weekday.csv")) 
    
  }
  
}

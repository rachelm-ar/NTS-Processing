extract_hb_ms <- function(drive, user, weekday, week){
  
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr")
  
  library_c(library_list)
  
 # Directories -------------------------------------------------------------
  
  y_dir <- "Y:/NTS/"
  c_dir <- str_c("C:/Users/", user, "/Documents/NTS_C/")
  nts_dir <- ifelse(drive == "Y", y_dir, c_dir)
  
  import_dir <- str_c(nts_dir, "import/")
  cb_dir <- str_c(import_dir, "classified builds/")
  
  export_dir <- str_c(nts_dir, "outputs/hb/hb_mode_split/")
  dir.create(export_dir, showWarnings = FALSE)
  
  # Read classified_build
  cb <- read_csv(str_c(cb_dir, "classified_build.csv"))
  
 # Pre Processing ---------------------------------------------------------
  
  # Car availability instead of household attribute
  cb <- cb %>% 
    filter(trip_purpose %in% 1:8) %>% 
    mutate(tfn_area_type = ifelse(tfn_area_type == 2, 1 , tfn_area_type),
           gender = ifelse(age_work_status == 1, 2, gender),
           ca = ifelse(cars == 1, 0 ,1))
  
  # Add this to classified build!
  cb <- cb %>%
    lu_traveller_type()
  
  # Add weights
  cb <- mutate(cb, trip_weights = W1 * W2 * W5 * sw_weight)
  

 # Mode Split - Week -------------------------------------------------------
  
  if(week){
    
    ms_week <- cb %>% 
      select(hb_purpose, traveller_type, tfn_area_type, main_mode, trip_weights) %>% 
      group_by(hb_purpose, traveller_type, tfn_area_type, main_mode) %>%
      summarise(total_trips = sum(trip_weights, na.rm = TRUE)) %>% 
      group_by(hb_purpose, traveller_type, tfn_area_type) %>% 
      mutate(mode_split = total_trips/sum(total_trips)) %>% 
      arrange(hb_purpose, traveller_type, tfn_area_type, mode_split) %>% 
      ungroup()
    
    ms_week <- ms_week %>%
      rename(p = hb_purpose,
             area_type = tfn_area_type) %>% 
      select(p, traveller_type, area_type, main_mode, mode_split) %>%
      mutate_at(.vars = vars("p", "traveller_type", "area_type", "main_mode"),
                .funs = as.integer)
    
    # Fill in missing and transform to wide
    ms_week_wide <- ms_week %>%
      complete(p = 1:8, traveller_type = 1:88, main_mode = c(1,2,3,5,6), area_type = 1:8, fill = list(mode_split = 0)) %>% 
      pivot_wider(names_from = main_mode, values_from = mode_split, names_prefix = "m") %>% 
      arrange(p, traveller_type, area_type) %>%
      select(p, traveller_type, area_type, m1, m2, m3, m5, m6)
    
    write_csv(ms_week_wide, str_c(export_dir, "hb_mode_split_week.csv"))
    
  }
  
 # Mode Split - Weekdays -------------------------------------------------------
  
  if(weekday){
    
    ms_weekday <- cb %>% 
      filter(start_time %in% 1:4) %>% 
      select(hb_purpose, traveller_type, tfn_area_type, main_mode, trip_weights) %>% 
      group_by(hb_purpose, traveller_type, tfn_area_type, main_mode) %>%
      summarise(total_trips = sum(trip_weights, na.rm = TRUE)) %>% 
      group_by(hb_purpose, traveller_type, tfn_area_type) %>% 
      mutate(mode_split = total_trips/sum(total_trips)) %>% 
      arrange(hb_purpose, traveller_type, tfn_area_type, mode_split) %>% 
      ungroup()
    
    ms_weekday <- ms_weekday %>%
      rename(p = hb_purpose,
             area_type = tfn_area_type) %>% 
      select(p, traveller_type, area_type, main_mode, mode_split) %>%
      mutate_at(.vars = vars("p", "traveller_type", "area_type", "main_mode"),
                .funs = as.integer)
    
    # Fill in missing and transform to wide
    ms_weekday_wide <- ms_weekday %>%
      complete(p = 1:8, traveller_type = 1:88, main_mode = c(1,2,3,5,6), area_type = 1:8, fill = list(mode_split = 0)) %>% 
      pivot_wider(names_from = main_mode, values_from = mode_split, names_prefix = "m") %>% 
      arrange(p, traveller_type, area_type) %>%
      select(p, traveller_type, area_type, m1, m2, m3, m5, m6)
    
    write_csv(ms_weekday_wide, str_c(export_dir, "hb_mode_split_weekday.csv")) 
    
  }
  
}
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
  
  cb_dir <- str_c(nts_dir, "classified builds/", cb_name, ".csv")
  
  export_dir <- str_c(nts_dir, "outputs/hb/")
  dir.create(export_dir, showWarnings = FALSE)
  
  # Read classified_build
  cb <- read_csv(cb_dir)
  
  # Pre Processing ---------------------------------------------------------
  
  # Removing london Underground?
  # Why redefinition of tfn area type?
  # Children Genderless
  cb <- cb %>% 
    filter(trip_purpose %in% 1:8) %>% 
    mutate(tfn_area_type = ifelse(tfn_area_type == 2, 1 , tfn_area_type),
           gender = ifelse(age_work_status == 1, 2, gender))
  
  # Add this to classified build!
  cb <- cb %>%
    lu_traveller_type()
  
  # Add weights
  cb <- mutate(cb, trip_weights = W1 * W2 * W5 * sw_weight)
  
  # Renaming
  cb <- rename(cb, tp = start_time, m = main_mode)
  
  # Time Split - Week -------------------------------------------------------
  tpm_split <- cb %>% 
    filter(!is.na(tp)) %>% 
    select(hb_purpose, traveller_type, tfn_area_type, tp, m, trip_weights) %>% 
    group_by(hb_purpose, traveller_type, tfn_area_type, tp, m) %>%
    summarise(total_trips = sum(trip_weights, na.rm = TRUE)) %>% 
    group_by(hb_purpose, traveller_type, tfn_area_type) %>%
    mutate(tpm_split = total_trips/sum(total_trips)) %>% 
    arrange(hb_purpose, traveller_type, tfn_area_type, tp, m) %>%
    select(-total_trips) %>%
    ungroup()
  
  tpm_split <- tpm_split %>%
    rename(p = hb_purpose,
           area_type = tfn_area_type) %>% 
    complete(p = 1:8, traveller_type = 1:88,  area_type = 1:8, tp = 1:6, m = c(1,2,3,5,6), fill = list(tpm_split = 0)) %>% 
    mutate(tp = str_c("tp", tp),
           m = str_c("m", m)) %>%
    unite(tpm, tp, m, sep = "_") %>% 
    pivot_wider(names_from = tpm, values_from = tpm_split)
  
  write_csv(tpm_split, str_c(export_dir, "hb_time_mode_split.csv")) 
  
}

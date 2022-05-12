# Main function ----------------------------------------------------------------
build_tld_and_co <- function(input_csv){
  # Load packages --------------------------------------------------------------
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr",
                    "tidyverse",
                    "scales")
  
  library_c(library_list)

  # read input csv
  input_csv <- read_csv(input_csv)
  input_csv <- transpose_input_csv(input_csv)
  
  dir.create(str_c(input_csv$tld_save_dir, "\\reports\\"), showWarnings = FALSE, recursive = TRUE)
  
  # output tld name
  tld_output_dir <- str_c(input_csv$tld_save_dir, "\\",
                         input_csv$tld_name, "_",
                         input_csv$tld_version, ".csv")
  

  # read Classified build
  cb <- read_csv(input_csv$cb_csv_dir)

  # prepare purpose for tld
  cb <- escort_trips(cb, option = 1) # embed escort to main purpose
  cb <- data_filter(cb, hb_only = FALSE, remove_van = FALSE, remove_air = TRUE,
                    aggregate_rail = TRUE) #filter data
  cb <- cb %>%
    mutate(p = ifelse(trip_direction == "hb_to", -p, p)) #change to_home back to its purpose
  
  # Build TLD ------------------------------------------------------------------
  # GOR: 1-NE, 2-NW, 3-YH, 4-EM, 5-WM, 6-EAST, 7-LON, 8-SE, 9-SW, 10-WALES, 11-SCOTLAND
  nts <- cb %>%
    group_by(main_mode, p, tfn_tt, tfn_at, soc, ns, hh_type, gender, aws, start_time, TripOrigGOR_B02ID, TripDestGOR_B02ID,
             HHoldOSLAUA_B01ID, TripOrigUA2009_B01ID, TripDestUA2009_B01ID,
             trip_direction, occupant, TripDisIncSW) %>%
    summarise(trips = sum(weighted_trips)) %>%
    mutate(trip_mile = TripDisIncSW * trips) %>%
    #mutate(is_north = ifelse(TripOrigGOR_B02ID %in% c(1,2,3,5) & TripDestGOR_B02ID %in% c(1,2,3,5),'tfn','ext')) %>%
    filter(!p %in% c(0,10))
  
  write_csv(nts,tld_output_dir)

}
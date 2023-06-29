# audit nts
define_nts_audit_params <- function(nts_dat){

  # Include more variable to check. i.e. trip purpose, age_work_status
  
  nts_dat %>%
    count(SurveyYear)
  
}

# deal with escort trip
escort_trips <- function(cb, option = 1) {
  # set purpose [p] for hb_trip_rate, mts, etc.calculation
  # option 1: embed escort trip to main purpose, p = trip_purpose - 10
  # option 2: embed escort trips to personal business, p = 5
  # option 3: set escort trips to zero, p = 0
  if(option == 1) {
    cb <- cb %>%
      mutate(p = ifelse(trip_purpose > 10, trip_purpose - 10, trip_purpose)) 
  } else if(option == 2) {
    cb <- cb %>%
      mutate(p = ifelse(trip_purpose > 10, 5, trip_purpose))
  } else {
    cb <- cb %>%
      mutate(p = ifelse(trip_purpose > 10, 0, trip_purpose))
  }
  
  # set [hb_to] to negative to be excluded from hb_trip_rate calculation
  # set nhb = 10 + hb
  cb <- cb %>%
    mutate(p = ifelse(trip_direction %in% c("hb_fr"), p, 
                      ifelse(trip_direction %in% c("hb_to"), -p, 10 + p)))
  
  # return output
  return(cb)

}

# pre-processing
cb_preprocess <- function(ub, cb_version){
  # postcodes - checked OK, e.g. AL4 9Y -> AL49, M 16 8 -> M1689
  # cb <- ub %>% 
  #   mutate(PSUPSect = str_replace(PSUPSect, "^(.*\\s.).*", "\\1"),
  #          PSUPSect = str_replace_all(PSUPSect, " ", ""))
  
  # infill W2 with zero for blank records
  cb <- ub %>%
    mutate(W2 = as.double(str_replace(W2, " ","0"))) %>%
    mutate(W5xHH = as.double(str_replace(W5xHH, " ","0"))) %>%
    mutate(W2 = ifelse(is.na(W2),0,W2)) %>%
    mutate(W5xHH = ifelse(is.na(W5xHH),0,W5xHH))

  # create ns from NSSec_B03ID data, set ns (-9,6) to 5
  # could've created a lookup table to do this
  cb <- cb %>% 
    mutate(ns = NSSec_B03ID) %>%
    mutate(ns = ifelse(ns == -9, 6, ns)) %>% 
    group_by(HouseholdID) %>%
    mutate(ns = min(ns)) %>%
    ungroup() %>%
    mutate(ns = ifelse(ns == 6, 5, ns))

  # address incorrectly reported purpose
  cb <- cb %>%
    arrange(SurveyYear, HouseholdID, IndividualID, DayID, TripID) %>%
    mutate(IndiNext = lead(IndividualID, n=1, last(IndividualID))) %>%
    mutate(TripNext = lead(TripID, n=1, last(TripID))) %>%
    mutate(PurpFrom = lead(TripPurpFrom_B01ID, n=1, last(TripPurpTo_B01ID))) %>%
    mutate(TripPurpTo_B01ID = ifelse(IndividualID==IndiNext & TripID+1==TripNext,
                                     PurpFrom, TripPurpTo_B01ID)) %>%
    select(!c("IndiNext","TripNext","PurpFrom"))
    
  # allocate records to trip_group
  cb <- cb %>%
    arrange(IndividualID, TripID) %>%
    group_by(IndividualID) %>%
    mutate(flag_fr = ifelse(TripPurpFrom_B01ID == 23, 1, 0)) %>%
    mutate(trip_group = cumsum(flag_fr)) %>%
    ungroup() %>%
    select(!c("flag_fr"))
  
  # return output
  return(cb)

}

# Main function ----------------------------------------------------------------
build_cb <- function(input_csv){
  
  # Library --------------------------------------------------------------------
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr",
                    "purrr",
                    "parallel")
  
  library_c(library_list)
  
  # Read input csv -------------------------------------------------------------
  input_csv <- read_csv(input_csv)
  input_csv <- transpose_input_csv(input_csv)
  
  # raw nts columns to be extracted
  ub_columns <- read_csv(input_csv$ub_columns_csv_dir)
  
  
  # classified build write dir
  cb_output_dir <- str_c(input_csv$cb_save_dir,
                         "\\",
                         input_csv$cb_name,
                         ".csv")
  
  # hb trip rates build write dir
  hb_trip_rates_output_dir <- str_c(input_csv$hb_trip_rates_save_dir,
                                    "\\",
                                    input_csv$hb_trip_rates_build_name,
                                    ".csv")
  
  # hb trip rates response weights write dir
  hb_trip_rates_weights_output_dir <- str_c(input_csv$hb_trip_rates_save_dir,
                                            "\\",
                                            input_csv$hb_trip_rates_response_weights_name,
                                            ".csv")
  
  # Unclassified build ---------------------------------------------------------
  # table paths
  variable_list <- colnames(ub_columns)
  variable_path <- str_replace(variable_list, "cols", "special_2002-2021_protect.tab")
  variable_path <- str_c(input_csv$nts_raw_dir, variable_path, sep = "\\")
  
  # convert ub columns df to list
  extraction_variables <- map(variable_list, pull, .data = ub_columns)
  extraction_variables <- map(extraction_variables, na.omit)
  extraction_variables <- map(extraction_variables, unique)
  
  # read tables
  ub_tables <- map(variable_path, read_delim, delim = "\t", 
                   num_threads = detectCores()-2) #set number of threads to speed up reading
  
  # select only variables for each table specified in ub_columns.csv
  ub_tables <- map2(ub_tables, extraction_variables, ~ .x %>% select(all_of(.y)))
  
  ub <- reduce(ub_tables, left_join) %>% #added filter to remove NA records
    filter(!is.na(TripID))
  
  # Audit 1 - raw NTS data
  nts_audit <- define_nts_audit_params(ub)
  
  # Classified build -----------------------------------------------------------
  # all non-lookup pre-processing
  cb <- cb_preprocess(ub, version)
   
  # classify all required variables
  # (NN - updated lookup tables to allow optional filter_na)
  cb <- cb %>% 
    lu_direction() %>% 
    lu_purpose() %>%
    lu_occupant() %>%
    lu_gender() %>%
    lu_aws(filter_na = FALSE) %>%
    lu_hh_type(filter_na = FALSE) %>%
    lu_main_mode(filter_na = FALSE) %>%
    lu_start_time(filter_na = FALSE) %>%
    lu_end_time(filter_na = FALSE) %>%
    lu_sw_weight() %>% 
    lu_is_north() %>% 
    lu_soc(filter_na = FALSE) %>%
    # lu_tfn_at(filter_na = FALSE) %>% 
    lu_tt()

  # embed escort trip [X] to purpose [X]
  cb <- escort_trips(cb, 1) #embed escort to main purpose
  
  # Calculate weighted trips & write cb database -------------------------------
  cb <- mutate(cb, weighted_trips = W5xHH * W2 * JJXSC)

  write_csv(cb, cb_output_dir)

  # Build HB trip rates inputs from CB -----------------------------------------
  # filter required attributes
  # remove Air & Van trips, remove other non-escort and other escort
  # filter for aws, gender, hh_type, soc, ns, and tfn_at
  hb_trip_rates_input <- filter(cb, !main_mode %in% c(4, 8),
                                !is.na(aws), !is.na(gender), !is.na(hh_type),
                                !is.na(soc), !is.na(ns))
  
  # define required variables
  # note: 2011 data not used for trip-rate calculation as no valid Postcode sector
  grouping_vars <- c("IndividualID", "p", "SurveyYear", "aws", "gender",
                     "hh_type", "soc", "ns")
  
  grouping_vars <- colnames(cb)[colnames(cb) %in% grouping_vars]
  
  # weight trips by short walk and calculate weekly trips
  weighted_trips <- hb_trip_rates_input %>%
    filter(p %in% c(1:8), W1 == 1) %>%
    group_by_at(grouping_vars) %>%
    summarise(weekly_trips = sum(JJXSC)) %>%
    ungroup()
  
  # remove purpose from grouping vars
  grouping_vars <- str_subset(grouping_vars, "^p$", negate = TRUE)
    
  # every individual must have an observation for each trip purpose
  hb_trip_rates_out <- weighted_trips %>%
    complete(nesting(!!!dplyr::syms(grouping_vars)),
             p = c(1:8), fill = list(weekly_trips = 0)) %>% 
    ungroup()
    
  write_csv(hb_trip_rates_out, hb_trip_rates_output_dir)
  
  # output response weights
  response_weights <- cb %>% 
    filter(p %in% c(1:8), W1 == 1) %>%
    select(IndividualID, p, SurveyYear, W5xHH, JJXSC, W2) %>% 
    mutate(trips = 1) %>% 
    complete(nesting(IndividualID, SurveyYear),
             p = c(1:8), fill = list(W5xHH = 0, trips = 0, JJXSC = 0, W2 = 0)) %>% 
    group_by(p, SurveyYear) %>% 
    summarise(r_weights = sum(W5xHH*JJXSC*W2)/sum(trips*JJXSC*W2),
              count = sum(trips)) %>% 
    ungroup()
      
    write_csv(response_weights, hb_trip_rates_weights_output_dir)
    
    # cleanse database
    ub_tables <- 0
    ub <- 0
    cb <- 0
    hb_trip_rates_input <- 0
    hb_trip_rates_out <- 0
    weighted_trips <- 0
    gc()

}

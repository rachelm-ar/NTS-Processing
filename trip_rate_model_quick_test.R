#################################
#'
#' HB TRIP RATES MODEL TESTING
#'
#################################

Read_packages <- function(packages_list){
  
  "
  Description
  ----------
  - Install packages if not previously installed
  - Load in packages
  
  Parameters
  ----------
  packages_list:
    A vector of packages required to run the main function
  
  Return
  ----------
  A boolean list of TRUE indicating all packages are loaded
  
  "
  
  # Packages not installed
  packages_new <- packages_list[!(packages_list %in% installed.packages()[,"Package"])]
  
  # Install packages
  if(length(packages_new)) install.packages(packages_new)
  
  # Load packages
  lapply(packages_list, require, character.only = TRUE)
  
}

Process_data <- function(df){
  
  "
  Description
  ----------
  - Remove not available categories for soc and sec
  - Transform categorical variables into factors
  
  Parameters
  ----------
  df:
    Trip rates data frame
    
  Return
  ----------
  transformed_df:
    Trip rates data frame prepared for modelling
  
  "
  
  # Remove not available categories of soc and sec
  filtered_df <- df %>%
    filter(soc_cat != -8)
  #(ns_sec == -9 & age_work_status == "0-16_child")|(ns_sec != -9))
  
  # Transform categorical variables into factors
  # Convert to factors
  transformed_df <- filtered_df %>%
    mutate_at(c("age_work_status",
                "gender",
                "hh_adults",
                "cars",
                "soc_cat",
                "ns_sec",
                "tfn_area_type"), 
              funs(factor)) %>%                                
    select(weekly_trips, trip_weights, trip_purpose, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type) %>%
    mutate(new_weights = replace_na(trip_weights/weekly_trips,1),
           new_weights = ifelse(is.infinite(new_weights), 1, new_weights),
           trip_weights = ifelse(trip_weights == 0, 1, trip_weights)) 
  
}

Convert_travellertypes <- function(tt_unlist, k, new_data, i){
  
  "
      Description
      ----------
      - Converts predicted data into traveller types
      
      Parameters
      ----------
      
      tt_unlist:
        A list of traveller types
        
      k:
        Identify the traveller type
      
      new_data:
        Dataframe with trip rates
        
      i:
        Identify which purpose
        
      Return
      ----------
      
      tt_df:
        A list of 88 dataframes corresponding to 88 traveller_types trip rates
      
      "
  
  x <- tt_unlist
  
  if(x$age_work_status == "0-16_child"){
    
    # TT 1 to 8
    tt_df <- new_data %>%
      filter(str_detect(hh_adults, x$hh_adults),
             str_detect(age_work_status, x$age_work_status),
             str_detect(gender, x$gender),
             cars == x$cars) %>%
      mutate(traveller_type = k,
             purpose = i) %>%
      group_by(purpose, hh_adults, age_work_status, cars, soc_cat, ns_sec, tfn_area_type, traveller_type) %>%
      summarise(tfn_predictions = mean(tfn_predictions, na.rm = TRUE)) %>%
      ungroup() %>%
      select(purpose, traveller_type, tfn_area_type, soc_cat, ns_sec, tfn_predictions)
    
  } else {
    
    # TT not child or 75+
    tt_df <- new_data %>%
      filter(str_detect(hh_adults, x$hh_adults),
             str_detect(age_work_status, x$age_work_status),
             str_detect(gender, x$gender),
             cars == x$cars) %>%
      mutate(traveller_type = k,
             purpose = i) %>%
      select(purpose, traveller_type, tfn_area_type, soc_cat, ns_sec, tfn_predictions)
  }
  
}

Post_processing <- function(trip_rates_df){
  
  "
      Description
      ----------
      - Renames parameters for trip production model
      - Recodes SOC and removes SEC = -9
      - Fixes children SOC
      
      Parameters
      ----------
      
      combs:
        A list of combinations
        
      Return
      ----------
      
      updated_df:
        Data frame post classification aggregation
      
      Results:
        Summary of the combinations performance
      
      "
  
  # Rename and filter
  recoded_trip_rates <- trip_rates_df %>% 
    rename(p = purpose,
           soc = soc_cat,
           ns = ns_sec,
           area_type = tfn_area_type, 
           trip_rate = tfn_predictions) %>%
    filter(ns != -9) %>%
    mutate(soc = case_when(
      soc == -9 ~ 0,
      soc == 1 ~ 1,
      soc == 2 ~ 2,
      soc == 3 ~ 3,
      soc == 99 ~ 99))
  
  # Recode Childrens
  children_sorted <- recoded_trip_rates %>%
    filter(traveller_type %in% c(1,2,3,4,5,6,7,8)) %>%
    mutate(soc = 0) %>%
    group_by(p, traveller_type, ns, area_type) %>%
    summarise(trip_rate = mean(trip_rate)) %>%
    ungroup() %>%
    mutate(soc = 0) %>% 
    select(p, traveller_type, soc, ns, area_type, trip_rate) %>%
    arrange(p, traveller_type, soc, area_type, ns)
  
  # Remove children from trip rates
  children_removed <- recoded_trip_rates %>% filter(!traveller_type %in% c(1,2,3,4,5,6,7,8))
  
  # Add children back with updated recoding
  trip_rates_out <- bind_rows(children_sorted, children_removed)
  
}


hb_trip_rates <- function(hb_csv, tfn_trip_rates_csv, ntem_csv, production_csv, 
                          tfn_vs_ntem_tr = FALSE, soc_sec_weight = FALSE) {
  
  # Install and load packages
  packages_list <- c("MASS",
                     "tidyverse",
                     "sjstats",
                     "pscl",
                     "survey",
                     "combinat",
                     "magrittr",
                     "rlang",
                     "ggpmisc",
                     "ggpubr",
                     "rlist",
                     "gridExtra",
                     "openxlsx",
                     "reshape2")
  
  # Install (if not already) and load libraries
  Read_packages(packages_list)
  
  # Redefine select if masked by MASS
  select <- dplyr::select
  
  # Read in Weekly trip rates
  hb_df <- read_csv(hb_csv)
  
  # Extract variable levels
  variables <- c("age_work_status",
                 "gender", 
                 "hh_adults", 
                 "cars", 
                 "soc_cat", 
                 "ns_sec",
                 "tfn_area_type")
  
  hb_df <- hb_df %>%
    filter(soc_cat != -8) %>%
    mutate_at(variables, .funs = factor, ordered = is.ordered(variables))
  
  # Classifications of each variable
  variable_levels <- hb_df %>%
    select(all_of(variables)) %>%
    sapply(levels)
  
  # Split data by purpose
  purpose_df <- hb_df %>% group_split(trip_purpose)
  
  tfn_trip_rates <- list()
  final_df <- list()
  final_model <- list()
  
  for(i in 1:length(purpose_df)){
    
    # Split data into 75% train and 25% test
    smp_size <- floor(0.75*nrow(purpose_df[[i]]))
    train_ind <<- sample(seq_len(nrow(purpose_df[[i]])), size = smp_size)
    
    # SOC_CAT for commuting and business, NS-SEC for all others
    if(i %in% c(1,2)){
      
      vars <- str_subset(variables, "ns_sec", negate = TRUE)
      
      var_levels <- list_modify(variable_levels, "ns_sec" = NULL)
      
    } else {
      
      vars <- str_subset(variables, "soc_cat", negate = TRUE)
      
      var_levels <- list_modify(variable_levels, "soc_cat" = NULL)
      
    }
    
    # Formula to use depending on whether we want soc_cat or ns_sec
    nbr_formula <- as.formula(paste("trip_rate", paste(vars, collapse = " + "), sep = " ~ "))
    
    final_df[[i]] <- purpose_df[[i]]
    
    final_model[[i]] <- glm.nb(formula = nbr_formula,
                               data = purpose_df[[i]],
                               subset = train_ind)
    
    # Extract new variable levels
    new_levels <- var_levels
    
    # Calculate combinations of all variables
    new_data <- do.call("crossing", new_levels)
    
    # Predict new trip rates
    tfn_predictions <- predict(final_model[[i]], newdata = new_data, type = "response") %>% as.vector()
    
    # Fill in non-differential missing values for SEC (purpose 1 and 2) and SOC (all others)
    if(i %in% c(1:2)){
      
      new_data <- new_data %>%
        mutate(tfn_predictions = tfn_predictions, ns_sec = "99")
      
    } else {
      
      new_data <- new_data %>%
        mutate(tfn_predictions = tfn_predictions, soc_cat = "99")
      
    }
    
    # Convert to factors and unjoin any rows with the same trip rate
    new_data <- new_data %>%
      mutate_at(variables, .funs = factor, ordered = is.ordered(variables))
    
    # Build traveller type list:
    aws <- c(
      rep("0-16_child"    , 8),
      rep("16-74_fte"     , 8),
      rep("16-74_pte"     , 8),
      rep("16-74_stu"     , 8),
      rep("16-74_unm"     , 8),
      rep("75\\+_retired" , 8),
      rep("16-74_fte"     , 8),
      rep("16-74_pte"     , 8),
      rep("16-74_stu"     , 8),
      rep("16-74_unm"     , 8),
      rep("75\\+_retired" , 8)
    )
    
    gndr <- c(
      rep("Male|Female"  , 8),
      rep("Male"         , 40),
      rep("Female"       , 40)
    )
    
    crs <- rep(c("0", "1+", "0", "1", "2+", "0", "1", "2+"), 11)
    
    hha <- rep(c("1","1","2","2","2","3+","3+","3+"), 11)
    
    traveller_type_list <- list(age_work_status = aws,
                                hh_adults = hha,
                                cars = crs,
                                gender = gndr)
    
    traveller_types_unlist <- traveller_type_list %>%
      purrr::transpose() %>%
      map(flatten_chr) %>%
      lapply(as.list)
    
    # Convert Individual and Household characteristics to traveller types
    tt_df <- mapply(Convert_travellertypes, 
                    tt_unlist = traveller_types_unlist, 
                    k = seq_along(traveller_types_unlist), 
                    MoreArgs = list(new_data = new_data, i=i),
                    SIMPLIFY = FALSE)
    
    # Collect all traveller types into a data frame
    tfn_trip_rates[[i]] <- tt_df %>% bind_rows()
    
    print(paste0("Completed Purpose ", i, ": ", Sys.time()))
    
  }
  
  # Collect all purpose trip rates into a data frame
  tfn_trip_rates_result <- tfn_trip_rates %>%
    bind_rows() %>%
    arrange(purpose, traveller_type, tfn_area_type, soc_cat, ns_sec)
  
  trip_rates_out <- Post_processing(tfn_trip_rates_result)
  
  # %>% mutate(soc = as.integer(soc)) - do this if using 99
  trip_rates_out <- trip_rates_out %>% 
    mutate(soc = case_when(
      soc == "99" ~ "none",
      soc == 0 & p %in% c(3:8) ~ "none",
      TRUE ~ as.character(soc)))
  
  trip_rates_out <- trip_rates_out %>%
    mutate(ns = case_when(
      ns == "99" ~ "none",
      TRUE ~ as.character(ns)))
  
  trip_rates_out %>% write_csv('Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates_test.csv')
  
}

# Path for trip rates input from trip_rate_pre_processing.R
hb_csv <- "C:/Users/Pluto/Documents/Trip_rate_testing/trip_rate_model_input_test.csv"

hb_trip_rates(hb_csv = hb_csv)

#hb_trip_rates(hb_csv = hb_csv,
#              tfn_trip_rates_csv = "Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates.csv",
#              ntem_csv = ntem_csv,
#              production_csv = "Y:/NTS/TfN_Trip_Rates/trip_productions_ns.csv",
#              tfn_vs_ntem_tr = FALSE,
#              soc_sec_weight = TRUE)
#

tfn_tr <- read_csv('Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates_test.csv')
ntem_tr <- read_csv('Y:/NorMITs Synthesiser/import/ntem_trip_rates_2016.csv')

tfn_tr <- tfn_tr %>%
  group_by(p, traveller_type, area_type) %>%
  summarise(trip_rate = mean(trip_rate))

ntem_tt <- ntem_tr %>%
  filter(purpose == 8) %>%
  group_by(traveller_type) %>%
  summarise(trip_rate = mean(trip_rate))

tfn_tt <- tfn_tr %>%
  filter(p == 8) %>%
  group_by(traveller_type) %>%
  summarise(trip_rate = mean(trip_rate))

tfn_tt %>%
  left_join(ntem_tt, by = "traveller_type") %>%
  rename(tfn = trip_rate.x, ntem = trip_rate.y) %>%
  mutate(difference = tfn-ntem,
         percent = tfn/ntem * 100) %>%
  View()
  

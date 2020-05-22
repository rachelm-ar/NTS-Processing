#################################
#'
#' HB TRIP RATES MODEL
#'
#################################
#' Script should run via source and output Trip rates csv to Y:/NTS/TfN_Trip_Rates folder
#' 
#' There are two functions which can be run post-model:
#' 
#' 1. tfn_vs_ntem - trigger: tfn_vs_ntem_tr = TRUE
#' 2. soc_sec_compare - trigger: soc_sec_weight = TRUE
#'
#' To run these functions, edit the arguments at the bottom of the script
#'
#' TODO:
#' 1. Weighting for 75+ people does not seem to be working correctly
#' 2. Education trip rates are still down
#' 3. Holiday trip rates are massively over predicting - I suspect the data inputted requires changing (maybe day trips should exclude walking trips)
#' 4. Add Differentiation within year as a variable (SurveyYear)
#' 5. Fix file path system

# Path for trip rates input from trip_rate_pre_processing.R
hb_csv_input <- "Y:/NTS/TfN_Trip_Rates/trip_rate_model_input.csv"

# Path for ntem trip rates
ntem_csv <- "Y:/NorMITs Synthesiser/import/ntem_trip_rates_2016.csv"

Extract_levels <- function(variable_name,df){
  
  "
  Description
  ----------
  Obtain the unique levels of a variable in a given dataframe
  
  Parameters
  ----------
  
  variable_name: 
    Variable to extract factors of
    
  df:
    Input a data-frame variable
  
  Returns
  ----------
  A vector of the variable's levels
  
  "
  
  df %>%
    pull(variable_name) %>%
    as.factor() %>%
    levels()
  
}

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

Variable_status <- function(df, variables, nbr_formula){
  
  "
      Description
      ----------
      - Build an initial model
      - Identify variables that have all significant classifications
      
      Parameters
      ----------
      
      df:
        A data frame pre-filtered for a purpose to build model on
        
      Return
      ----------
      
      var_status:
        A vector dictating if the variable can be skipped or classifications require aggregation
      
      "
  
  # Build an initial NBR model
  int_model <- glm.nb(formula = nbr_formula,
                      data = df,
                      subset = train_ind)
  
  int_model_summ <- summary(int_model)
  
  # Extract coefficients of model 
  int_model_vars <- int_model_summ$coefficients %>% 
    as.data.frame() %>%
    select("Pr(>|z|)") %>%
    tibble::rownames_to_column("Variables") %>%
    rename(p_vals = "Pr(>|z|)")
  
  # Identify if variables have all classifications significant
  var_status <- lapply(variables, function(x){
    
    var_pvals <- int_model_vars %>% 
      filter(str_detect(Variables, x)) %>%
      pull(p_vals)
    
    ifelse(all(var_pvals < 0.01) | all(var_pvals > 0.01), "skip", "configure")
    
  })
  
  var_status <- var_status %>% unlist()
  
  list(var_status, int_model_vars)
  
}

remove_combinations <- function(combs, vars){
  
  "
      Description
      ----------
      - Remove combinations which are of no use
      
      Parameters
      ----------
      
      combs:
        A list of combinations
      
      vars:
        - insig_vars for insignificant variables
        - sig_vars for significant variables
        
      Return
      ----------
      
      combinations:
        A list of combinations containing NULL sublists for removed combinations
      
      "
  
  if(!all(combs %in% vars)){
    
    new_comb <- combs
    
  }
  
}

Build_models <- function(combs, purpose_data, variables, j, nbr_formula){
  
  "
      Description
      ----------
      - Update data frame by aggregating with chosen classifications
      - Build weighted negative binomial regression models
      - Extract a summay of the combinations performance
      
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
  
  # Update the data frame with aggregated data
  updated_df <- purpose_data %>%
    mutate(!!variables[j] := case_when(
      get(variables[j]) %in% combs ~ str_c(combs, collapse = " Join "),
      TRUE ~ as.character(get(variables[j]))
    ))
  
  # Build a Negative-bionomial regression model
  nb_mod <- glm.nb(formula = nbr_formula,
                   data = updated_df,
                   subset = train_ind)
  
  # Extract p-values and obtain number of segments, significant segments and aic
  p_val_summ <- nb_mod %>% summary()
  
  p_val <- p_val_summ$coefficients %>% 
    as.data.frame() %>% 
    select("Pr(>|z|)") %>%
    tibble::rownames_to_column("Variables") %>%
    filter(str_detect(Variables, variables[j]))
  
  p_val_sig <- p_val %>% 
    rename(pvals = "Pr(>|z|)") %>% 
    filter(pvals < 0.05)
  
  sig_number <- nrow(p_val_sig)
  cat_number <- nrow(p_val)
  prop_sig <- sig_number/cat_number
  aic <- AIC(nb_mod)
  
  results <- data.frame(category_number = cat_number,
                        significant_number = sig_number,
                        proportion_significant = prop_sig,
                        aic = aic)
  
  list(updated_df, results)
  
}

Convert_travellertypes <- function(tt_unlist, k, new_data, fte75, pte75, rte75, i){
  
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
        
      fte75:
        75+_fte weights
      
      pte75:
        75+_pte weights
      
      rte75:
        75+_retired weights
        
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
    
  } else if (x$age_work_status == "75\\+_retired|75\\+_pte|75\\+_fte"){
    
    #TT 41-48 and 81-88
    tt_df <- new_data %>%
      filter(str_detect(hh_adults, x$hh_adults),
             str_detect(age_work_status, x$age_work_status),
             str_detect(gender, x$gender),
             cars == x$cars) %>%
      mutate(traveller_type = k,
             purpose = i,
             weights75 = ifelse(age_work_status == "75+_fte", fte75,
                                ifelse(age_work_status == "75+_pte", pte75, rte75))) %>%
      group_by(purpose,traveller_type, hh_adults, gender, cars, soc_cat, ns_sec, tfn_area_type) %>%
      summarise(tfn_predictions = weighted.mean(tfn_predictions, w = weights75, na.rm = TRUE)) %>%
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

#' TODO:
#' 1. X and Y axis scale need to be identical
#' 2. Fit titles in plots
#' 3. There may be no need for overall_comparison function if I can get both purposes comparison and overall to function similarily
purposes_comparison <- function(df){
  
  "
      Description
      ----------
      - Creates a plot comparing TfN trip rates with NTEM segmented by purposes
      
      Parameters
      ----------
      
      df:
        A data frame with both tfn and ntem trip rates
        
      Return
      ----------
      
      purp_plots:
        A plot of a purpose
        
      "
  
  purp_plots <- ggplot(data = df, aes(x = ntem, y = tfn)) +
    geom_point() + 
    geom_smooth(method = "lm", se=FALSE, color="red", formula=y~x) +
    #labs(x = "NTEM trip rate", y = "TfN trip rate", title = paste0("NTEM vs TfN Trip Rates for purpose ", parent.frame()$i[])) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
  
}

overall_comparison <- function(df){
  
  "
      Description
      ----------
      - Creates a plot comparing TfN trip rates with NTEM without segmentation
      
      Parameters
      ----------
      
      df:
        A data frame with both tfn and ntem trip rates
        
      Return
      ----------
      
      purp_plots:
        One plot of ntem vs tfn trip rates
        
      "
  
  ggplot(data = df, aes(x = ntem, y = tfn)) +
    geom_point() + 
    geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
    labs(x = "NTEM trip rate", y = "TfN trip rate", title = "NTEM vs TfN Trip Rates") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
  
}

tfn_vs_ntem <- function(tfn_df, tfn_trip_rates_csv, ntem_csv, post_model = FALSE){
  
  "
      Description
      ----------
      - Compares TfN trip rates to NTEM trip rates with and without purpose segmentation
      
      Parameters
      ----------
      
      tfn_df:
        The final data frame from the hb_trip_rates model 
        
      tfn_csv:
        A path to tfn trip rates if this function is run separetely
        
      ntem_csv:
        Path to NTEM trip rates
      
      post_model = FALSE:
        Trigger to input either a csv or dataframe. Data frame when running with main script. CSV when separetely
        
      Return
      ----------
      
      ntem_vs_tfn.jpg:
        Plot of TfN vs NTEM trip rates without segmentation
        
      ntem_vs_tfn_purposes.jpg:
        Plot of TfN vs NTEM trip rates segmented by purpose
      
      ntem_vs_tfn_purposes:
        CSV comparing TfN vs NTEM trip rates segmented by purposes
        
      ntem_vs_tfn_tt:
        CSV comparing TfN vs NTEM trip rates segmented by traveller type
        
      "
  
  # TfN trip rates
  if(post_model == FALSE){
    
    tfn <- tfn_df
    
  } else if (post_model == TRUE){
    
    tfn <- read_csv(tfn_trip_rates_csv)
    
  }
  
  # NTEM trip rates
  ntem <- read_csv(ntem_csv)
  
  # Replicate tfn trip rates for ntem by grouping soc and sec
  tfn <- tfn %>%
    group_by(p, traveller_type, area_type) %>%
    summarise(trip_rate = mean(trip_rate)) %>%
    mutate(area_type = as.integer(area_type)) %>%
    rename(purpose = p)
  
  # Join trip rates
  trip_rates <- ntem %>% 
    left_join(tfn, by = c("purpose", "traveller_type", "area_type")) %>%
    rename(ntem = trip_rate.x,
           tfn = trip_rate.y)
  
  # Table of NTEM trip rates vs TfN trip rates split by purpose
  purposes_comparison_df <- trip_rates %>%
    group_by(purpose) %>%
    summarise_at(c("ntem","tfn"), mean)
  
  # Table of traveller type NTEM trip rates vs TfN trip rates
  tt_comparison_df <- trip_rates %>%
    group_by(traveller_type) %>%
    summarise_at(c("ntem","tfn"), mean)
  
  # Plot of Ntem trip rates against TfN trip rates with regression line
  overall_plot <- overall_comparison(trip_rates)
  
  # Plot of traveller types Ntem trip rates against TfN trip rates with regression line
  tt_comparison_plot <- overall_comparison(tt_comparison_df)
  
  # Plots of Ntem trip rates against TfN trip rates split by purpose
  purposes_split <- trip_rates %>% group_split(purpose)
  
  purposes_plot <- do.call(grid.arrange, c(lapply(purposes_split, purposes_comparison), nrow=4, ncol=2))
  
  # Save all plots and dataframes
  
  purposes_comparison_df %>% write_csv("Y:/NTS/TfN_Trip_Rates/Plots/ntem_vs_tfn_purposes.csv")
  
  tt_comparison_df %>% write_csv("Y:/NTS/TfN_Trip_Rates/Plots/ntem_vs_tfn_tt.csv")
  
  ggsave(filename = "Y:/NTS/TfN_Trip_Rates/Plots/ntem_vs_tfn.jpg",
         plot = overall_plot)
  
  ggsave(filename = "Y:/NTS/TfN_Trip_Rates/Plots/ntem_vs_tfn_tt.jpg",
         plot = tt_comparison_plot)
  
  ggsave(filename = "Y:/NTS/TfN_Trip_Rates/Plots/ntem_vs_tfn_purposes.jpg",
         plot = purposes_plot)
  
}

soc_sec_compare <- function(tfn_df, tfn_trip_rates_csv, production_csv, post_model = FALSE){
  
  "
      Description
      ----------
      - Compares SOC and SEC by purpose
      - Outputs parameters for NEELUM
      
      Parameters
      ----------
      
      tfn_df:
        The final data frame from the hb_trip_rates model 
        
      tfn_trip_rates_csv:
        A path to tfn trip rates if this function is run separetely
      
      production_csv:
        A path to production output of trips for each segment - This is extracted from production model
        Therefore this is only used post_model to get weighted neelum parameters
      
      post_model = FALSE:
        Trigger to input either a csv or dataframe. Data frame when running with main script. CSV when separetely
        
      Return
      ----------
      
      soc_sec_compare.xlsx:
        Excel workbook with 4 sheets -
          1. NEELUM SOC - parameters to use with neelum
          2. NEELUM SEC - parameters to use with neelum
          3. All SOC - comparison by purpose
          4. All SEC - comparison by purpose
        
      "
  
  # TfN trip rates
  if(post_model == FALSE){
    
    tfn_trip_rates <- tfn_df
    
    mean_type <- "Mean"
    
    # Filter for soc categories of interest and all working people
    neelum_soc <- tfn_trip_rates %>%
      filter(soc %in% c(1,2,3),
             p %in% c(1,2),
             traveller_type %in% c(seq(9,18,1), seq(49,64,1))) %>%
      group_by(p, soc) %>%
      summarise(trip_rate = mean(trip_rate, na.rm = TRUE))
    
    neelum_soc <- dcast(neelum_soc, p ~ soc) %>% as_tibble()
    colnames(neelum_soc) <- c("Purpose", "Soc 1", "Soc 2", "Soc 3")
    neelum_soc <- neelum_soc %>%
      mutate(Purpose = c("Commute", "Business"))
    
    # Filter for ns sec categories of interest and all not working people
    neelum_ns <- tfn_trip_rates %>%
      filter(ns != -9, !(p %in% c(1,2))) %>%
      group_by(ns) %>%
      summarise(trip_rate = mean(trip_rate, na.rm = TRUE)) %>%
      mutate(p = "other") %>%
      select(p, ns, trip_rate)
    
    neelum_ns <- dcast(neelum_ns, p ~ ns) %>% as_tibble()
    colnames(neelum_ns) <- c("Purpose", "NS-SeC 1", "NS-SeC 2", "NS-SeC 3", "NS-SeC 4", "NS-SeC 5")
    
    all_soc <- tfn_trip_rates %>%
      filter(soc %in% c(1,2,3),
             p %in% c(1,2,3,4,5,6,7,7,8),
             traveller_type %in% c(seq(9,24,1), seq(49,64,1))) %>%
      group_by(p, soc) %>%
      summarise(trip_rate = mean(trip_rate, na.rm = TRUE))
    
    all_soc <- dcast(all_soc, p ~ soc) %>% as_tibble()
    colnames(all_soc) <- c("Purpose", "Soc 1", "Soc 2", "Soc 3")
    all_soc <- all_soc %>%
      mutate(Purpose = c("Commute", "Business"))
    
    all_ns <- tfn_trip_rates %>%
      filter(ns != -9) %>%
      group_by(p, ns) %>%
      summarise(trip_rate = mean(trip_rate, na.rm = TRUE)) %>%
      filter(!p %in% c(1,2))
    
    all_ns <- dcast(all_ns, p ~ ns) %>% as_tibble()
    colnames(all_ns) <- c("Purpose", "NS-SeC 1", "NS-SeC 2", "NS-SeC 3", "NS-SeC 4", "NS-SeC 5")
    all_ns <- all_ns %>%
      mutate(Purpose = c("Education", "Shopping", "Personal Business", "Social", "Visit Friends", "Holiday and Trips"))
    
  } else if (post_model == TRUE){
    
    # Tfn Trip rates
    tfn_trip_rates <- read_csv(tfn_trip_rates_csv)
    
    mean_type <- "Weighted_Mean"
    
    # Number of trips of each segment from production model
    productions <- read_csv(production_csv)
    
    # Extract total number of trips for each purpose segmented by soc
    soc_total <- productions %>%
      filter(p %in% c(1,2), traveller_type %in% c(seq(9,18,1), seq(49,64,1))) %>%
      group_by(p, soc) %>%
      summarise(total_trips = sum(trips))
    
    # Calculate weights of each segment (For every purpose and soc category, weights should add to 1)
    soc_weights <- productions %>%
      filter(p %in% c(1,2), soc != 0, traveller_type %in% c(seq(9,18,1), seq(49,64,1))) %>%
      left_join(soc_total) %>%
      mutate(weights = trips/total_trips) %>%
      arrange(p, soc)
    
    # Calculate weighted mean
    soc_trip_rates <- tfn_trip_rates %>%
      filter(p %in% c(1,2), soc != 0, traveller_type %in% c(seq(9,18,1), seq(49,64,1))) %>%
      mutate(soc = as.double(soc)) %>%
      arrange(p, soc) %>%
      left_join(soc_weights) %>%
      mutate(weighted_trips = trip_rate * weights) %>%
      group_by(p,soc) %>%
      summarise(mean_soc = sum(weighted_trips)/5)
    
    neelum_soc <- dcast(soc_trip_rates, p ~ soc) %>% as_tibble()
    colnames(neelum_soc) <- c("Purpose", "Soc 1", "Soc 2", "Soc 3")
    neelum_soc <- neelum_soc %>%
      mutate(Purpose = c("Commute", "Business"))
    
    ### The same procedure for NS-SeC but with different filters
    ns_total <- productions %>%
      filter(!p %in% c(1,2)) %>%
      group_by(ns) %>%
      summarise(total_trips = sum(trips))
    
    ns_weights <- productions %>%
      filter(!p %in% c(1,2)) %>%
      left_join(ns_total) %>%
      mutate(weights = trips/total_trips) %>%
      arrange(p, ns)
    
    ns_trip_rates <- tfn_trip_rates %>% 
      filter(!p %in% c(1,2)) %>%
      mutate(soc = ifelse(soc == "none", NA, soc)) %>%
      arrange(p, ns) %>%
      left_join(ns_weights) %>%
      mutate(weighted_trips = trip_rate * weights) %>%
      group_by(ns) %>%
      summarise(mean_ns = sum(weighted_trips)/5) %>%
      mutate(p = "other") %>%
      dplyr::select(p, ns, mean_ns)
    
    neelum_ns <- dcast(ns_trip_rates, p ~ ns) %>% as_tibble()
    colnames(neelum_ns) <- c("Purpose", "NS-SeC 1", "NS-SeC 2", "NS-SeC 3", "NS-SeC 4", "NS-SeC 5")
    
  }
  
  wb <- createWorkbook()
  
  # Non weighted means for all SOC and SEC segments
  if(post_model == FALSE){
    
    addWorksheet(wb, "All_SOC")
    addWorksheet(wb, "All_NS-SEC")
    
    writeData(wb, sheet = "All_SOC", x = all_soc)
    writeData(wb, sheet = "All_NS-SEC", x = all_ns)
    
  }
  
  # Neelum parameters which can be weighted or non weighted depending on if post_model or with model
  addWorksheet(wb, "Neelum_SOC")
  addWorksheet(wb, "Neelum_NS-SEC")

  writeData(wb, sheet = "Neelum_SOC", x = neelum_soc)
  writeData(wb, sheet = "Neelum_NS-SEC", x = neelum_ns)
  
  export_dir <- paste0("Y:/NTS/TfN_Trip_Rates/Plots/Soc_Sec_Compare_", mean_type ,".xlsx")
  
  saveWorkbook(wb, export_dir, overwrite = TRUE)
  
}

stop_quietly <- function() {
  
  "
    Description
      ----------
      - Stops a function quietly without any error message
  
  "
  
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
  
}

hb_trip_rates <- function(hb_csv_input, 
                          tfn_trip_rates_csv, 
                          ntem_csv, 
                          production_csv, 
                          tfn_vs_ntem_tr = FALSE, 
                          soc_sec_weight = FALSE){
  
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
  
  Read_packages(packages_list)
  
  # Redefine select if masked by MASS
  select <- dplyr::select
  
  # Optional Function 1 - tfn_vs_ntem comparison:
  if (tfn_vs_ntem_tr == TRUE){
    
    tfn_vs_ntem(tfn_trip_rates_csv = tfn_trip_rates_csv,
                ntem_csv = ntem_csv,
                post_model = TRUE)
    
    print(paste("Post Model tfn vs ntem comparison complete"))
    
  }
  
  # Optional Function 2 - soc_sec_compare for weighted neelum parameters
  if (soc_sec_weight == TRUE){
    
    soc_sec_compare(tfn_trip_rates_csv = tfn_trip_rates_csv,
                    production_csv = production_csv,
                    post_model = TRUE)
    
    print(paste("Post Model soc and sec comparison complete"))
    
  }
  
  if(any(c(tfn_vs_ntem_tr, soc_sec_weight))){
    
    print(paste("Post Model Functions Complete - Ending Function early"))
    
    stop_quietly()
    
  }
  
  # Read in Weekly trip rates
  hb_df <- read_csv(hb_csv_input)
  
  # Read in ntem csv for comparison between tfn trip rates and ntem
  ntem_csv <- "Y:/NorMITs Synthesiser/import/ntem_trip_rates_2016.csv"
  
  # Filter and transform data
  #hb_df <- Process_data(hb_df)
  
  hb_df <- hb_df %>%
    filter(soc_cat != -8)
  
  # Transform categorical variables into factors
  hb_df <- hb_df %>%
    mutate_at(c("age_work_status",
                "gender",
                "hh_adults",
                "cars",
                "soc_cat",
                "ns_sec",
                "tfn_area_type"), 
              funs(factor))
  
  
  # Extract variable levels
  variables <- c("age_work_status",
                 "gender", 
                 "hh_adults", 
                 "cars", 
                 "soc_cat", 
                 "ns_sec",
                 "tfn_area_type")
  
  variable_levels <- lapply(variables, Extract_levels, hb_df)
  names(variable_levels) <- variables
  
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
    
    # Run an initial model to detect which variables do not require further aggregation
    var_status <- Variable_status(df = purpose_df[[i]], 
                                  variables = vars, 
                                  nbr_formula = nbr_formula)
    
    print(paste0("Initial Model for purpose ", i, " finished: ", Sys.time()))
    
    for (j in 1:length(vars)){
      
      # First initialisation
      if(j == 1){
        
        purpose_data <- purpose_df[[i]]
        
      }
      
      # Gender has only two classifications => skip
      if(vars[[j]] == "gender") {
        
        print(paste0("Skipping Gender for Purpose ", i))
        
        next
        
      }
      
      # If all classifications are significant and not final variable then move to next variable
      if(var_status[[1]][j] == "skip" & j != length(vars)){
        
        print(paste0("Skipping ",vars[j]," for Purpose ", i))
        
        next
        
      }
      
      # If all classifications are insignificant and not final variable then move to next variable
      var_pvals <- var_status[[2]] %>%
        filter(str_detect(Variables, vars[j])) %>%
        pull()
      
      if(all(var_pvals > 0.01) & j != length(vars)){
        
        print(paste0("Skipping ", vars[j], " (", j, ") for purpose ", i, " - ", Sys.time()))
        
        next
        
      }
      
      # Atleast one classification is insignificant and variable status is to configure
      if(any(var_pvals < 0.01) & var_status[[1]][j] == "configure") {
        
        # Find all combinations of classifications
        combinations <- do.call(c, lapply(seq_along(var_levels[[j]]), combn, x = var_levels[[j]], simplify = FALSE))
        
        # Remove combinations where variable levels are by themself
        combinations <- combinations[(length(var_levels[[j]]) + 1): (length(combinations)-1)]
        
        ### Remove any combinations which merge insignificant classifications with insignificant clasifications
        insig_vars <- var_status[[2]] %>%
          filter(str_detect(Variables, vars[j])) %>%
          filter(p_vals > 0.01) %>%
          pull(Variables) %>%
          str_replace(vars[j], "")
        
        insig_vars <- c(insig_vars, var_levels[[j]][1]) # Add baseline classification
        
        combinations <- lapply(combinations, remove_combinations, insig_vars)
        
        ### Remove any combinations which merge significant classifications with significant classifications
        sig_vars <- var_status[[2]] %>%
          filter(str_detect(Variables, vars[j])) %>%
          filter(p_vals < 0.01) %>%#
          pull(Variables) %>%
          str_replace(vars[j], "")
        
        sig_vars <- c(sig_vars, var_levels[[j]][1]) # Add baseline classification
        
        combinations <- lapply(combinations, remove_combinations, sig_vars)
        
        # This sets the first combination to default where no aggregation is taken place
        combinations <- c("",compact(combinations))
        
        # Build models and extract data frames and selection criteria results
        mod_results <- lapply(combinations, Build_models, purpose_data = purpose_data, variables = vars, j = j, nbr_formula = nbr_formula)
        
        # Extract dataframes of aggregated segments
        results_df <- sapply(mod_results, function(x) x[1])
        
        # Extract our selection criteria results
        results_values <- sapply(mod_results, function(x) x[2])
        
        # Obtain the top row based on selection criteria which is most significant variables
        model_select <- results_values %>%
          bind_rows() %>% 
          mutate(n = seq_along(mod_results)) %>%
          select(n, everything()) %>%
          arrange(desc(proportion_significant),desc(significant_number), desc(category_number), aic)
        
        # Obtain winning combination
        combination_winner <- model_select %>% 
          slice(1) %>% 
          pull(n)
        
        # Update purpose_data with winning combination
        purpose_data <- results_df %>% extract2(combination_winner)
        
        if (j == length(vars)){
          
          final_df[[i]] <- purpose_data
          
        }
        
        print(paste0("Completed Purpose ", i, " for variable ", j, ": ", Sys.time()))
        
      } else if (var_status[[1]][j] == "skip" & j == length(vars)){
        
        final_df[[i]] <- purpose_data
        
        print(paste0("Skipping ",vars[j]," for Purpose ", i))
        
      }
      
    }
    
    # Build final model
    final_model[[i]] <- glm.nb(formula = nbr_formula,
                               data = final_df[[i]],
                               subset = train_ind)
    
    # Extract new variable levels
    new_levels <- lapply(vars, Extract_levels, df = final_df[[i]])
    names(new_levels) <- vars
    
    # Calculate combinations of all variables
    new_data <- do.call("crossing",new_levels)
    
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
    
    new_data <- new_data %>%
      mutate_at(c("age_work_status",
                  "gender",
                  "hh_adults",
                  "cars",
                  "soc_cat",
                  "ns_sec",
                  "tfn_area_type"), 
                funs(factor))
    
    new_data <- new_data %>%
      separate_rows("hh_adults", sep = " Join ") %>%
      separate_rows("gender", sep = " Join ") %>%
      separate_rows("age_work_status", sep = " Join ") %>%
      separate_rows("cars", sep = " Join ") %>%
      separate_rows("soc_cat", sep = " Join ") %>%
      separate_rows("ns_sec", sep = " Join ") %>%
      separate_rows("tfn_area_type", sep = " Join ")
    
    # Build traveller type list:
    aws <- c(rep("0-16_child"                  , 8),
             rep("16-74_fte"                   , 8),
             rep("16-74_pte"                   , 8),
             rep("16-74_stu"                   , 8),
             rep("16-74_unm"                   , 8),
             rep("75\\+_retired|75\\+_pte|75\\+_fte" , 8),
             rep("16-74_fte"                   , 8),
             rep("16-74_pte"                   , 8),
             rep("16-74_stu"                   , 8),
             rep("16-74_unm"                   , 8),
             rep("75\\+_retired|75\\+_pte|75\\+_fte" , 8))
    
    gndr <- c(rep("Male|Female"                , 8),
              rep("Male"                       , 40),
              rep("Female"                     , 40))
    
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
    
    # Extracts weights to apply to 75+ to replicate NTEM classifications
    weights75plus <- hb_df %>% 
      filter(str_detect(age_work_status, "75\\+_retired|75\\+_fte|75\\+_pte")) %>%
      group_by(age_work_status) %>% 
      count()
    
    total75 <- weights75plus %>% pull(n) %>% sum()
    fte75 <- weights75plus %>% filter(age_work_status == "75+_fte") %>% pull(n)
    fte75 <- fte75/total75
    pte75 <- weights75plus %>% filter(age_work_status == "75+_pte") %>% pull(n)
    pte75 <- pte75/total75
    rte75 <- weights75plus %>% filter(age_work_status == "75+_retired") %>% pull(n)
    rte75 <- rte75/total75
    
    # Convert Individual and Household characteristics to traveller types
    tt_df <- mapply(Convert_travellertypes, 
                    tt_unlist = traveller_types_unlist, 
                    k = seq_along(traveller_types_unlist), 
                    MoreArgs = list(new_data = new_data, 
                                    fte75 = fte75, 
                                    pte75 = pte75, 
                                    rte75 = rte75, 
                                    i=i),
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
  
  print(paste0("Finished Post Processing"))
  
  trip_rates_out %>% write_csv('Y:/NTS/TfN_Trip_Rates/hb_trip_rates_NBR_none_algo.csv')
  
  print(paste0("Finished saving trip rates csv"))
  
  # Comparison of tfn and ntem
  tfn_vs_ntem(tfn_df = trip_rates_out, ntem_csv = ntem_csv)
  
  print(paste0("Finished tfn vs ntem comparison"))
  
  # Comparison of SOC and SEC
  soc_sec_compare(tfn_df = trip_rates_out)
  
  print(paste0("Finished SOC and SEC comparison"))
  
}

hb_trip_rates(hb_csv_input = hb_csv_input,
              tfn_trip_rates_csv = "",
              ntem_csv = ntem_csv,
              production_csv = "",
              tfn_vs_ntem_tr = FALSE,
              soc_sec_weight = FALSE)

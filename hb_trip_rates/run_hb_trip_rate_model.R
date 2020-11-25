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

library(tidyverse)

# Importing Directories
github_dir <- "C:/Users/Pluto/Documents/GitHub/NTS-Processing/"
nts_dir <- "Y:/NTS/" # NTS folder
import_dir <- str_c(nts_dir, "import/") # Imports folder
output_dir <- str_c(nts_dir, "outputs/hb_trip_rates/") # Outputs folder
lookup_dir <- str_c(nts_dir, "lookups/") # Lookups

# Reading external files
source(str_c(github_dir,"hb_trip_rates/trip_rate_model.r"))
source(str_c(lookup_dir,"lookups.r"))

# Classified_nts_trip_rates
hb_csv <- str_c(import_dir, "classified_nts_trip_rates.csv")

read_packages <- function(packages_list){
  
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
  #lapply(packages_list, require, character.only = TRUE)
  
}

variable_status <- function(df, variables, nbr_formula){
  
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
  int_model <- MASS::glm.nb(formula = nbr_formula,
                      data = df)
  
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

build_models <- function(combs, purpose_data, variables, j, nbr_formula){
  
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
  nb_mod <- MASS::glm.nb(formula = nbr_formula,
                   data = updated_df)
  
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

post_processing <- function(trip_rates_df){
  
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
  
  # Rename columns
  renamed_df <- trip_rates_df %>%
    rename(p = purpose,
           area_type = tfn_area_type,
           soc = soc_cat,
           ns = ns_sec,
           trip_rate = tfn_predictions)
  
  # Purposes 1 and 2 transformations
  purposes1_to_2 <- renamed_df %>%
    filter(p %in% 1:2) %>%
    filter((traveller_type %in% 1:8 & soc == 99) | traveller_type %in% 9:88) %>%
    mutate(soc = ifelse(soc == 99, 0, soc),
           ns = "none")
  
  # Purposes 3 to 8 transformations
  purposes3_to_8 <- renamed_df %>%
    filter(p %in% 3:8, ns != 99) %>%
    mutate(soc = "none")
  
  # Bind together
  df_out <- bind_rows(purposes1_to_2, purposes3_to_8) %>%
    select(p, traveller_type, soc, ns, area_type, trip_rate) %>%
    arrange(p, traveller_type, soc, ns, area_type)
  
  return(df_out)

}

#' TODO:
#' 1. X and Y axis scale need to be identical
#' 2. Fit titles in plots
#' 3. There may be no need for overall_comparison function if I can get both purposes comparison and overall to function similarily
purposes_comparison <- function(df, purp_names){
  
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
  
  axis_max <- df %>%
    select(ntem, tfn) %>% 
    max() %>%
    ceiling()
  
  purposes_names <- c("Commute",
                      "Business",
                      "Education",
                      "Shopping",
                      "Personal Business",
                      "Entertainment",
                      "Visiting Friends",
                      "Holiday/Trips")
  
  purp_plots <- ggplot(data = df, aes(x = ntem, y = tfn)) +
    geom_point() + 
    geom_smooth(method = "lm", se=FALSE, color="red", formula=y~x) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    labs(x = "NTEM", y = "TfN", title = purp_names) +
    stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) + 
    scale_x_continuous(limits = c(0,axis_max)) +
    scale_y_continuous(limits = c(0,axis_max))
  
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
  
  axis_max <- df %>%
    select(ntem, tfn) %>% 
    max() %>%
    ceiling()

  ggplot(data = df, aes(x = ntem, y = tfn)) + 
    geom_point() + 
    geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
    labs(x = "NTEM trip rate", y = "TfN trip rate", title = "TfN vs NTEM Trip Rates") +
    scale_x_continuous(limits = c(0,axis_max)) +
    scale_y_continuous(limits = c(0,axis_max)) +
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
  
  purposes_plot <- do.call(grid.arrange, c(mapply(purposes_comparison, purposes_split, purposes_names, SIMPLIFY = FALSE), nrow=4, ncol=2))
  
  # Save all plots and dataframes
  purposes_comparison_df %>% write_csv("Y:/NTS/outputs/hb_trip_rates/Comparisons/ntem_vs_tfn_purposes.csv")
  
  tt_comparison_df %>% write_csv("Y:/NTS/outputs/hb_trip_rates/ntem_vs_tfn_tt.csv")
  
  ggsave(filename = "Y:/NTS/outputs/hb_trip_rates/Plots/ntem_vs_tfn.jpg",
         plot = overall_plot)
  
  ggsave(filename = "Y:/NTS/outputs/hb_trip_rates/Plots/ntem_vs_tfn_tt.jpg",
         plot = tt_comparison_plot)
  
  ggsave(filename = "Y:/NTS/outputs/hb_trip_rates/Plots/ntem_vs_tfn_purposes.jpg",
         plot = purposes_plot, width = 25, height = 35, units = "cm")
  
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
    # Questions to ask Chris:
    
    #1. "FTE for commute and business only?"
    #2. "no nhb trips for commute and we setle with 0.08 for business?"
    
    
    # Tfn Trip rates
    tfn_trip_rates <- read_csv(tfn_trip_rates_csv)
    
    # Number of trips of each segment from production model
    productions <- read_csv(production_csv)
    
    mean_type <- "Weighted_Mean"
    
    workers_ind <- c(seq(9,24,1), seq(49,64,1))
    fte_ind <- c(seq(9,16,1), seq(49,56,1))
    
    #####
    
    soc_tfn_tr <- tfn_trip_rates %>%
      select(-ns) %>%
      filter(p %in% c(1,2), soc != 0, traveller_type %in% workers_ind) %>%
      mutate(soc = as.double(soc))
    
    soc_productions <- productions %>%
      select(-ns) %>%
      filter(p %in% c(1,2), soc != 0) 
    
    # Non weighted Average
    soc_tfn_tr %>%
      group_by(p, soc) %>%
      summarise(mean(trip_rate)/5)
    
    # Weighted average (without taking into account of fte and pte)
    soc_segment_trips <- soc_productions %>%
      group_by(p, soc) %>%
      summarise(segment_trips = sum(trips))
    
    soc_weights <- soc_productions %>%
      left_join(soc_segment_trips) %>%
      mutate(weight_trips = trips/segment_trips)
    
    soc_tr <- soc_tfn_tr %>%
      left_join(soc_weights) %>% 
      mutate(mean_trips = trip_rate * weight_trips) %>%
      group_by(p, soc) %>%
      summarise(trip_rate = sum(mean_trips)/5)
    
    neelum_soc <- dcast(soc_tr, p ~ soc) %>% as_tibble()
    colnames(neelum_soc) <- c("Purpose", "Soc 1", "Soc 2", "Soc 3")
    neelum_soc <- mutate(neelum_soc, Purpose = c("Commute", "Business"))
    
    ### The same procedure for NS-SeC but with different filters
     
    ns_productions <- productions %>%
      filter(!p %in% c(1,2)) %>%
      select(-soc)
    
    ns_tfn_tr <- tfn_trip_rates %>%
      filter(!p %in% c(1,2)) %>%
      select(-soc)
    
    # Non weighted
    
    ns_tfn_tr %>%
      group_by(p,ns) %>%
      summarise(mean(trip_rate)/5)
    
    ns_tfn_tr %>%
      group_by(p,ns) %>%
      summarise(total_trips = mean(trip_rate)/5)
    
    # Weight by purpose first method:
    ns_total <- ns_productions %>%
      group_by(p, ns) %>%
      summarise(segment_trips = sum(trips))
    
    ns_weights <- ns_productions %>%
      left_join(ns_total) %>%
      mutate(weights = trips/segment_trips)
    
    ns_tfn_tr %>%
      left_join(ns_weights) %>%
      mutate(mean_tr = trip_rate * weights) %>%
      group_by(ns) %>%
      summarise(trip_rate = sum(mean_tr/5))
    
    # Weight just by ns
    ns_total <- ns_productions %>%
      group_by(ns) %>%
      summarise(segment_trips = sum(trips))
    
    ns_weights <- ns_productions %>%
      left_join(ns_total) %>%
      mutate(weights = trips/segment_trips)
    
    ns_tfn_tr %>%
      left_join(ns_weights) %>%
      mutate(mean_tr = trip_rate * weights) %>%
      group_by(ns) %>%
      summarise(trip_rate = sum(mean_tr/5))
    
    ####
    ns_productions <- productions %>%
      filter(!p %in% c(1,2))
    
    ns_total <- ns_productions %>%
      group_by(p) %>%
      summarise(segment_trips = sum(trips))
    
    ns_weights <- ns_productions %>%
      left_join(ns_total) %>%
      mutate(weights = trips/segment_trips)
  
    
    ns_trip_rates <- tfn_trip_rates %>%
      filter(!p %in% c(1,2)) %>%
      mutate(soc = ifelse(soc == "none", NA, soc)) %>%
      left_join(ns_weights) %>%
      mutate(weighted_trips = trip_rate * weights) %>%
      group_by(p) %>%
      summarise(mean_trips = sum(weighted_trips)/5)
    
    neelum_ns <- dcast(ns_trip_rates, . ~ ns) %>% as_tibble()
    colnames(neelum_ns) <- c("Purpose", "NS-SeC 1", "NS-SeC 2", "NS-SeC 3", "NS-SeC 4", "NS-SeC 5")
    neelum_ns <- mutate(neelum_ns, Purpose = "Other")
    
    
    ##########
    
    upd_prod <- productions %>%
      mutate(p = case_when(
        p == 1 ~ "1",
        p == 2 ~ "2",
        p %in% c(3:8) ~ "other"
      ))
    
    # Populations
    upd_prod %>%
      group_by(p) %>%
      summarise(sum(trips))
    
    # Unweighted full time employed commute and business
    tfn_trip_rates %>%
      filter(p %in% c(1,2), traveller_type %in% workers_ind) %>%
      group_by(p) %>%
      summarise(trip_rate = mean(trip_rate)/5)
    
    tfn_trip_rates %>%
      filter(p %in% c(1,2)) %>%
      group_by(p, soc) %>%
      summarise(mean(trip_rate))
    
    tfn_trip_rates %>%
      filter(!p %in% c(1,2)) %>%
      group_by(ns) %>%
      summarise(mean(trip_rate))
    
    soc_trip_rates %>%
      ungroup() %>%
      group_by(p) %>%
      summarise(mean(trip_rate))
    
    
    neelum_soc
    
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

hb_trip_rates <- function(hb_csv, tfn_trip_rates_csv, ntem_csv, production_csv, 
                          tfn_vs_ntem_tr = FALSE, soc_sec_weight = FALSE) {
  
  # Install and load packages
  packages_list <- c("MASS",
                     "tidyverse",
                     "sjstats",
                     "pscl",
                     "combinat",
                     "magrittr",
                     "rlang",
                     "ggpmisc",
                     "ggpubr",
                     "rlist",
                     "gridExtra",
                     "openxlsx",
                     "reshape2")
  
  # Install (if not already)
  read_packages(packages_list)
  
  library(tidyverse)
  library(openxlsx)
  library(rlist)
  library(ggpmisc)
  library(gridExtra)
  
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
    
  } else {
    
    print(paste("No Post Model options selected - Running trip rates function"))
    
  }
  
  # Read in Weekly trip rates
  hb_df <- read_csv(hb_csv)

  # Exploratory variables
  variables <- c("age_work_status",
                 "gender", 
                 "hh_adults", 
                 "cars", 
                 "soc_cat", 
                 "ns_sec",
                 "tfn_area_type")
  
  # Convert explanatory variables to factors
  hb_df <- hb_df %>%
    mutate_at(variables, .funs = factor, ordered = is.ordered(variables))
  
  # Classifications of each variable
  variable_levels <- hb_df %>%
    select(all_of(variables)) %>%
    sapply(levels)

  # Split data by purpose
  purpose_df <- group_split(hb_df, trip_purpose)
  
  tfn_trip_rates <- list()
  final_df <- list()
  final_model <- list()
  
  for(i in 1:length(purpose_df)){
    
    # Add a row indicator
    p_df <- purpose_df[[i]]
    
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
    var_status <- variable_status(df = p_df, 
                                  variables = vars, 
                                  nbr_formula = nbr_formula)
    
    print(paste0("Initial Model for purpose ", i, " finished: ", Sys.time()))
    
    for (j in 1:length(vars)){
      
      # First initialisation
      if(j == 1){
        
        purpose_data <- purpose_df[[i]]
        
      }
      
      # Gender has only two classifications => skip
      if(vars[[j]] %in% c("gender","SurveyYear")) {
        
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
        combinations <- c("", compact(combinations))
        
        # Remove any combinations which combine with 99
        remove99 <- which(sapply(combinations, function(x) any(str_detect(x, '99'))))
        
        if(!is_empty(remove99)){
          
          combinations <- combinations[-remove99]
          
        }
        
        # Build models and extract data frames and selection criteria results
        mod_results <- lapply(combinations, build_models, purpose_data = purpose_data, variables = vars, j = j, nbr_formula = nbr_formula)
        
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
        purpose_data <- results_df %>% magrittr::extract2(combination_winner)
        
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
    final_model[[i]] <- MASS::glm.nb(formula = nbr_formula,
                               data = final_df[[i]])
    
    # Extract new variable levels
    new_levels <- final_df[[i]] %>%
      mutate_at(variables, .funs = factor, ordered = is.ordered(variables)) %>%
      select(all_of(variables)) %>%
      sapply(levels)
    
    # Calculate combinations of all variables
    new_data <- do.call("crossing", new_levels)
    
    if(i %in% 1:2) {
      
      new_data <- new_data %>%
        group_by(age_work_status, gender, hh_adults, cars, soc_cat, tfn_area_type) %>%
        summarise() %>%
        mutate(ns_sec = '99') %>%
        ungroup()
      
      # Predict new trip rates
      tfn_predictions <- predict(final_model[[i]], newdata = new_data, type = "response") %>% as.vector()
      
      new_data <- mutate(new_data, tfn_predictions = tfn_predictions)
      
    } else {
      
      new_data <- new_data %>%
        group_by(age_work_status, gender, hh_adults, cars, ns_sec, tfn_area_type) %>%
        summarise() %>%
        mutate(soc_cat = '99') %>%
        ungroup()
      
      # Predict new trip rates
      tfn_predictions <- predict(final_model[[i]], newdata = new_data, type = "response") %>% as.vector()
      
      new_data <- mutate(new_data, tfn_predictions = tfn_predictions)
      
    }
    
    # Convert to factors and unjoin any rows with the same trip rate
    #new_data <- new_data %>%
    #  mutate_at(variables, .funs = factor, ordered = is.ordered(variables)) %>% # Convert to factors
    #  separate_rows(all_of(variables), sep = " Join ")
    
    new_data <- new_data %>%
      separate_rows(age_work_status, sep = " Join ") %>%
      separate_rows(hh_adults, sep = " Join ") %>%
      separate_rows(cars, sep = " Join ") %>%
      separate_rows(tfn_area_type, sep = " Join ") %>%
      separate_rows(soc_cat, sep = " Join ") %>%
      separate_rows(ns_sec, sep = " Join ")
      
    # Add traveller types by combining underlying variables and joining a lookup
    new_data <- new_data %>%
      unite("traveller_type_char", "age_work_status", "gender", "hh_adults", "cars", remove=FALSE, sep="_") %>%
      lu_traveller_type() %>%
      na.omit()
    
    # Keep only the combinations in NTEM traveller types
    tfn_trip_rates[[i]] <- new_data %>%
      mutate(purpose = i) %>%
      select(purpose, traveller_type, tfn_area_type, soc_cat, ns_sec, tfn_predictions)
    
    print(paste0("Completed Purpose ", i, ": ", Sys.time()))
    
  }
  
  # Collect all purpose trip rates into a data frame
  tfn_trip_rates_result <- tfn_trip_rates %>%
    bind_rows() %>%
    arrange(purpose, traveller_type, tfn_area_type, soc_cat, ns_sec)
  
  trip_rates_out <- post_processing(tfn_trip_rates_result)
  
  print(paste0("Finished Post Processing"))
  
  trip_rates_out %>% write_csv("Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates_HA.csv")
  
  #print(paste0("Finished saving trip rates csv"))
  
  # Comparison of tfn and ntem
  #tfn_vs_ntem(tfn_df = trip_rates_out, ntem_csv = ntem_csv_input)
  
  #print(paste0("Finished tfn vs ntem comparison"))
  
  # Comparison of SOC and SEC
 #soc_sec_compare(tfn_df = trip_rates_out)
  
  #print(paste0("Finished SOC and SEC comparison"))
  
}

# Triggers for post model processing
tfn_vs_ntem_tr = FALSE
soc_sec_weight = FALSE

#hb_trip_rates(hb_csv = hb_csv)

## Path for ntem trip rates
ntem_csv <- "Y:/NorMITs Synthesiser/import/ntem_trip_rates_2016.csv"
#
## Path to tfn_trip_rates csv for post model procesing
#tfn_trip_rates_csv = "Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates.csv"
#
## Path to production csv from production model
#production_csv = "Y:/NTS/TfN_Trip_Rates/trip_productions_tp.csv"



hb_trip_rates(hb_csv = hb_csv,
              tfn_trip_rates_csv = "Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates.csv",
              ntem_csv = ntem_csv,
              production_csv = "Y:/NTS/TfN_Trip_Rates/trip_productions_ns.csv",
              tfn_vs_ntem_tr = TRUE,
              soc_sec_weight = TRUE)



# Model type based on form specified in model_forms.csv
build_model <- function(mod_form, mod_formula, hb_df){
  
  if(mod_form == "nb"){
    
    MASS::glm.nb(formula = as.formula(mod_formula),
                 data = hb_df)
    
  } else if(mod_form == "zip"){
    
    pscl::zeroinfl(formula = as.formula(mod_formula),
                   data = hb_df,
                   dist = "poisson")
    
  } else if(mod_form == "zinb"){
    
    pscl::zeroinfl(formula = as.formula(mod_formula),
                   data = hb_df,
                   dist = "negbin")
    
  }
  
  
}

extract_levels <- function(hb_model, mod_form){
  
  if(mod_form == "nb"){
    new_levels <- hb_model$xlevels
    
    
  } else {
    
    new_levels <- hb_model$levels
    
  }
  
  new_levels %>% 
    cross() %>% 
    bind_rows() %>% 
    mutate_all(factor)
  
}

add_predictions <- function(new_data, hb_model){
  
  predictions <-  predict(hb_model, new_data, type = "response")
  
  new_data %>%
    mutate(trips = predictions)
  
}

process_newdata <- function(new_data, aws, tfn_or_ntem){
  
  # Add SOC & gender for children
  # Add SOC for non working
  if(aws == 1){
    
    new_data <- mutate(new_data, gender = factor(1))
    
    if(tfn_or_ntem == "tfn"){
      
      new_data <- mutate(new_data, soc = factor(2))
      
    }
    
  } else if(aws %in% 4:6 & tfn_or_ntem == "tfn"){
    
    new_data <- mutate(new_data, soc = factor(2))
    
  }
  
  new_data
  
}

infil_p_aws <- function(data, purpose, age_work_status){
  
  data %>% 
    mutate(p = purpose,
           aws = age_work_status)
  
}

c_weighted_rates <- function(data, response_weights, tfn_or_ntem){
  
  if(tfn_or_ntem == "tfn"){
    
    year_grouping <- c("p", "aws", "gender", "hh_type", "soc", "ns", "tfn_at")
    
  } else if (tfn_or_ntem == "ntem"){
    
    year_grouping <- c("p", "aws", "gender", "hh_type", "ntem_at")
    
  }
  
  response_weights <- response_weights %>% 
    mutate(SurveyYear = factor(SurveyYear))
  
  data %>%
    left_join(response_weights, by = c("p", "SurveyYear")) %>% 
    group_by(!!!syms(year_grouping)) %>% 
    mutate(prop = trips * r_weights * (count/sum(count))) %>% 
    summarise(trips = sum(prop)) %>% 
    ungroup()
  
}

build_hb_trip_rates <- function(user, drive, tfn_or_ntem){

# Load packages -----------------------------------------------------------
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr",
                    "purrr",
                    "ggplot2",
                    "ggpmisc",
                    "ggthemes")
  
  library_c(library_list)
  
# Read inputs -------------------------------------------------------------
  
  # NTS directory
  y_dir <- "Y:/NTS/"
  c_dir <- str_c("C:/Users/", user, "/Documents/NTS_C/")
  nts_dir <- ifelse(drive == "Y", y_dir, c_dir)
  export_dir <- str_c(nts_dir, "outputs/hb/hb_trip_rates/")
  
  # HB Classified Build
  cb_dir <- str_c(nts_dir, "classified builds/cb_hb_tr_", tfn_or_ntem, ".csv")
  model_forms_dir <- str_c(nts_dir, "import/hb_trip_rates/model_forms.csv")
  response_weights_dir <- str_c(nts_dir, "import/hb_trip_rates/hb_response_weights_", tfn_or_ntem, ".csv")
  
  # cTripEnd Trip Rates
  ctripend_dir <- str_c(nts_dir, "import/ctripend/TR_NTEM_TrendSnr1_Year2016_NTEMT1.csv")
  
  # Read
  cb <- read_csv(cb_dir)
  model_forms <- read_csv(model_forms_dir)
  response_weights <- read_csv(response_weights_dir)
  ctripend <- read_csv(ctripend_dir)
  
  # Export directory
  out_hb_tr_dir <- str_c(export_dir, "hb_trip_rates_")
  out_hb_tr_dir <- ifelse(tfn_or_ntem == "tfn", 
                          str_c(out_hb_tr_dir, "tfn_v2.0.csv"), 
                          str_c(out_hb_tr_dir, "ntem.csv"))
  
  out_plot_dir <- str_c(export_dir, "Reports/", tfn_or_ntem, "_vs_ctripend_trip_rates.png")
  out_purpose_plot_dir <- str_c(export_dir, "Reports/", tfn_or_ntem, "_vs_ctripend_trip_rates_purposes.png")
  
  # Create export directories
  dir.create(export_dir, showWarnings = FALSE)
  
# Pre Processing ----------------------------------------------------------

  if(tfn_or_ntem == "tfn"){
    
    # Combine AT 1 & 2
    cb <- mutate(cb, tfn_at = ifelse(tfn_at == 1, 2, tfn_at))
    
    # Define covars for age work status groups
    worker_covars <- c("gender", "hh_type", "soc", "ns", "tfn_at", "SurveyYear")
    child_covars <- c("hh_type", "ns", "tfn_at", "SurveyYear")
    non_worker_covars <- c("gender", "hh_type", "ns", "tfn_at", "SurveyYear")
    
  } else if(tfn_or_ntem == "ntem"){
    
    cb <- filter(cb, SurveyYear %in% 2002:2012)
    
    worker_covars <- c("gender", "hh_type", "ntem_at", "SurveyYear")
    child_covars <- c("hh_type", "ntem_at", "SurveyYear")
    non_worker_covars <- c("gender", "hh_type", "ntem_at", "SurveyYear")
    
  }

  # Define glm formulas for age work status groups
  worker_formula <- str_c("weekly_trips ~ ", str_c(worker_covars, collapse = " + "))
  child_formula <- str_c("weekly_trips ~ ", str_c(child_covars, collapse = " + "))
  non_worker_formula <- str_c("weekly_trips ~ ", str_c(non_worker_covars, collapse = " + "))
  
  # Convert co-variates to factors
  cb <- mutate(cb, across(all_of(worker_covars), ~ factor(.)))
  
  # Join formulas based on trip purpose and age work status
  models <- model_forms %>%
    mutate(mod_formula = case_when(
      aws == 1 & mod_form == "nb" ~ child_formula,
      aws == 1 & mod_form != "nb" ~ str_c(child_formula, " | 1"),
      aws %in% 2:3 & mod_form == "nb" ~ worker_formula,
      aws %in% 2:3 & mod_form != "nb" ~ str_c(worker_formula, " | 1"),
      aws %in% 4:6 & mod_form == "nb" ~ non_worker_formula,
      aws %in% 4:6 & mod_form != "nb" ~ str_c(non_worker_formula, " | 1"),
    ))
  
  # Filter for corresponding segment
  models <- mutate(models, hb_df = map2(p, aws, function(x, y) filter(cb, p == x, aws == y)))
  
# Model Building and predicting -------------------------------------------
  
  # Build model for each segment
  models <- mutate(models, hb_model = pmap(list(mod_form, mod_formula, hb_df), build_model))
  
  # Extract new levels and combine crosswise to build new data set for prediction
  models <- mutate(models, new_data = map2(hb_model, mod_form, extract_levels))
  
  # Add predictions for the new data set
  models <- mutate(models, new_data = map2(new_data, hb_model, add_predictions))
  
  # Process data to fill in missing classifications. i.e. gender for children
  models <- mutate(models, new_data = map2(new_data, aws, process_newdata, tfn_or_ntem))

# Un-weighted Regressions Report ------------------------------------------
  
  # Same format as ATKINS/AECOM 2016 Report
  unweight_report <- models %>%
    mutate(trip_rate = map(new_data, function(x) x %>% summarise(trip_rate = mean(trips)))) %>%
    unnest(cols = trip_rate) %>%
    select(p, aws, trip_rate) %>%
    pivot_wider(names_from = aws, values_from = trip_rate)
  
  colnames(unweight_report) <- c("P", "Child", "FTE", "PTE", "STU", "NEET", "Above_75")  

  unweight_report <- select(unweight_report, P, FTE, PTE, NEET, STU, Above_75, Child)
  
  write_csv(unweight_report, str_c(export_dir, "Reports/unweighted_trip rates_report_", tfn_or_ntem, ".csv"))
  
# Weighted Regressions ----------------------------------------------------

  
  
  # Add purpose and age work status back in
  models <- models %>%
    mutate(new_data = pmap(list(new_data, p, aws), infil_p_aws))
  
  models <- models %>%
    mutate(new_data = map(new_data, c_weighted_rates, response_weights, tfn_or_ntem))

  hb_trip_rates <- models %>% 
    pull(new_data) %>% 
    bind_rows() %>% 
    rename(trip_rates = trips)
  
  if(tfn_or_ntem == "ntem"){
    
    hb_trip_rates <- hb_trip_rates %>% 
      lu_ntem_tt() %>%
      select(p, ntem_tt, ntem_at, trip_rates) %>% 
      arrange(p, ntem_tt, ntem_at)
    
  } else if(tfn_or_ntem == "tfn"){
    
    hb_trip_rates <- hb_trip_rates %>%
      filter(tfn_at == 2) %>% 
      mutate(tfn_at = factor(1)) %>%
      bind_rows(hb_trip_rates)
    
    hb_trip_rates <- hb_trip_rates %>%
      lu_tt() %>% 
      select(p, tfn_tt, ntem_tt, tfn_at, trip_rates) %>% 
      arrange(p, tfn_tt, ntem_tt, tfn_at)
    
  }
  
  # Save HB Trip Rates
  write_csv(hb_trip_rates, out_hb_tr_dir)
  
# Trip Rates vs cTripEnd Report -------------------------------------------
  
  if(tfn_or_ntem == "tfn"){
    
    hb_trip_rates <- hb_trip_rates %>%
      rename(area_type = tfn_at) %>% 
      group_by(p, ntem_tt, area_type) %>%
      summarise(trip_rates = mean(trip_rates)) %>% 
      ungroup()
    
  } else if(tfn_or_ntem == "ntem"){
    
    hb_trip_rates <- rename(hb_trip_rates, area_type = ntem_at)
    
  }
  
  ctripend <- ctripend %>% 
    rename(p = h,
           ntem_tt = s,
           area_type = r,
           ctripend_rates = TripRates)
  
  joined_rates <- hb_trip_rates %>% 
    mutate(area_type = as.double(area_type)) %>% 
    left_join(ctripend)
  
  joined_rates <- joined_rates %>%
    mutate(p = case_when(
      p == 1 ~ "Commute",
      p == 2 ~ "Business",
      p == 3 ~ "Education",
      p == 4 ~ "Shopping",
      p == 5 ~ "PB",
      p == 6 ~ "Leisure",
      p == 7 ~ "Visit Friends",
      p == 8 ~ "Holiday/Day Trip"
    ))
  
  joined_rates <- joined_rates %>%
    mutate(p = factor(p, levels = c("Commute", "Business", "Education", "Shopping", "PB", "Leisure", "Visit Friends", "Holiday/Day Trip")))
  
  ylabel <- ifelse(tfn_or_ntem == "tfn", "TfN", "NTEM")
  
  single_plot <- joined_rates %>%
    ggplot(aes(x = ctripend_rates, y = trip_rates)) +
    geom_smooth(method = "lm", se = FALSE, colour = "red", formula = y ~ x) +
    stat_poly_eq(formula = y ~ x, 
                 aes(label = paste0(..eq.label..)), 
                 parse = TRUE,
                 colour = "red") +
    stat_poly_eq(formula = y ~ x, 
                 aes(label = paste0(..rr.label..)), 
                 parse = TRUE,
                 colour = "red",
                 label.y = 0.9) +
    geom_point() +
    xlab("CTripEnd Trip Rates") +
    ylab(str_c(ylabel, " Trip Rates")) +
    geom_rangeframe() +
    theme_tufte()
  
  png(filename = out_plot_dir,
      type="cairo",
      units="in", 
      width=4, 
      height=4, 
      pointsize=12, 
      res=300)
  
  print(single_plot)
  dev.off()
  
  png(filename = out_purpose_plot_dir,
      type="cairo",
      units="in", 
      width=6, 
      height=12, 
      pointsize=6, 
      res=300)
  
  purpose_plot <- single_plot +
    facet_wrap( ~ p , ncol = 2, scales = "free") +
    geom_blank() +
    theme(strip.text = element_text(face="bold", size = 12),
          axis.title.x = element_text(size = 17),
          axis.title.y = element_text(size = 17))
  
  print(purpose_plot)
  dev.off()
  
}


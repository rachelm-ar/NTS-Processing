#' TODO:
#' 1. Create function to factor variables using a list
#' 2. Use weighted average to remove SurveyYear

library(tidyverse)

# Read in hb classified build
cb <- read_csv("C:/Users/Pluto/Documents/NTS_C/classified builds/cb_hb trip rates_tfn.csv")
model_forms <- read_csv("C:/Users/Pluto/Documents/NTS_C/import/hb_trip_rates/model_forms.csv")
response_weights <- read_csv("C:/Users/Pluto/Documents/NTS_C/outputs/hb/hb_trip_rates/hb_response_weights.csv")

cb1 <- cb 
  
cb1 <- cb1 %>%
  mutate(tfn_area_type = ifelse(tfn_area_type == 1, 2, tfn_area_type))

cb2 <- cb1 %>%
  mutate(gender = factor(gender, 1:3),
         hh_type = factor(hh_type, 1:8),
         soc = factor(soc, 1:3),
         ns = factor(ns, 1:5),
         tfn_area_type = factor(tfn_area_type, 2:8),
         SurveyYear = factor(SurveyYear, c(2002:2010, 2012:2019)))

cb2 <- cb2 %>%
  mutate(weights = W2 * W5)

worker_covars <- c("gender", "hh_type", "soc", "ns", "tfn_area_type", "log(W5)")
child_covars <- c("hh_type", "ns", "tfn_area_type", "log(W5)")
non_worker_covars <- c("gender", "hh_type", "ns", "tfn_area_type", "log(W5)")

worker_formula <- str_c("weekly_trips ~ ", str_c(worker_covars, collapse = " + "))
child_formula <- str_c("weekly_trips ~ ", str_c(child_covars, collapse = " + "))
non_worker_formula <- str_c("weekly_trips ~ ", str_c(non_worker_covars, collapse = " + "))

models <- model_forms %>%
  mutate(mod_formula = case_when(
    aws == 1 & mod_form == "nb" ~ child_formula,
    aws == 1 & mod_form != "nb" ~ str_c(child_formula, " | 1"),
    aws %in% 2:3 & mod_form == "nb" ~ worker_formula,
    aws %in% 2:3 & mod_form != "nb" ~ str_c(worker_formula, " | 1"),
    aws %in% 4:6 & mod_form == "nb" ~ non_worker_formula,
    aws %in% 4:6 & mod_form != "nb" ~ str_c(non_worker_formula, " | 1"),
  ))

# Filter df for segment
models <- models %>%
  mutate(hb_df = map2(p, aws, function(x, y) filter(cb2, trip_purpose == x, age_work_status == y)))

# Build a model for each segment

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

models <- models %>% 
  mutate(hb_model = pmap(list(mod_form, mod_formula, hb_df), build_model))

# Extract new levels for each segment
extract_newdata <- function(hb_model, mod_form){
  
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

models <- models %>%
  mutate(new_data = map2(hb_model, mod_form, extract_newdata))

# Make predictions for each segment
add_predictions <- function(new_data, hb_model){
  
  predictions <-  predict(hb_model, new_data, type = "response")
  
  new_data %>%
    mutate(trips = predictions)
  
}

models <- models %>%
  mutate(new_data = map2(new_data, hb_model, add_predictions))

# Process new data

process_newdata <- function(new_data, aws){
  
  # Add SOC & gender for children
  # Add SOC for non working
  if(aws == 1){
    
    new_data <- new_data %>%
      mutate(gender = factor(1),
             soc = factor(2))
    
  } else if(aws %in% 4:6){
    
    new_data <- mutate(new_data, soc = factor(2))
    
  }
  
  new_data
  
  # No longer need SurveyYear
  #new_data %>%
  #  group_by(gender, hh_type, soc, ns, tfn_area_type) %>%
  #  summarise(trips = mean(trips)) %>%
  #  ungroup()
  
}

models <- models %>%
  mutate(new_data = map2(new_data, aws, process_newdata))

output <- models %>%
  mutate(trip_rate = map(new_data, function(x) x %>% summarise(trip_rate = mean(trips)))) %>%
  unnest(cols = trip_rate) %>%
  select(p, aws, trip_rate) %>%
  pivot_wider(names_from = aws, values_from = trip_rate)

colnames(output) <- c("p", "child", "fte", "pte", "STU", "NEET", "Old")  

output %>%
  select(p, fte, pte, NEET, STU, Old, child) %>% 
  write_csv("C:/Users/Pluto/Documents/NTS_C/test.csv")

# multiply unweighted trip rates with response weights
models2 <- models %>%
  left_join(response_weights, by = c('p' = 'trip_purpose',
                                     'aws' = 'age_work_status')) %>%
  rename(W5 = W5W2) %>%
  mutate(new_data2 = pmap(list(p, aws, new_data, W5), function(x,y,z,k){
    
    z %>%
      mutate(trips = trips * k,
             p = x,
             aws = y)
    
  }))
  
hb_trip_rates <- models2 %>% 
  pull(new_data2) %>%
  bind_rows() %>%
  rename(trip_purpose = p,
         age_work_status = aws,
         trip_rates = trips) %>%
  lu_tfn_tt("tfn")

hb_trip_rates <- hb_trip_rates %>%
  rename(p = trip_purpose,
         trip_rate = trip_rates,
         area_type = tfn_area_type,
         traveller_type = tfn_traveller_type) %>%
  select(p, traveller_type, soc, ns, area_type, trip_rate)

# Add area type 1 back in
hb_trip_rates <- hb_trip_rates %>%
  filter(area_type == 2) %>%
  mutate(area_type = factor(1)) %>%
  bind_rows(hb_trip_rates)

write_csv(hb_trip_rates, "C:/Users/Pluto/Documents/NTS_C/outputs/hb/hb_trip_rates/hb_trip_rates_2002to2017.csv")

test <- read_csv("C:/Users/Pluto/Documents/classified_nts_pre-weighting_child_genderless.csv")

ppp1 <- test %>%
  filter(trip_purpose %in% 1:8) %>%
  mutate(trip_weights = W1 * W5xHh * W2) %>%
  group_by(IndividualID, trip_purpose, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, W2) %>%
  summarise(trip_weights = sum(trip_weights),
            weekly_trips = n())%>%
  ungroup() %>%
  mutate(trip_rate = trip_weights/W2,
         trip_purpose = as.integer(trip_purpose))

df <- ppp1 %>%
  complete(nesting(IndividualID, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type),
           trip_purpose = 1:8,
           fill = list(weekly_trips = 0, trip_rate = 0, trip_weights = 0))

hb_df <- df %>%
  select(age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, trip_purpose, weekly_trips, trip_rate, trip_weights) %>%
  mutate_at(.vars = c('age_work_status', 'gender', 'hh_adults', 'cars', 'soc_cat', 'ns_sec', 'tfn_area_type'), .funs = as.factor) %>%
  mutate(trip_weights = ifelse(trip_weights == 0, 1, trip_weights),
         trip_rate = ifelse(trip_rate == 0, 1, trip_rate))

variables <- c("age_work_status",
               "gender", 
               "hh_adults", 
               "cars", 
               "soc_cat", 
               "ns_sec",
               "tfn_area_type")

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
  
  # SOC_CAT for commuting and business, NS-SEC for all others
  if(i %in% c(1,2)){
    
    vars <- str_subset(variables, "ns_sec", negate = TRUE)
    
    var_levels <- list_modify(variable_levels, "ns_sec" = NULL)
    
  } else {
    
    vars <- str_subset(variables, "soc_cat", negate = TRUE)
    
    var_levels <- list_modify(variable_levels, "soc_cat" = NULL)
    
  }
  
  final_df[[i]] <- purpose_df[[i]]
  
  # Formula to use depending on whether we want soc_cat or ns_sec
  nbr_formula <- as.formula(paste("weekly_trips", paste(vars, collapse = " + "), sep = " ~ "))
  
  final_model[[i]] <- zeroinfl(formula = weekly_trips ~ age_work_status + gender + hh_adults + cars + soc_cat + ns_sec + tfn_area_type,
                              data = final_df[[i]],
                              dist = 'negbin')
  
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
  
  new_data <- new_data %>%
    unite("traveller_type_char", "age_work_status", "gender", "hh_adults", "cars", remove=FALSE, sep="_") %>%
    lu_traveller_type() %>%
    na.omit()
  
  # Keep only the combinations in NTEM traveller types
  tfn_trip_rates[[i]] <- new_data %>%
    mutate(purpose = i) %>%
    select(purpose, traveller_type, tfn_area_type, soc_cat, ns_sec, tfn_predictions)

}

tfn_trip_rates_result <- tfn_trip_rates %>%
  bind_rows() %>%
  arrange(purpose, traveller_type, tfn_area_type, soc_cat, ns_sec)

trip_rates_out <- post_processing(tfn_trip_rates_result)


test %>% 
  select(IndividualID, SurveyYear, age_work_status, gender, hh_adults, area_type, cars, W1, W2, W5xHh) %>%
  filter(age_work_status == 1, gender == 0, hh_adults == 1, cars == 1, area_type == 1, SurveyYear == 2002) %>%
  count(W2)

test %>%
  colnames()


trip_rates_out %>% write_csv("Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates_HA_ZINB.csv")

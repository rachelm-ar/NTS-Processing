library("tidyverse")
library("sjstats")
library("MASS")
library("survey")

select <- dplyr::select

# Split data by purpose
purpose_split <- df1 %>% group_split(hb_purpose)

# Loop over all purposes
main_loop <- lapply(purpose_split, function(purpose_data){
  
  # Split data into 75% train and 25% test
  smp_size <- floor(0.75*nrow(purpose_data))
  
  train_ind <- sample(seq_len(nrow(purpose_data)), size = smp_size)
  train <- df1 %>% slice(train_ind)
  test <- df1 %>% slice(-train_ind)
  
  ## TODO: Use Ian Williams reccomendation: svyglm.nb instead of glm.nb
  
  ## Create a survey design for Negative-Binomial regression model
  #des <- svydesign(
  #  id = ~1,
  #  strat = NULL,
  #  weights = NULL,
  #  data = purpose_data,
  #  nest = TRUE
  #)
  
  # Build a Negative-bionomial regression model with all variables
  model_full <- glm.nb(formula = tfn_trip_rate ~ household + age, subset = train_ind, data = purpose_data)
  
  # Extract all categorical variables used inside model
  categorical_variables <- model_full$xlevels
  
  # Extract classification levels used for each variable
  variable_levels <- model_full$model %>% colnames() %>% setdiff(.,"tfn_trip_rate")
  
  # Extract p-value
  p_values <- model_full %>% summary()
  p_values <- p_values$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
  
})


#################### Testing
Test_df1 <- df1 %>% filter(hb_purpose == 1)

smp_size <- floor(0.75*nrow(Test_df1))

train_ind <- sample(seq_len(nrow(Test_df1)), size = smp_size)
train <- df1 %>% slice(train_ind)
test <- df1 %>% slice(-train_ind)
model_full <- glm.nb(formula = tfn_trip_rate ~ household + age, subset = train_ind, data = Test_df1)
categorical_variables <- model_full$model %>% colnames() %>% setdiff(.,"tfn_trip_rate")
variable_levels <- model_full$xlevels

p_values <- model_full %>% summary()
p_values <- p_values$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
p_values <- tibble::rownames_to_column(p_values, "Variables")

current_var <- categorical_variables[1]


## Check if any rows are insignificant for the first variable

# Extract classifications for first variable
Current_classes <- p_values %>% filter(str_detect(Variables, current_var))

# First check if there are more than 2 classifications (excluding control classification)
if(nrow(Current_classes) > 1){
  
  # Identify classifications which are insignificant and which classification to merge with
  if(any(Current_classes$`Pr(>|z|)` > 0.05)){
    
    Current_classes
    
    
      
  } else {
    
    print(paste0("All classifications for ", current_var, " are significant so there is no need to aggregate"))
    
  }
  
} else {
  
  print(paste0(current_var, " has less than two classifications so we can no longer aggregate further"))
  
}










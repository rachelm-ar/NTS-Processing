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
  p_values <- tibble::rownames_to_column(p_values, "Variables")
  
})


#################### Testing
Test_df1 <- df1 %>% filter(hb_purpose == 1)

smp_size <- floor(0.75*nrow(Test_df1))

train_ind <- sample(seq_len(nrow(Test_df1)), size = smp_size)
train <- Test_df1 %>% slice(train_ind)
test <- Test_df1 %>% slice(-train_ind)
model_full <- glm.nb(formula = tfn_trip_rate ~ household + area_type + employment + gender + cars , subset = train_ind, data = Test_df1)
categorical_variables <- model_full$model %>% colnames() %>% setdiff(.,"tfn_trip_rate")
variable_levels <- model_full$xlevels

p_values <- model_full %>% summary()
p_values <- p_values$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
p_values <- tibble::rownames_to_column(p_values, "Variables")

current_var <- categorical_variables[1]
current_var_levels <- variable_levels[[1]]

## Check if any rows are insignificant for the first variable

# Extract classifications for first variable
Current_classes <- p_values %>% filter(str_detect(Variables, current_var))

# First check if there are more than 2 classifications (excluding control classification) and atleast one classification is insignificant
if(nrow(Current_classes) > 1 & any(Current_classes$`Pr(>|z|)`> 0.05)){
  
  # We have the following combinations:
  # Current: 1, 2, 3+
  # Possible: 1, 2 & 3+, 1 & 2, 1 & 3+
  # We want to access the data, join these together and run regressions on the 3 cases
  # If we find a case which atleast one of the variables are now significant, we can use this case
  
  # Extract permutation of all possible classifications
  classes_combination <- combn(current_var_levels, m = 2, simplify = FALSE)
  
  # Extract all possible combinations of 
  aggregated_combinations <- lapply(classes_combination, function(classes_combination){
    
    Test_df1 %>%
      mutate(household = case_when(
        household %in% classes_combination ~ str_c(classes_combination, collapse = " Join "),
        TRUE ~ as.character(household),
      ))
    
  })
  
  # Run new regression model with all possible combinations
  
  t1 <- lapply(aggregated_combinations, function(x){
    
    # Updated Models
    
    updated_model <- glm.nb(formula = tfn_trip_rate ~ household + area_type + employment + gender + cars , subset = train_ind, data = x)
    updated_pvalues <- updated_model %>% summary()
    updated_pvalues <- updated_pvalues$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
    updated_pvalues <- tibble::rownames_to_column(updated_pvalues, "Variables")
    current_pvalues <- updated_pvalues %>% filter(str_detect(Variables, current_var))
  
  })
  
  t1[[1]]
  
  t1[[2]]$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
  tibble::rownames_to_column(t1[[1]], "Variables")
  
  current_var <- categorical_variables[1]
  current_var_levels <- variable_levels[[1]]
  
  ## Check if any rows are insignificant for the first variable
  
  # Extract classifications for first variable
  Current_classes <- p_values %>% filter(str_detect(Variables, current_var))

  
    
} else {
  
  print(paste0(current_var, " has either less than 2 classifications or all classifications are significant"))
  
}

####### More testing

Test_df1 %>% select(household)

Test2_df <- Test_df1 %>%
  mutate(New_Household = case_when(
    household == "1 Adult" ~ "1 Adult Join 2 Adults",
    household == "2 Adults" ~ "1 Adult Join 2 Adults",
    household == "3+ Adults" ~ "3+ Adults",
  ))

glm.nb(formula = tfn_trip_rate ~ New_Age, subset = train_ind, data = Test3_df) %>% summary()

Test3_df <- Test_df1 %>%
  mutate(New_Age = case_when(
    age == "16-74" ~ "under 16 join 16-74",
    age == "75 or over" ~ "75 or over",
    age == "under 16" ~ "under 16 join 16-74"
  ))

Test3_df$New_area <- Test3_df$New_area %>% factor(ordered = FALSE)

Test3_df <- Test3_df %>% mutate(New_area = relevel(New_area, ref = "1"))

glm.nb(formula = tfn_trip_rate ~  New_area + age, subset = train_ind, data = Test3_df) %>% summary()

Test_df1 %>% select(age) %>% distinct()



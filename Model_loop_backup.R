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
  
  
  # Extract categorical classifications
  # model_classifications <- summary(model_full)
  # model_classifications <- model_classifications$coefficients
  # model_classifications <- as.data.frame(model_classifications)
  # 
  # classification_names <- model_classifications %>% rownames()
  # classification_pvalues <- model_classifications %>% select("Pr(>|z|)")
  
  
})

Test <- main_loop[[1]]

variable_names <- Test$model %>% colnames() %>% setdiff(.,"tfn_trip_rate")
model_classifcations <- summary(Test)
model_classifcations$coefficients
Test$xlevels



Test %>% summary()

## TODO: Automation attempt
Test$xlevels

lapply(Test$xlevels, length)

Test %>% summary()


## Junk
main_loop[[1]]$xlevels
test_sums <- summary(main_loop[[1]])
test_sums_coefficients <- test_sums$coefficients
test_sums_coefficients %>% rownames()
main_loop[[1]]$xlevels
main_loop[[1]]$coefficients

test_sums_coefficients %>% colnames()

main_loop[[1]] %>% summary()

test_data <- df1 %>% filter(hb_purpose == 1)

smp_size <- floor(0.75*nrow(test_data))
train_ind <- sample(seq_len(nrow(test_data)), size = smp_size)
train <- test_data %>% slice(train_ind)
test <- test_data %>% slice(-train_ind)

des <- svydesign(
  id = ~ 0,
  strat = NULL,
  weights = NULL,
  data = test_data,
  nest = TRUE
)
svyglm.nb(tfn_trip_rate ~ employment, des)
glm.nb(formula = tfn_trip_rate ~ household + age, subset = train_ind, data = test_data) %>% summary()


for (p in purposes){
  
  # Filter for purpose
  df1 %>% select(hb_purpose) %>% distinct()
  
  
  # Create a survey design
  des <- svydesign(
    id = ~1,
    strat = NULL,
    weights = NULL,
    data = df1,
    nest = TRUE
  )
  
  
  for(v in variables){
    
    # Check if any classifications of a variable have an insignificant p-value
    if( p-value > 0.05){
      
      # Then aggregate with another classification within the same variable
      
      # Try the regression model again
      
      # If the new classification is still not significant then aggregate with another variable by changing the data set
      
      # Check
      
      # The above will most likely need to be another loop (not sure at this)
      
      # Finally: If you have only two classifications remaining and they are still insignificant, choose to drop the first classification
    }
  }
}
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












####################################################################################################################
####################################################################################################################

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
  
  dependent_variable <- "tfn_trip_rate"
  independent_variables <- c("household", "age")
  nbr_formula <- paste("tfn_trip_rate", paste(independent_variables, collapse = " + "), sep = " ~ ")
  
  # Build a Negative-bionomial regression model with all variables
  model_full <- glm.nb(formula = nbr_formula, subset = train_ind, data = purpose_data)
  
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

dependent_variable <- "tfn_trip_rate"
independent_variables <- c("household", "age")
nbr_formula <- paste("tfn_trip_rate", paste(independent_variables, collapse = " + "), sep = " ~ ")



glm.nb(formula = nbr_formula, data = Test_df1)

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

# First check if there are atleast 2 classifications (excluding control classification) and atleast one classification is insignificant
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
  
  t1
  
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


glm.nb(tfn_trip_rate ~ age_work_status, data = unclassified_build)

unclassified_build %>% View()

###########################################################################################################
###########################################################################################################
library("tidyverse")
library("sjstats")
library("MASS")
library("survey")
library("combinat")

select <- dplyr::select

Testingg <- df1 %>% pull(household) %>% levels()

variable_levels <- lapply(independent_variables, function(x) df1 %>% pull(x) %>% levels())

variable_permutations <- independent_variables %>% permn()




# Split data by purpose
purpose_split <- df1 %>% group_split(hb_purpose)

# Loop over all purposes
main_loop <- lapply(purpose_split, function(purpose_data){
  
  # Split data into 75% train and 25% test
  smp_size <- floor(0.75*nrow(purpose_data))
  
  train_ind <- sample(seq_len(nrow(purpose_data)), size = smp_size)
  train <- df1 %>% slice(train_ind)
  test <- df1 %>% slice(-train_ind)
  
  dependent_variable <- "tfn_trip_rate"
  independent_variables <- c("household", "age", "cars")
  nbr_formula <- paste("tfn_trip_rate", paste(independent_variables, collapse = " + "), sep = " ~ ")
  
  # Build a Negative-bionomial regression model with all variables
  model_full <- glm.nb(formula = nbr_formula, subset = train_ind, data = purpose_data)
  
  # Extract classification levels used for each variable
  variable_levels <- model_full$xlevels
  
  # Extract p-value
  p_values <- model_full %>% summary()
  p_values <- p_values$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
  p_values <- tibble::rownames_to_column(p_values, "Variables")
  
})

# 1) Extract all variables
# 2) Calculate permutations for variables
# 3) Extract all classifications for each variable
# 4) Calculate classification combinations for each variable
# 5) For each permutation of variables, run a model for all the combination of classifications
# Here is an example:
#   1. Variables: household, age, cars                            [Total = 3]
#   2. Permutations: h a c; h c a; a h c; a c h; c a h; c h a               [Total = 6]
#   3. Classifications for variables:
#       A. household: 1 Adult; 2 Adults; 3+ Adults                                    [Total = 3]
#       B. age: 16-74; 75+ ; under 16                                                 [Total = 3]
#       C. cars: 0, 1, 1+, 2+                                                         [Total = 4]
#   4. Classification Combinations for each variable: (in bracket => no aggregation)
#       A. household: 1A + 2A (3A); 1A + 3A (2A); 2A + 3A (1A); (1A) (2A) (3A)                  [Total = 4]
#       B. age: 16-74 + U16 (75p); 16-74 + 75p (U16); U16 + 75p (16-74); (U16) (75p) (16-74)    [Total = 4]
#       C. cars: 0 + 1 (1p)(2p); 0 + 1p (1)(2p); 0 + 2p (1)(1p); 
#                1 + 1p (0)(2p); 1 + 2p (0)(1p);
#                1p + 2p (0)(1);
#                (0)(1)(1p)(2p)                                                                 [Total = 7]
#   5. Combination of classifications for permutation of variables:
#       A. Each permutation has 15 regression models built: E.G. h a c -- 4 + 4 + 7 = 15        [Total = 15]
#       B. There are 6 permutations of variables so in total there will be 6 * 15 regression models         [[TOTAL = 90]]

## TODO: Work out a strategy to identify which of these 90 models are best.
# This will be something along the lines of: Most number of significant variables and then we drop all variables which are not significant

#################### Testing
Test_df1 <- df1 %>% filter(hb_purpose == 1)

smp_size <- floor(0.75*nrow(Test_df1))

train_ind <- sample(seq_len(nrow(Test_df1)), size = smp_size)
train <- Test_df1 %>% slice(train_ind)
test <- Test_df1 %>% slice(-train_ind)

dependent_variable <- "tfn_trip_rate"
independent_variables <- c("household", "age")
nbr_formula <- paste("tfn_trip_rate", paste(independent_variables, collapse = " + "), sep = " ~ ")

glm.nb(formula = nbr_formula, data = Test_df1)

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

# First check if there are atleast 2 classifications (excluding control classification) and atleast one classification is insignificant
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
  
  Model_Updater <- function(classification_combinations, nbr_formula){
    
    updated_model <- glm.nb(formula = nbr_formula, subset = train_ind, data = classification_combinations)
    updated_pvalues <- updated_model %>% summary()
    updated_pvalues <- updated_pvalues$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
    updated_pvalues <- tibble::rownames_to_column(updated_pvalues, "Variables")
    current_pvalues <- updated_pvalues %>% filter(str_detect(Variables, current_var))
    
  }
  
  lapply(aggregated_combinations, Model_Updater, nbr_formula)
  
  t1
  
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


glm.nb(tfn_trip_rate ~ age_work_status, data = unclassified_build)

unclassified_build %>% View()



## TODO: Use Ian Williams reccomendation: svyglm.nb instead of glm.nb

## Create a survey design for Negative-Binomial regression model
#des <- svydesign(
#  id = ~1,
#  strat = NULL,
#  weights = NULL,
#  data = purpose_data,
#  nest = TRUE
#)


################################
#### Apply familly attempt

main_loop <- lapply(purpose_split, function(purpose_data){
  
  # Split data into 75% train and 25% test
  smp_size <- floor(0.75*nrow(purpose_data))
  
  train_ind <- sample(seq_len(nrow(purpose_data)), size = smp_size)
  train <- df1 %>% slice(train_ind)
  test <- df1 %>% slice(-train_ind)
  
  
  
})



lapply(variable_permutations, function(var_perm){
  
  model_full <- list()
  
  for(i in 1:length(var_perm)){
    
    # Build a Negative-bionomial regression model with all variables
    model_full  <- glm.nb(formula = nbr_formula, subset = train_ind, data = purpose_split[[1]])
    
  } 
  
})

lapply(variable_permutations, function(var_perm){
  
  lapply(var_perm, function(var_class){
    
    glm.nb(formula = nbr_formula, subset = train_ind, data = purpose_split[[1]])
    
  })
  
})

# Extract p-value
p_values <- model_full %>% summary()
p_values <- p_values$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
p_values <- tibble::rownames_to_column(p_values, "Variables")

########################

for (perm in 1:length(variable_permutations)){
  
  for(perm_comb in 1:length(variable_permutations[[perm]])){
    
    for (var_class in 1:length(variable_permutations[[perm]][[perm_comb]])){
      
      for(var_level in 1:length(variable_levels[[perm_comb]])){
        
        # Extract permutation of all possible classifications
        classes_combination <- combn(variable_levels[[perm_comb]], m = 2, simplify = FALSE)
        
        for (class_comb in 1:length(classes_combination)){
          
          New_df <-  Test_df1 %>%
            mutate(variable_permutations[[perm]][[perm_comb]] %in% classes_combination[[class_comb]] ~ str_c(classes_combination[[class_comb]], collapse = " Join "),
                   TRUE ~ as.character(variable_permutations[[perm]][[perm_comb]]))
          
          
          
        }
      }
    }
  }
}


######################################################################################
library("tidyverse")
library("sjstats")
library("MASS")
library("survey")
library("combinat")

# Redefine select if masked by MASS
select <- dplyr::select

# Independent variables to be included in model
variables <- c("household", "age", "cars")

# Levels within each independent variable
variable_levels <- lapply(variables, function(x) df1 %>% pull(x) %>% levels())

# Permutations of variables
variable_permutations <- independent_variables %>% permn()

# Formula for Negative binomial regression
nbr_formula <- paste("tfn_trip_rate", paste(variables, collapse = " + "), sep = " ~ ")

# Split data by purpose
purpose_split <- df1 %>% group_split(hb_purpose)

#### For loop attempt

# Initialise structures
model_full <- list()
p_values <- list()

for (p in 1:length(purpose_split)){
  
  # Split data into 75% train and 25% test
  smp_size <- floor(0.75*nrow(purpose_split[[p]]))
  
  train_ind <- sample(seq_len(nrow(purpose_split[[p]])), size = smp_size)
  train <- df1 %>% slice(train_ind)
  test <- df1 %>% slice(-train_ind)
  
  # Extract permutation of all possible classifications
  classes_combination <- combn(variable_levels[[1]], m = 2, simplify = FALSE)
  
  combn(variable_levels[[3]], m = 2, simplify = FALSE)
  permn(variable_levels[[3]])
  
  Test <- c(1,2,3,4)
  
  # Extracts all classification combinations required
  combinations <- do.call(c, lapply(seq_along(variable_levels[[1]]), combn, x = variable_levels[[1]], simplify = FALSE))
  combinations <- combinations[(length(variable_levels[[1]]) + 1): (length(combinations)-1)]
  combinations <- c(list(""), combinations)
  # Aggregate data with classification combinations
  
  Tt <- lapply(combinations, function(x){
    
    New_df <- Test_df1 %>%
      mutate(household = case_when(
        household %in% x ~ str_c(x, collapse = " Join "),
        TRUE ~ as.character(household),
      )) %>% select(household) %>% distinct()
    
    # Run a NBR model
    glm.nb(formula = nbr_formula, subset = train_ind, data = New_df)
    
  })
  
  a <- c("1A Join 2A", "1A Join 3A", "2A Join 3A","No Join")
  b <- c("1,2",
         "1,3",
         "1,4",
         "1,2,3",
         "1,3,4",
         "1,2,4",
         "2,3",
         "2,4",
         "2,3,4",
         "3,4",
         "No Join")
  c <- c("1 Join 2", "1 Join 3", "2 Join 3", "No Join")
  
  variables
  variable_levels
  
  crossing(a,b,c)
  
  
  Tt[[4]] %>% select(household)
  glm.nb(formula = nbr_formula, subset = train_ind, data = Tt[[4]])
  
  Test_df1 %>%
    mutate(household = case_when(
      household %in% combinations[[1]] ~ str_c(combinations[[1]], collapse = " Join "),
      TRUE ~ as.character(household),
    ))
  
  
  
}


# Build a Negative-bionomial regression model
model_full[[p]] <- glm.nb(formula = nbr_formula, subset = train_ind, data = purpose_split[[p]])

# Extract p-value
p_values[[p]] <- model_full[[p]] %>% summary()
p_values[[p]] <- p_values[[p]]$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
p_values[[p]] <- tibble::rownames_to_column(p_values[[p]], "Variables")



}


# 1) Extract all variables
# 2) Calculate permutations for variables
# 3) Extract all classifications for each variable
# 4) Calculate classification combinations for each variable
# 5) For each permutation of variables, run a model for all the combination of classifications
# Here is an example:
#   1. Variables: household, age, cars                            [Total = 3]
#   2. Permutations: h a c; h c a; a h c; a c h; c a h; c h a               [Total = 6]
#   3. Classifications for variables:
#       A. household: 1 Adult; 2 Adults; 3+ Adults                                    [Total = 3]
#       B. age: 16-74; 75+ ; under 16                                                 [Total = 3]
#       C. cars: 0, 1, 1+, 2+                                                         [Total = 4]
#   4. Classification Combinations for each variable: (in bracket => no aggregation)
#       A. household: 1A + 2A (3A); 1A + 3A (2A); 2A + 3A (1A); (1A) (2A) (3A)                  [Total = 4]
#       B. age: 16-74 + U16 (75p); 16-74 + 75p (U16); U16 + 75p (16-74); (U16) (75p) (16-74)    [Total = 4]
#       C. cars: 0 + 1 (1p)(2p); 0 + 1p (1)(2p); 0 + 2p (1)(1p); 
#                1 + 1p (0)(2p); 1 + 2p (0)(1p);
#                1p + 2p (0)(1);
#                (0)(1)(1p)(2p)                                                                 [Total = 7]
#   5. Combination of classifications for permutation of variables:
#       A. Each permutation has 15 regression models built: E.G. h a c -- 4 + 4 + 7 = 15        [Total = 15]
#       B. There are 6 permutations of variables so in total there will be 6 * 15 regression models         [[TOTAL = 90]]

## TODO: Work out a strategy to identify which of these 90 models are best.
# This will be something along the lines of: Most number of significant variables and then we drop all variables which are not significant

#################### Testing
Test_df1 <- df1 %>% filter(hb_purpose == 1)

smp_size <- floor(0.75*nrow(Test_df1))

train_ind <- sample(seq_len(nrow(Test_df1)), size = smp_size)
train <- Test_df1 %>% slice(train_ind)
test <- Test_df1 %>% slice(-train_ind)

dependent_variable <- "tfn_trip_rate"
independent_variables <- c("household", "age")
nbr_formula <- paste("tfn_trip_rate", paste(independent_variables, collapse = " + "), sep = " ~ ")

glm.nb(formula = nbr_formula, data = Test_df1)

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

# First check if there are atleast 2 classifications (excluding control classification) and atleast one classification is insignificant
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
  
  Model_Updater <- function(classification_combinations, nbr_formula){
    
    updated_model <- glm.nb(formula = nbr_formula, subset = train_ind, data = classification_combinations)
    updated_pvalues <- updated_model %>% summary()
    updated_pvalues <- updated_pvalues$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
    updated_pvalues <- tibble::rownames_to_column(updated_pvalues, "Variables")
    current_pvalues <- updated_pvalues %>% filter(str_detect(Variables, current_var))
    
  }
  
  lapply(aggregated_combinations, Model_Updater, nbr_formula)
  
  t1
  
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


glm.nb(tfn_trip_rate ~ age_work_status, data = unclassified_build)

unclassified_build %>% View()



## TODO: Use Ian Williams reccomendation: svyglm.nb instead of glm.nb

## Create a survey design for Negative-Binomial regression model
#des <- svydesign(
#  id = ~1,
#  strat = NULL,
#  weights = NULL,
#  data = purpose_data,
#  nest = TRUE
#)


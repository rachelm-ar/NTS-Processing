library("tidyverse")
library("sjstats")
library("MASS")
library("survey")
library("combinat")
library("magrittr")

# Redefine select if masked by MASS
select <- dplyr::select

# Read in Weekly trip rates
trip_rates_df <- read_csv("Y:/NTS/weekly_trip_rates_HA.csv")

# Transform catgeorical variables to factors
variables <- c("hb_purpose", "age_work_status", "gender", "hh_adults", "cars", "soc_cat", "ns_sec","area_type")
trip_rates_df <- trip_rates_df %>% mutate_at(variables, funs(factor))

# Independent variables to be included in model -- Testing
variables <- c("hh_adults", "age_work_status", "cars")

# Permutations of variables
variable_permutations <- variables %>% permn()

# Levels within each independent variable
variable_levels <- lapply(variables, function(x) trip_rates_df %>% pull(x) %>% levels())

# Combinations for each level to aggregate data with
combinations <- lapply(variable_levels, function(var_levels){
  
  combinations <- do.call(c, lapply(seq_along(var_levels), combn, x = var_levels, simplify = FALSE))
  combinations <- combinations[(length(var_levels) + 1): (length(combinations)-1)]
  combinations <- lapply(combinations, function(x) x %>% str_c(collapse = " Join "))
  combinations <- combinations %>% unlist()
  combinations <- c("", combinations)
  
})

# Create a dataframe with all possible combinations and then transform to list
names(combinations) <- variables
all_combinations <- do.call(crossing, combinations)
all_combinations <- split(all_combinations, seq(nrow(all_combinations))) %>% unname()

# Formula for Negative binomial regression
nbr_formula <- paste("tfn_trip_rate", paste(variables, collapse = " + "), sep = " ~ ")

# Split data by purpose
purpose_split <- trip_rates_df %>% group_split(hb_purpose)

# Split data into 75% train and 25% test
# smp_size <- floor(0.75*nrow(purpose_data))
# 
# train_ind <- sample(seq_len(nrow(purpose_data)), size = smp_size)
# train <- purpose_data %>% slice(train_ind)
# test <- purpose_data %>% slice(-train_ind)
# 
# train_ind <- lapply(purpose_split, function(x){
#   
#   smp_size <- floor(0.75*nrow(x))
#   train_ind <- sample(seq_len(nrow(x)), size = smp_size)
#   
# })

f1 <- function(all_combinations, variables, updated_df,trip_rates_df){
  
  for(i in 1:length(variables)){
    
    combs <- all_combinations %>% pull(variables[i]) %>% str_split(pattern = " Join ") %>% unlist()
    
    if(i == 1){
      
      updated_df[[i]] <- updated_df[[i]] %>%
        mutate(!!variables[i] := case_when(
          get(variables[i]) %in% combs ~ str_c(combs, collapse = " Join "),
          TRUE ~ as.character(get(variables[i]))
        ))
      
    } else {
      
      updated_df[[i]] <- updated_df[[i-1]] %>%
        mutate(!!variables[i] := case_when(
          get(variables[i]) %in% combs ~ str_c(combs, collapse = " Join "),
          TRUE ~ as.character(get(variables[i]))
        ))
      
    }
    
    if(i == length(variables)){
      
      final_df <- updated_df[[i]]
      
      updated_df <- list(trip_rates_df)
      
    }
    
  }
  
  # Build a Negative-bionomial regression model
  nbr_model <- glm.nb(formula = nbr_formula, data = final_df)
  
  # Extract p-values and obtain number of segments and significant segments and aic
  p_values_summary <- nbr_model %>% summary()
  p_values <- p_values_summary$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
  p_values <- tibble::rownames_to_column(p_values, "Variables")
  p_values_significant <- p_values %>% rename(pvals = "Pr(>|z|)") %>% filter(pvals < 0.05)
  significant_number <- nrow(p_values_significant) - 1
  number_variables <- nrow(p_values) - 1
  aic <- p_values_summary$aic
  
  results <- data.frame(number_segments = number_variables,
                        significant_variables = significant_number,
                        aic)
  
  list(final_df, results)

}

results_final <- lapply(purpose_split, function(purpose_data){
  
  updated_df <- list(purpose_data)
  
  results <- lapply(all_combinations[1:5], f1, variables, updated_df, trip_rates_df)
  
  results_df <- sapply(results, function(x) x[1])
  
  results_values <- sapply(results, function(x) x[2])
  
  model_select <- results_values %>%
    bind_rows %>% 
    mutate(n = seq_along(results)) %>%
    select(n,everything()) %>%
    arrange(desc(significant_variables), desc(number_segments), aic)
  
  combination_winner <- model_select %>% slice(1) %>% pull(n)
  
  df_winner <- results_df %>% extract2(combination_winner)
  
  # Build winner model
  model_winner <- glm.nb(formula = nbr_formula, data = df_winner)
  
})

##### TODO: 

#### Essentials
# 1. Split data into train and test (this resulted in errors so it has been taken out for now).
#    Good chance the current model is over-fitting 
# 2. Improve model selection method
# 2. Extract output for all models

#### Improve Script

#1. Add interactions

#2. Add sample size checks

#3. Use Ian Williams reccomendation: svyglm.nb instead of glm.n

## Create a survey design for Negative-Binomial regression model
#des <- svydesign(
#  id = ~1,
#  strat = NULL,
#  weights = NULL,
#  data = purpose_data,
#  nest = TRUE
#)
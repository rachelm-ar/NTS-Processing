library("tidyverse")
library("sjstats")
library("MASS")
library("survey")
library("combinat")

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

lapply(purpose_split, function(purpose_data){
  
  # Split data into 75% train and 25% test
  smp_size <- floor(0.75*nrow(purpose_data))
  
  train_ind <- sample(seq_len(nrow(purpose_data)), size = smp_size)
  train <- purpose_data %>% slice(train_ind)
  test <- purpose_data %>% slice(-train_ind)
  
  updated_df <- list(purpose_data)
  
  lapply(all_combinations[1:4], f1, variables, updated_df, trip_rates_df)
  
})

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
  nbr_model <- glm.nb(formula = nbr_formula, data = final_df, subset = train_ind)
  
  return(nbr_model)
  
}

# This gives you the final output!
pp <- lapply(all_combinations[1:3], f1, 
             variables, 
             updated_df = updated_df, 
             trip_rates_df = trip_rates_df)

smp_size <- floor(0.75*nrow(trip_rates_df))

train_ind123 <- sample(seq_len(nrow(trip_rates_df)), size = smp_size)
train <- trip_rates_df %>% slice(train_ind)
test <- trip_rates_df %>% slice(-train_ind)

  # Extract p-value
  p_values[[p]] <- model_full[[p]] %>% summary()
  p_values[[p]] <- p_values[[p]]$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
  p_values[[p]] <- tibble::rownames_to_column(p_values[[p]], "Variables")
  
  pp[[1]] %>% summary()

p_values <- model_full %>% summary()
p_values <- p_values$coefficients %>% as.data.frame() %>% select("Pr(>|z|)")
p_values <- tibble::rownames_to_column(p_values, "Variables")

## TODO: Use Ian Williams reccomendation: svyglm.nb instead of glm.nb

## Create a survey design for Negative-Binomial regression model
#des <- svydesign(
#  id = ~1,
#  strat = NULL,
#  weights = NULL,
#  data = purpose_data,
#  nest = TRUE
#)

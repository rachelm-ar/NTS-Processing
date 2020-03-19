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

trip_rates_cs <- read_csv("Y:/NTS/weekly_trip_rates.csv")

# Transform catgeorical variables to factors
variables <- c("age_work_status", 
               "gender", 
               "hh_adults", 
               "cars", 
               "soc_cat", 
               "ns_sec",
               "area_type"
               )

trip_rates_df <- trip_rates_df %>% mutate_at(variables, funs(factor))

Extract_levels <- function(variable_name,df){
  
  df %>%
    pull(variable_name) %>%
    as.factor() %>%
    levels()
  
}

variable_levels <- lapply(variables, Extract_levels, trip_rates_df)

# Formula for Negative binomial regression with all variables
nbr_formula <- paste("tfn_trip_rate", paste(variables, collapse = " + "), sep = " ~ ")

# Split data by purpose
purpose_split <- trip_rates_df %>% group_split(hb_purpose)

final_df <- list()
final_model <- list()
output <- list()

for(j in 1:length(purpose_split)){
  
  # Split data into 75% train and 25% test
  smp_size <- floor(0.75*nrow(purpose_split[[j]]))
  train_ind <- sample(seq_len(nrow(purpose_split[[j]])), size = smp_size)
  
  
  for (i in 1:length(variables)){
    
    if(i == 1){
      
      purpose_data <- purpose_split[[j]]
      
    } else {
      
      purpose_data <- new_df
      
    }
    
    if(variables[[i]] == "gender"){
      
      next
      
    }
    
    # Find all combinations of classifications
    Testing <- do.call(c, lapply(seq_along(variable_levels[[i]]), combn, x = variable_levels[[i]], simplify = FALSE))
    # Remove combinations which are not necessary. i.e a variable level by it's own
    Testing <- Testing[(length(variable_levels[[i]]) + 1): (length(Testing)-1)]
    
    # First combination is always no aggregation
    Testing <- c("", Testing)
    
    # Update the data frame with aggregated data
    results <- lapply(Testing, function(x){
      
      updated_df <- purpose_data %>%
        mutate(!!variables[i] := case_when(
          get(variables[i]) %in% x ~ str_c(x, collapse = " Join "),
          TRUE ~ as.character(get(variables[i]))
        ))
      
      # Build a Negative-bionomial regression model
      nbr_model <- glm.nb(formula = nbr_formula, data = updated_df, subset = train_ind)
      
      # Extract p-values and obtain number of segments, significant segments and aic
      p_values_summary <- nbr_model %>% summary()
      
      p_values <- p_values_summary$coefficients %>% 
        as.data.frame() %>% 
        select("Pr(>|z|)")
      
      p_values <- tibble::rownames_to_column(p_values, "Variables")
      
      p_values <- p_values %>% filter(str_detect(Variables, variables[i]))
      
      p_values_significant <- p_values %>% 
        rename(pvals = "Pr(>|z|)") %>% 
        filter(pvals < 0.05)
      
      significant_number <- nrow(p_values_significant)
      number_variables <- nrow(p_values)
      proportion_significant <- significant_number/number_variables
      aic <- p_values_summary$aic
      
      results <- data.frame(number_segments = number_variables,
                            significant_variables = significant_number,
                            proportion_significant,
                            aic)
      
      list(updated_df, results)
      
    })
    
    # Extract dataframes of aggregated segmente
    results_df <- sapply(results, function(x) x[1])
    
    # Extract our selection criteria results
    results_values <- sapply(results, function(x) x[2])
    
    # Obtain the top row based on selection criteria which is most significant variables
    # I'm hoping to improve this somehow!
    model_select <- results_values %>%
      bind_rows() %>% 
      mutate(n = seq_along(results)) %>%
      select(n,everything()) %>%
      arrange(desc(proportion_significant),desc(significant_variables), desc(number_segments), aic)
    
    # Obtain winning combination
    combination_winner <- model_select %>% 
      slice(1) %>% 
      pull(n)
    
    # Get dataframe of winning combination
    new_df <- results_df %>% extract2(combination_winner)
    
    if (i == length(variables)){
      
      final_df[[j]] <- new_df
      
    }
    
  }
  
  # Build final model
  final_model[[j]] <- glm.nb(formula = nbr_formula, data = final_df[[j]], subset = train_ind)
  
  # Extract new variable levels
  new_levels <- lapply(variables, Extract_levels, final_df[[j]])
  names(new_levels) <- variables
  
  # Calculate combinations of all variables
  new_data <- do.call("crossing",new_levels)
  
  # Predict new trip rates
  tfn_predictions <- predict(final_model[[j]], newdata = new_data, type = "response") %>% as.vector()
  
  output[[j]] <- new_data %>% mutate(tfn_predictions = tfn_predictions)
  
  
}


t1 <- lapply(variables, Extract_levels, final_df[[1]])

names(t1) <- variables
t2 <- do.call("crossing",t1)

predictions <- predict(final_model[[1]], newdata = t2, type = "response") %>% as.vector()

t3 <- t2 %>% mutate(predicted_rates = predictions)




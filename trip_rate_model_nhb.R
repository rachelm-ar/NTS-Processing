# First test this with combination[1:2] and then all combinations
nhb_trip_rates <- function(csv_input, output_dir, ntem_tr_input){
  
  # List of packages required
  packages_list <- c("tidyverse",
                     "sjstats",
                     "MASS",
                     "survey",
                     "combinat",
                     "magrittr",
                     "rlang",
                     "ggpmisc",
                     "ggpubr")
  
  # Packages not installed
  packages_new <- packages_list[!(packages_list %in% installed.packages()[,"Package"])]
  
  # Install packages
  if(length(packages_new)) install.packages(packages_new)
  
  # Load packages
  lapply(packages_list, require, character.only = TRUE)
 
  # Redefine select if masked by MASS
  select <- dplyr::select
  
  # Read in Weekly trip rates
  trip_rates_df <- read_csv(csv_input)
  
  # Filter for required columns and remove unavailable data
  trip_rates_df <- trip_rates_df %>%
    select(nhb_purpose, 
           nhb_purpose_hb_leg,
           main_mode,
           tfn_trip_rate) %>%
    filter(nhb_purpose != 99,
           nhb_purpose_hb_leg != 99,
           main_mode != 99)
  
  # Transform catgeorical variables to factors
  variables <- c("nhb_purpose_hb_leg",
                 "main_mode")
  
  # Extract levels of variables
  Extract_levels <- function(variable_name,df){
    
    df %>%
      pull(variable_name) %>%
      as.factor() %>%
      levels()
    
  }
  variable_levels <- lapply(variables, Extract_levels, trip_rates_df)
  
  # Formula for Negative binomial regression with all variables
  nbr_formula <- paste("tfn_trip_rate", paste(variables, collapse = " + "), sep = " ~ ")
  
  # Convert to factors
  trip_rates_df <- trip_rates_df %>% mutate_at(variables, funs(factor))
  
  # Split data by purpose
  purpose_split_df <- trip_rates_df %>% group_split(nhb_purpose)

  tfn_trip_rates <- list()
  final_df <- list()
  final_model <- list()
 
  for(i in 1:length(purpose_split_df)){
   
    # Split data into 75% train and 25% test
    smp_size <- floor(0.75*nrow(purpose_split_df[[i]]))
    train_ind <<- sample(seq_len(nrow(purpose_split_df[[i]])), size = smp_size)
   
    for(j in 1:length(variables)){
     
      if(j == 1){
       
        purpose_data <- purpose_split_df[[i]]
       
      } else {
       
        purpose_data <- new_df
        
      }
     
      # Find all combinations of classifications
      combinations <- do.call(c, lapply(seq_along(variable_levels[[j]]), combn, x = variable_levels[[j]], simplify = FALSE))
      
      # Remove combinations which are not necessary. i.e a variable level by it's own
      combinations <- combinations[(length(variable_levels[[j]]) + 1): (length(combinations)-1)]
      
      # First combination is always no aggregation
      combinations <- c("", combinations)
      
      # Update the data frame with aggregated data
      mod_results <- lapply(combinations, function(x){
        
        updated_df <- purpose_data %>%
          mutate(!!variables[j] := case_when(
            get(variables[j]) %in% x ~ str_c(x, collapse = " Join "),
            TRUE ~ as.character(get(variables[j]))
          ))
        
        # Build a Negative-bionomial regression model
        nbr_mod <- glm.nb(formula = nbr_formula, data = updated_df, subset = train_ind)
        
        # Extract p-values and obtain number of segments, significant segments and aic
        p_val_summ <- nbr_mod %>% summary()
        
        p_val <- p_val_summ$coefficients %>% 
          as.data.frame() %>% 
          select("Pr(>|z|)") %>%
          tibble::rownames_to_column("Variables") %>%
          filter(str_detect(Variables, variables[j]))
        
        p_val_sig <- p_val %>% 
          rename(pvals = "Pr(>|z|)") %>% 
          filter(pvals < 0.05)
        
        sig_number <- nrow(p_val_sig)
        cat_number <- nrow(p_val)
        prop_sig <- sig_number/cat_number
        aic <- p_val_summ$aic
        
        results <- data.frame(category_number = cat_number,
                              significant_number = sig_number,
                              proportion_significant = prop_sig,
                              aic = aic)
        
        list(updated_df, results)
        
      })
     
     # Extract dataframes of aggregated segmente
     results_df <- sapply(mod_results, function(x) x[1])
     
     # Extract our selection criteria results
     results_values <- sapply(mod_results, function(x) x[2])
     
     # Obtain the top row based on selection criteria which is most significant variables
     # I'm hoping to improve this somehow!
     model_select <- results_values %>%
       bind_rows() %>% 
       mutate(n = seq_along(mod_results)) %>%
       select(n,everything()) %>%
       arrange(desc(proportion_significant),desc(significant_number), desc(category_number), aic)
     
     # Obtain winning combination
     combination_winner <- model_select %>% 
       slice(1) %>% 
       pull(n)
     
     # Get dataframe of winning combination
     new_df <- results_df %>% extract2(combination_winner)
     
     if (j == length(variables)){
       
       final_df[[i]] <- new_df
       
     }
     
     print(paste0("Completed Purpose ", i, " for variable ", j))
     
   }
   
   # Build final model
   final_model[[i]] <- glm.nb(formula = nbr_formula, data = final_df[[i]], subset = train_ind)
   
   # Extract new variable levels
   new_levels <- lapply(variables, Extract_levels, final_df[[i]])
   names(new_levels) <- variables
   
   # Calculate combinations of all variables
   new_data <- do.call("crossing",new_levels)
   
   # Predict new trip rates
   tfn_predictions <- predict(final_model[[i]], newdata = new_data, type = "response") %>% as.vector()
   
   # Add purpose and mode variables and rename to match NTEM
   new_data <- new_data %>% mutate(tfn_predictions = tfn_predictions, 
                                   nhb_purpose = i,
                                   ph = 1)
   
   mode_ph <- tibble(mode = seq_along(new_levels$main_mode)) %>% 
     mutate(ph = 1)
   
   new_data <- new_data %>% 
     left_join(mode_ph) %>% 
     rename(nhb_mode = main_mode,
            purpose = nhb_purpose_hb_leg) %>%
     select(nhb_purpose, nhb_mode, purpose, mode, tfn_predictions)
   
   # Seperate rows which have joined categorical variables
   new_data <- new_data %>%
     separate_rows(purpose, sep = " Join ") %>%
     separate_rows(nhb_mode, sep = " Join ")
   
   tfn_trip_rates[[i]] <- new_data
   
   print(paste0("Completed Purpose ", i))
   
 }
 
 # Combine rows for all trip purposes
 tfn_trip_rates_result <- tfn_trip_rates %>% bind_rows()
 
 # Write CSV [OUTPUT]
 tfn_trip_rates_result %>% write_csv(paste0(output_dir, "nhb_tfn_trip_rates.csv"))
 
}

#### TODO:::::::::::::::::::::::::
#
#### Compare TfN trip rates to NTEM
#
## Obtain same segments as NTEM
#tfn_trip_rates_result <- tfn_trip_rates_result %>% mutate_all(funs(as.numeric))
#
## Read in NTEM trip rates
#ntem_trip_rate <- read_csv(ntem_tr_input)
#
## Join trip rates
#joined_trip_rates <- ntem_trip_rate %>% left_join(tfn_trip_rates_result,
#                                                  by = c("purpose",
#                                                         "traveller_type",
#                                                         "tfn_area_type"))
#
#nhb_trip_rates(csv_input, output_dir, ntem_tr_input)

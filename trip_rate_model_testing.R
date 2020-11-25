# NTS folder
nts_dir <- "Y:/NTS/"

# Imports folder
import_dir <- str_c(nts_dir, "import/")

# Outputs folder
output_dir <- str_c(nts_dir, "outputs/hb_trip_rates/")

# Lookups
lookup_dir <- str_c(nts_dir, "lookups/")

# Lookup functions
source(str_c(lookup_dir,"lookups.r"))

# Classified_nts_trip_rates
hb_csv <- str_c(import_dir, "classified_nts_trip_rates.csv")

# Model forms
model_forms_csv <- str_c(lookup_dir, "model_form---trip_purpose--age_work_status.csv")

Read_packages <- function(packages_list){
  
  "
  Description
  ----------
  - Install packages if not previously installed
  - Load in packages
  
  Parameters
  ----------
  packages_list:
    A vector of packages required to run the main function
  
  Return
  ----------
  A boolean list of TRUE indicating all packages are loaded
  
  "
  
  # Packages not installed
  packages_new <- packages_list[!(packages_list %in% installed.packages()[,"Package"])]
  
  # Install packages
  if(length(packages_new)) install.packages(packages_new)
  
  # Load packages
  lapply(packages_list, require, character.only = TRUE)
  
}

Variable_status <- function(df, variables, nbr_formula){
  
  "
      Description
      ----------
      - Build an initial model
      - Identify variables that have all significant classifications
      
      Parameters
      ----------
      
      df:
        A data frame pre-filtered for a purpose to build model on
        
      Return
      ----------
      
      var_status:
        A vector dictating if the variable can be skipped or classifications require aggregation
      
      "
  
  # Build an initial NBR model
  int_model <- glm.nb(formula = nbr_formula,
                      data = df,
                      subset = train_ind)
  
  int_model_summ <- summary(int_model)
  
  # Extract coefficients of model 
  int_model_vars <- int_model_summ$coefficients %>% 
    as.data.frame() %>%
    select("Pr(>|z|)") %>%
    tibble::rownames_to_column("Variables") %>%
    rename(p_vals = "Pr(>|z|)")
  
  # Identify if variables have all classifications significant
  var_status <- lapply(variables, function(x){
    
    var_pvals <- int_model_vars %>% 
      filter(str_detect(Variables, x)) %>%
      pull(p_vals)
    
    ifelse(all(var_pvals < 0.01) | all(var_pvals > 0.01), "skip", "configure")
    
  })
  
  var_status <- var_status %>% unlist()
  
  list(var_status, int_model_vars)
  
}

library("tidyverse")

hb_trip_rates <- function(hb_csv){
  
  # Read in Weekly trip rates
  hb_df <- read_csv(hb_csv)
  
  # Read in model forms
  model_forms <- read_csv(model_forms_csv)
  
  # explanatory variables
  explanatory_vars <- c("age_work_status",
                        "gender", 
                        "hh_adults", 
                        "cars", 
                        "soc_cat", 
                        "ns_sec",
                        "tfn_area_type")
  
  # Convert explanatory variables to factors
  hb_df <- hb_df %>%
    mutate_at(explanatory_vars, .funs = factor, ordered = is.ordered(explanatory_vars))
  
  # Classifications of each variable
  explanatory_var_levels <- hb_df %>%
    select(all_of(explanatory_vars)) %>%
    sapply(., levels)
  
  # Split data by purpose
  purpose_df <- hb_df %>% group_split(trip_purpose)
  
  tfn_trip_rates <- list()
  final_df <- list()
  final_model <- list()
  
  p <- 1
  
  for(p in 1:length(purpose_df)){
    
    # Add a row indicator
    p_df <- rowid_to_column(purpose_df[[p]], 'id')
    
    # Split data into train and test
    train_df <- sample_frac(p_df, .75)
    test_df <- anti_join(p_df, train_df, by = 'id')
    
    # Choose variables to model depending on purpose
    if(p %in% c(1,2)){
      
      exp_vars <- str_subset(explanatory_vars, 'ns_sec', negate = TRUE)
      
      exp_var_levels <- list_modify(explanatory_var_levels, 'ns_sec' = NULL)
      
    } else {
      
      exp_vars <- str_subset(explanatory_vars, 'soc_cat', negate = TRUE)
      
      exp_var_levels <- list_modify(explanatory_var_levels, 'soc_cat' = NULL)
      
    }
    
    for(v in 1:length(exp_vars)){
      
      m_form <- model_forms %>%
        filter(tp == p, aws == v) %>% 
        pull(model_form)
      
      if(m_form == 'NB'){
        
        model_formula <- as.formula(paste("trip_rate", paste(exp_vars, collapse = ' + '), sep = ' ~ '))
        
      } else if(m_form == 'ZINB'){
        
        model_formula <- 'insert ZINB model formula here'
        
      } else if(m_form == 'ZIP'){
        
        model_formula <- 'insert ZIP model formula here'
        
      }
      
    MASS::glm.nb(formula = model_formula,
                 data = train_df)
        
      
    }
    
    explanatory_var_levels[["age_work_status"]]
    
    # Formula to use depending on whether we want soc_cat or ns_sec
    nbr_formula <- as.formula(paste("trip_rate", paste(exp_vars, collapse = " + "), sep = " ~ "))
    
  
    
  }
  
  
  
  
  
  
}


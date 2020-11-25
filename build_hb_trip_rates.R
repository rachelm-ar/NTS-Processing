library("tidyverse")
library("MASS")


# Initialise Directories --------------------------------------------------

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
hb_csv <- str_c(import_dir, "classified_nts_child_genderless.csv")


# Sub functions -----------------------------------------------------------

train_indicator <- function(df, s_size){
  
  sample_size <- floor(0.75*nrow(df))
  
  train_ind <- df %>%
    nrow() %>%
    seq_len() %>%
    sample(., size = sample_size)
  
  return(train_ind)
  
}

get_covars <- function(tp, aws, covars){
  
  if(tp %in% 1:2 & aws == 1) {
    
    covariates <- str_subset(covars, 'gender|ns_sec|soc_cat', negate = TRUE)
    
  } else if(!tp %in% 1:2 & aws == 1) {
    
    covariates <- str_subset(covars, 'gender|soc_cat|ns_sec', negate = TRUE)
    
  } else if(tp %in% 1:2 & aws != 1){
    
    covariates <- str_subset(covars, 'ns_sec', negate = TRUE)
    
  } else if(!tp %in% 1:2 & aws != 1){
    
    covariates <- str_subset(covars, 'soc_cat', negate = TRUE)
    
  }
  
  return(list(covariates))
  
}

extract_levels <- function(df, covariate){
  
  df %>%
    select(all_of(covariate)) %>%
    sapply(levels)
  
}

select <- dplyr::select

# Main function -----------------------------------------------------------

hb_trip_rates <- function(hb_csv){
  
  # Read in Weekly trip rates
  hb_df <- read_csv(hb_csv) 
  
  # Exploratory variables
  covars <- c("gender", 
              "hh_adults", 
              "cars", 
              "soc_cat", 
              "ns_sec",
              "tfn_area_type")
  
  nest_vars <- c("trip_purpose", "age_work_status")
  
  covars_all <- c(nest_vars, covars)
  
  res_var <- "trip_rate"
  
  all_vars <- c(res_var, covars_all)
  
  models_split <- hb_df %>%
    group_nest(trip_purpose, age_work_status, .key = 'p_df')

  models_split1 <- models_split %>%
    mutate(covariates = map2(trip_purpose, age_work_status, get_covars, covars) %>% unlist(recursive = FALSE),
           p_df = map2(p_df, covariates, function(x,y) x %>% mutate_at(y, .funs = factor)),
           covar_levels = map2(p_df, covariates, extract_levels),
           regression_formula = map(covariates, function(x) as.formula(paste(res_var, paste(x, collapse = " + "), sep = " ~ "))),
           nbr = map2(regression_formula, p_df, function(x,y) glm.nb(formula = x, data = y)),
           nbr_summary = map(nbr, summary)
    )
  
  models_split2 <- models_split1 %>% 
    mutate(new_data = map(covar_levels, function(x) do.call('crossing', x)),
           trip_rates = map2(nbr, new_data, function(x,y) predict(x, newdata = y, type = 'response') %>% as.vector()),
           new_data = map2(new_data, trip_rates, function(x,y) x %>% mutate(trip_rates = y)))
    
  
  add_nested_data <- function(df, tp, aws){
    
    if(tp %in% 1:2 & aws == 1){
      
      df %>%
        mutate(soc_cat = 0,
               ns_sec = 'none',
               gender = 2,
               age_work_status = aws,
               trip_purpose = tp)
      
    } else if (tp %in% 3:8 & aws == 1){
      
      df %>%
        mutate(soc_cat = 'none',
               ns_sec = 1,
               gender = 2,
               age_work_status = aws,
               trip_purpose = tp) %>%
        complete(nesting(hh_adults, cars, tfn_area_type, soc_cat, gender, age_work_status, trip_purpose),
                 ns_sec = 1:5) %>%
        fill(trip_rates)
      
    } else if (tp %in% 3:8 & aws == 3){
      
      df %>%
        filter(ns_sec != 99) %>%
        mutate(soc_cat = 'none',
               age_work_status = aws,
               trip_purpose = tp,
               ns_sec = as.integer(ns_sec)) %>% 
        complete(nesting(hh_adults, cars, tfn_area_type, soc_cat, gender, age_work_status, trip_purpose),
                 ns_sec = 1:5) %>%
        group_by(hh_adults, cars, tfn_area_type, soc_cat, gender, age_work_status, trip_purpose) %>%
        mutate(trip_rates = ifelse(is.na(trip_rates), mean(trip_rates, na.rm = TRUE), trip_rates),
               ns_sec = as.character(ns_sec))
      
    } else if (tp %in% 1:2 & aws != 1){
      
      df %>%
        mutate(soc_cat = ifelse(soc_cat == 99, 0, soc_cat),
               ns_sec = 'none',
               age_work_status = aws,
               trip_purpose = tp)
      
    } else if (tp %in% 3:8 & aws %in% c(2,4,5,6)){
      
      df %>%
        filter(ns_sec != 99) %>%
        mutate(soc_cat = 'none',
               age_work_status = aws,
               trip_purpose = tp) 
      
    }
    
  }


  try1 <- models_split3 %>%
    filter(trip_purpose == 3, age_work_status == 2) %>% 
    pull(new_data) %>% flatten_df
  
  try1 %>%
    count(ns_sec)
  
  models_split3 <- models_split2 %>%
    mutate(new_data = pmap(list(new_data, trip_purpose, age_work_status), add_nested_data))
  
  models_split3
  
  add_traveller_types <- function(df){
    
    df %>% 
      ungroup() %>% 
      mutate_at(covars_all, .funs = as.character) %>% 
      unite("traveller_type_char", "age_work_status", "gender", "hh_adults", "cars", remove=FALSE, sep="_") %>%
      lu_traveller_type() %>%
      na.omit()
    
  }
  
  sp <- models_split3 %>%
    mutate(final_df = map(new_data, add_traveller_types))
  
  trythis <- sp %>%
    select(final_df) %>%
    unnest(cols = c(final_df)) %>%
    rename(p = trip_purpose,
           soc = soc_cat,
           ns = ns_sec,
           area_type = tfn_area_type,
           trip_rate = trip_rates) %>%
    select(p, traveller_type, soc, ns, area_type, trip_rate) %>%
    arrange(p, traveller_type, soc, ns, area_type)
    
  trythis %>% write_csv('Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates_HA.csv')
  
  
  by_purpose1 <- by_purpose %>%
    mutate(train_df = map(p_df, function(x) x %>% sample_frac(.75)),
           test_df = map2(p_df, train_df, function(x, y) x %>% anti_join(y, by = 'id')),
           covars = ifelse(
             trip_purpose %in% 1:2,
             str_subset(variables, "ns_sec", negate = TRUE) %>% list(),
             str_subset(variables, "soc_cat", negate = TRUE) %>% list()
           ),
           covar_levels = ifelse(
             trip_purpose %in% 1:2,
             list_modify(variable_levels, "ns_sec" = NULL) %>% list(),
             list_modify(variable_levels, "soc_cat" = NULL) %>% list()
           ),
           nbr_formula = map(covars, function(x) as.formula(paste("trip_rate", paste(x, collapse = " + "), sep = " ~ ")))
           )
  

  t1 <- by_purpose1 %>%
    mutate(var_status = pmap(.l = list(df = train_df, 
                                       nbr_formula = nbr_formula,
                                       covar = covars
                                       ), variable_status)) %>%
    unnest_wider(var_status)

  t1 %>%
    unnest(c(covars, var_status, covar_levels)) %>%
    filter(var_status == "configure")
  
}


# Classifications of each variable
variable_levels <- hb_df %>%
  select(all_of(variables)) %>%
  sapply(levels)

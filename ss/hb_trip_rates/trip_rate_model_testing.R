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

=======
# Simplified Trip Rate model testing
library(tidyverse)
library(MASS)

hb_csv <- read_csv("Y:/NTS/import/classified_nts_trip_rates.csv")

model_forms <- read_csv("Y:/NTS/TfN_Trip_Rates - HA to Audit/model_forms.csv")

tt_lu <- read_csv("Y:/NTS/lookups/traveller_type---age_work_status--gender--hh_adults--cars.csv")

model_forms <- transpose(model_forms)

all_vars <- list(trip_purpose = 1:8,
                 age_work_status = 1:6,
                 hh_adults = 1:3,
                 gender = 0:2,
                 cars = 1:4,
                 tfn_area_type = 1:8,
                 soc_cat = c(1:3, 99),
                 ns_sec = c(1:5, 99))
all_predictors <- all_vars[c(-1,-2)]

hb_csv <- hb_csv %>% 
  dplyr::select(age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, trip_purpose, weekly_trips, trip_rate) %>% 
  mutate(gender = factor(gender, levels = 0:2),
         hh_adults = factor(hh_adults, levels = 1:3),
         cars = factor(cars, levels = 1:4),
         soc_cat = factor(soc_cat, levels = c(1:3, 99)),
         ns_sec = factor(ns_sec, levels = c(1:5, 99)),
         tfn_area_type = factor(tfn_area_type, levels = 1:8))

as.formula(paste("trip_rate", paste(vars, collapse = " + "), sep = " ~ "))

df_trip_rates <- replicate(8, vector("list", 6), simplify = FALSE)

for (trp in 1:length(all_vars$trip_purpose)) {
  
  for (aws in 1:length(all_vars$age_work_status)) {
    
    final_predictors <- c()
  
    cur_df <- filter(hb_csv, trip_purpose == trp, age_work_status == aws) 
    
    if(aws == 1){
      
      # Build model
      mod <- glm.nb(formula = trip_rate ~ hh_adults + cars + tfn_area_type,
                    data = cur_df)
      
    } else if (trp %in% 1:2){
      
      # Build model
      mod <- glm.nb(formula = trip_rate ~ hh_adults + cars + tfn_area_type + soc_cat + gender,
                    data = cur_df)
      
      
    } else if (trp %in% 3:8){
      
      mod <- glm.nb(formula = trip_rate ~ hh_adults + cars + tfn_area_type + ns_sec + gender,
                    data = cur_df)
      
    }
    
    new_data <- mod$xlevels %>% 
      cross() %>% 
      bind_rows() %>% 
      mutate_all(factor)
    
    final_predictions <- predict.glm(mod, newdata = new_data, type = "response")
    new_data <- new_data %>% 
      mutate(trip_rates = final_predictions,
             trip_purpose = as.factor(trp),
             age_work_status = as.factor(aws))
    
    if(trp %in% 1:2 & aws == 1){
      
      final_predictors <- c(final_predictors, "gender", "ns_sec", "soc_cat")
      missing_vars <- all_predictors[!names(all_predictors) %in% final_predictors]
      
       df_trip_rates[[trp]][[aws]] <- new_data %>%
        mutate(soc_cat = factor(0),
               ns_sec = "none",
               gender = factor(2)) 
      
      
    } else if (trp %in% 3:8 & aws == 1){
      
      final_predictors <- c(final_predictors, "gender", "ns_sec", "soc_cat")
      missing_vars <- all_predictors[!names(all_predictors) %in% final_predictors]
      
      df_trip_rates[[trp]][[aws]] <- new_data %>%
        mutate(soc_cat = factor("none"),
               gender = factor(2)) %>% 
        crossing(tibble(ns_sec = 1:5)) %>% 
        mutate(ns_sec = factor(ns_sec))
      
    } else if (trp %in% 1:2 & aws != 1){
      
      df_trip_rates[[trp]][[aws]] <- new_data %>% 
        mutate(ns_sec = factor("none"))
    
    } else if (trp %in% 3:8 & aws != 1){
      
      #NS SEC == 4 needs to be added back in
      new_data <- new_data %>%
        filter(ns_sec != 99) %>% 
        mutate(soc_cat = factor("none"))
      
      ns_sec4 <- new_data %>% 
        group_by(hh_adults, cars, tfn_area_type, gender, trip_purpose, age_work_status, soc_cat) %>% 
        summarise(trip_rates = mean(trip_rates)) %>% 
        ungroup() %>% 
        mutate(ns_sec = factor(4))
      
      df_trip_rates[[trp]][[aws]] <- new_data %>% 
        bind_rows(ns_sec4) %>% 
        mutate(ns_sec = factor(ns_sec))
      
    }
    
  }
  
}

df_trip_rates <- df_trip_rates %>% 
  unlist(recursive = FALSE) %>% 
  bind_rows()

tt_trip_rates <- df_trip_rates %>% 
  mutate_at(vars(hh_adults, cars, age_work_status, gender), as.double) %>%  
  left_join(tt_lu, by = c("hh_adults", "cars", "age_work_status", "gender")) %>% 
  na.omit()

tt_trip_rates <- tt_trip_rates %>% 
  group_by(trip_purpose, tt, tfn_area_type, soc_cat, ns_sec) %>%
  summarise(trip_rates = mean(trip_rates)) %>% 
  ungroup()

trip_rates_out <- tt_trip_rates %>% 
  rename(p = trip_purpose,
         traveller_type = tt,
         soc = soc_cat,
         ns = ns_sec,
         trip_rate = trip_rates,
         area_type = tfn_area_type) %>% 
  dplyr::select(p, traveller_type, soc, ns, area_type, trip_rate)

write_csv(trip_rates_out, "C:/Users/Pluto/Documents/trip_rate_testing.csv")


# Topline report ----------------------------------------------------------



# Read in trip rates, land use, time splits, mode splits and NTEM Control
trip_rates_out <- read_csv("C:/Users/Pluto/Documents/hb_trip_rates_v2.5.csv")
tp_split <- read_csv('C:/Users/Pluto/Documents/Trip_rate_testing/hb_time_split.csv')
mode_split <- read_csv('C:/Users/Pluto/Documents/Trip_rate_testing/hb_mode_split.csv')
ntem_control <- read_csv('C:/Users/Pluto/Documents/Trip_rate_testing/ntem_control.csv')

land_use <- read_csv('Y:/NorMITs Land Use/iter3/land_use_output_msoa.csv',
                     col_types = cols_only('msoa_zone_id' = col_character(),
                                           'area_type' = col_double(),
                                           'cars' = col_character(),
                                           'traveller_type' = col_double(),
                                           'soc' = col_character(),
                                           'ns' = col_character(),
                                           'people' = col_double()))

# Pre process land use and trip rates preparing for merge
p_join <- tibble(p = rep(1, 8),
                 p2 = seq(1,8))

land_use <- land_use %>% 
  mutate(ca = ifelse(cars == "0", 1, 2)) %>% 
  select(msoa_zone_id, area_type, ca, traveller_type, soc, ns, people) %>% 
  group_by(msoa_zone_id, area_type, ca, traveller_type, soc, ns) %>% 
  summarise(people = sum(people)) %>% 
  ungroup() %>% 
  mutate(p = 1) %>% 
  left_join(p_join, by = "p") %>% 
  select(-p) %>%
  rename(p = p2) %>%
  mutate(soc = ifelse(p %in% 1:2, as.character(as.integer(soc)), "none"),
         ns = ifelse(p %in% 1:2, "none", as.character(as.integer(ns))))

trip_rates <- trip_rates_out %>% 
  mutate(soc = ifelse(p %in% 1:2, as.character(soc), "none"),
         soc = ifelse(p %in% 1:2 & soc == "99", "0", as.character(soc)))

productions <- land_use %>% 
  left_join(trip_rates, by = c("traveller_type",
                               "p",
                               "soc",
                               "ns",
                               "area_type")) %>% 
  left_join(tp_split, by = c("area_type",
                           "traveller_type",
                           "p")) %>% 
  left_join(mode_split, by = c("area_type",
                               "ca",
                               "p")) %>% 
  select(p, people, trip_rate, tp1:tp4, m1:m6) %>% 
  na.omit()

tps <- c('tp1', 'tp2', 'tp3', 'tp4')
mds <- c('m1', 'm2', 'm3', 'm5', 'm6')
cross_vars <- crossing(tps, mds)

rm(land_use, trip_rates, tp_split, mode_split)

calculate_t_m_splits <- function(df, tps, mds){
  
  df %>% 
    transmute(!!paste(tps, mds, sep = "_") := !!sym(tps) * !!sym(mds) * people * trip_rate / 5)
  
}

productions_all <- productions %>% 
  bind_cols(map2_dfc(cross_vars$tps, cross_vars$mds, calculate_t_m_splits, df = productions)) %>% 
  mutate(m1 = tp1_m1 + tp2_m1 + tp3_m1 + tp4_m1,
         m2 = tp1_m2 + tp2_m2 + tp3_m2 + tp4_m2,
         m3 = tp1_m3 + tp2_m3 + tp3_m3 + tp4_m3,
         m5 = tp1_m5 + tp2_m5 + tp3_m5 + tp4_m5,
         m6 = tp1_m6 + tp2_m6 + tp3_m6 + tp4_m6) %>% 
  select(p, m1:m6) %>% 
  group_by(p) %>% 
  summarise_at(vars(m1:m6), list(~ sum(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(all = m1 + m2 + m3 + m5 + m6,
         p = as.character(p))

report_output <- productions_all %>%
  summarise_at(vars(m1:all), sum) %>% 
  mutate(p = "all") %>% 
  bind_rows(productions_all) %>% 
  arrange(p) %>% 
  select(p, everything())

write_csv(report_output, "C:/Users/Pluto/Documents/Trip_rate_testing/tfn_production_topline.csv")
>>>>>>> Stashed changes

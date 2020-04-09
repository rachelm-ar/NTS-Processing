working_dir <- "Y:/NTS/"
output_dir <- "Y:/NTS/TfN_Trip_Rates/"
trip_type <- "hb"
input_csv <- "hb_weekly_trip_rates.csv"

Build_Trip_Rates <- function(working_dir, output_dir, trip_type, input_csv){
  
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
  trip_rates_df <- read_csv(paste0(working_dir,input_csv))
  
  # Select necessary variables based on hb and nhb
  if(trip_type == "hb"){
    
    trip_rates_df <- trip_rates_df %>% mutate(purpose = hb_purpose)
    
    variables <- c("age_work_status",
                   "gender",
                   "hh_adults",
                   "cars",
                   "soc_cat",
                   "ns_sec",
                   "tfn_area_type")
    
  } else if(trip_type == "nhb"){
    
    trip_rates_df <- trip_rates_df %>% mutate(purpose = nhb_purpose)
    
    #TODO Add main_mode variable
    variables <- c("age_work_status",
                   "gender",
                   "hh_adults",
                   "cars",
                   "soc_cat",
                   "ns_sec",
                   "nhb_purpose_leg",
                   "tfn_area_type")
    
  } else {
    
    stop("Trip type must be either 'hb' or 'nhb'")
    
  }
  
  # Remove SOC = -8 and keep ns_sec = -9 only for children
  trip_rates_df <- trip_rates_df %>%
    select(one_of(variables),
           purpose,
           tfn_trip_rate) %>%
    filter(purpose != 99,
           soc_cat %in% c(1,2,3,-9),
           (ns_sec == -9 & age_work_status == "0-16_child")|(ns_sec != -9))
  
  trip_rates_df <- trip_rates_df %>% mutate_at(variables, funs(factor))
  
  # Extract levels of variables
  Extract_Levels <- function(variable_name,df){
    
    df %>%
      pull(variable_name) %>%
      as.factor() %>%
      levels()
    
  }
  
  variable_levels <- lapply(variables, Extract_Levels, trip_rates_df)
  
  # Formula for Negative binomial regression with all variables
  nbr_formula <- paste("tfn_trip_rate", paste(variables, collapse = " + "), sep = " ~ ")
  
  trip_rates_df <- trip_rates_df %>% sample_n(2500)
  
  # Split data by purpose
  purpose_split_df <- trip_rates_df %>% group_split(purpose)
  
  # Initialise variables to store results in
  tfn_trip_rates <- list()
  final_df <- list()
  final_model <- list()
  
  a <- Sys.time()
  
  # Build a model for each trip purpose
  for(i in 1:length(purpose_split_df)){
    
    # Split data into 75% train and 25% test
    smp_size <- floor(0.75*nrow(purpose_split_df[[i]]))
    train_ind <- sample(seq_len(nrow(purpose_split_df[[i]])), size = smp_size)
    
    # Loop through all variables - calcuating combinations of classifications to aggregate
    for(j in 1:length(variables)){
      
      # Updating the data frame based on previous iteration
      if(j == 1){
        
        purpose_df <- purpose_split_df[[i]]
        
      } else {
        
        purpose_df <- new_df
        
      }
      
      # Skip gender - only two classifications
      if(variables[[j]] == "gender"){
        
        next
        
      }
      
      # Find all combinations of classifications
      combinations <- do.call(c, lapply(seq_along(variable_levels[[j]]), combn, x = variable_levels[[j]], simplify = FALSE))
      # Remove combinations which are not necessary. i.e a variable level by it's own
      combinations <- combinations[(length(variable_levels[[j]]) + 1): (length(combinations)-1)]
      
      # First combination is always no aggregation
      combinations <- c("", combinations)
      
      # Loop through all combinations - build model and extract outputs
      mod_results <- lapply(combinations, function(x){
        
        # Update the data frame with aggregated data
        updated_df <- purpose_df %>%
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
      
      # Extract selection criteria results
      results_values <- sapply(mod_results, function(x) x[2])
      
      # Obtain the top row based on selection criteria - highest proportion of significant variables
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
      
      # Store results after all variables have been iterated over
      if (j == length(variables)){
        
        final_df[[i]] <- new_df
        
      }
      
    }
    
    # Build final model
    final_model[[i]] <- glm.nb(formula = nbr_formula, data = final_df[[i]], subset = train_ind)
    
    # Extract new variable levels
    new_levels <- lapply(variables, Extract_Levels, final_df[[i]])
    names(new_levels) <- variables
    
    # Calculate combinations of all variables
    new_data <- do.call("crossing",new_levels)
    
    # Predict new trip rates
    tfn_predictions <- predict(final_model[[i]], newdata = new_data, type = "response") %>% as.vector()
    
    # Add new predictions and seperate any joins
    new_data <- new_data %>%
      mutate(tfn_predictions = tfn_predictions) %>% 
      separate_rows(hh_adults, sep = " Join ") %>%
      separate_rows(gender, sep = " Join ") %>%
      separate_rows(age_work_status, sep = " Join ") %>% 
      separate_rows(cars, sep = " Join ") %>%
      separate_rows(soc_cat, sep = " Join ") %>%
      separate_rows(ns_sec, sep = " Join ") %>%
      separate_rows(tfn_area_type, sep = " Join ")
    
    # Build traveller type list:
    aws <- c(rep("0-16_child"                  , 8),
             rep("16-74_fte"                   , 8),
             rep("16-74_pte"                   , 8),
             rep("16-74_stu"                   , 8),
             rep("16-74_unm"                   , 8),
             rep("75\\+_retired|75\\+_pte|75\\+_fte" , 8),
             rep("16-74_fte"                   , 8),
             rep("16-74_pte"                   , 8),
             rep("16-74_stu"                   , 8),
             rep("16-74_unm"                   , 8),
             rep("75\\+_retired|75\\+_pte|75\\+_fte" , 8))
    
    gndr <- c(rep("Male|Female"                , 8),
              rep("Male"                       , 40),
              rep("Female"                     , 40))
    
    crs <- rep(c("0", "1+", "0", "1", "2+", "0", "1", "2+"), 11)
    
    hha <- rep(c("1","1","2","2","2","3+","3+","3+"), 11)
    
    traveller_type_list <- list(age_work_status = aws,
                                hh_adults = hha,
                                cars = crs,
                                gender = gndr)
    
    traveller_types_unlist <- traveller_type_list %>%
      transpose() %>%
      map(flatten_chr) %>%
      lapply(as.list)
    
    # Extracts weights to apply to 75+ to replicate NTEM classifications
    weights75plus <- trip_rates_df %>% 
      filter(str_detect(age_work_status, "75\\+_retired|75\\+_fte|75\\+_pte")) %>%
      group_by(age_work_status) %>% 
      count()
    
    total75 <- weights75plus %>% pull(n) %>% sum()
    fte75 <- weights75plus %>% filter(age_work_status == "75+_fte") %>% pull(n)
    fte75 <- fte75/total75
    pte75 <- weights75plus %>% filter(age_work_status == "75+_pte") %>% pull(n)
    pte75 <- pte75/total75
    rte75 <- weights75plus %>% filter(age_work_status == "75+_retired") %>% pull(n)
    rte75 <- rte75/total75
    
    TT_df <- list()
    
    for (k in 1:length(traveller_types_unlist)){
      
      x <- traveller_types_unlist[[k]]
      
      if(x$age_work_status == "0-16_child"){
        
        # TT 1 to 8
        TT_df[[k]] <- new_data %>%
          filter(str_detect(hh_adults, x$hh_adults),
                 str_detect(age_work_status, x$age_work_status),
                 str_detect(gender, x$gender),
                 str_detect(cars, x$cars)) %>%
          mutate(traveller_type = k,
                 purpose = i) %>%
          group_by(purpose, hh_adults, age_work_status, cars, soc_cat, ns_sec, tfn_area_type, traveller_type) %>%
          summarise(tfn_predictions = mean(tfn_predictions, na.rm = TRUE)) %>%
          ungroup() %>%
          select(purpose, traveller_type, tfn_area_type, soc_cat, ns_sec, tfn_predictions)
        
      } else if (x$age_work_status == "75\\+_retired|75\\+_pte|75\\+_fte"){
        
        #TT 41-48 and 81-88
        TT_df[[k]] <- new_data %>%
          filter(str_detect(hh_adults, x$hh_adults),
                 str_detect(age_work_status, x$age_work_status),
                 str_detect(gender, x$gender),
                 str_detect(cars, x$cars)) %>%
          mutate(traveller_type = k,
                 purpose = i,
                 weights75 = ifelse(age_work_status == "75+_fte", fte75,
                                    ifelse(age_work_status == "75+_pte", pte75, rte75))) %>%
          group_by(purpose,traveller_type, hh_adults, gender, cars, soc_cat, ns_sec, tfn_area_type) %>%
          summarise(tfn_predictions = weighted.mean(tfn_predictions, w = weights75, na.rm = TRUE)) %>%
          ungroup() %>%
          select(purpose, traveller_type, tfn_area_type, soc_cat, ns_sec, tfn_predictions)
        
      } else {
        
        # TT not child or 75+
        TT_df[[k]] <- new_data %>%
          filter(str_detect(hh_adults, x$hh_adults),
                 str_detect(age_work_status, x$age_work_status),
                 str_detect(gender, x$gender),
                 str_detect(cars, x$cars)) %>%
          mutate(traveller_type = k,
                 purpose = i) %>%
          select(purpose, traveller_type, tfn_area_type, soc_cat, ns_sec, tfn_predictions)
      }
      
    }
    
    # Compile all traveller types
    tfn_trip_rates[[i]] <- TT_df %>% bind_rows()
    
  }
  
  # Compile all combinations of trip rates
  tfn_trip_rates_result <- tfn_trip_rates %>%
    bind_rows() %>%
    group_by(purpose, traveller_type, soc_cat, ns_sec, tfn_area_type) %>%
    summarise(tfn_predictions = mean(tfn_predictions))
  
  # Save 
  TfN_trip_rates_result %>% write_csv(paste0(output_dir,trip_type,"_TfN_trip_rates.csv"))
  
}

b <- Sys.time()
b-a

# Obtain same segments as NTEM for TfN trip rates
tfn_ntem_replication <- tfn_trip_rates_result %>%
  group_by(purpose, traveller_type, tfn_area_type) %>%
  summarise(trip_rate = mean(tfn_predictions, na.rm = TRUE)) %>%
  mutate_all(funs(as.numeric))

# Read in NTEM trip rates
ntem_trip_rate <- read_csv("Y:/NorMITs Synthesiser/import/ntem_trip_rates_2016.csv")

# Join trip rates
joined_trip_rates <- ntem_trip_rate %>% left_join(tfn_ntem_replication,
                                                  by = c("purpose",
                                                         "traveller_type",
                                                         "tfn_area_type"))

# Plot of Ntem trip rates against TfN trip rates with regression line
ggplot(data = joined_trip_rates, aes(x = trip_rate.x, y = trip_rate.y)) +
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
  labs(x = "NTEM trip rate", y = "TfN trip rate", title = "NTEM vs TfN Trip Rates") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = round(seq(min(joined_trip_rates$trip_rate.y), max(joined_trip_rates$trip_rate.y), by = 1),1)) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)), parse = TRUE)      

# Plots of Ntem trip rates against TfN trip rates split by purpose
Joined_trip_split <- joined_trip_rates %>% group_split(purpose)

Comparison_Plot <- function(df){

  ggplot(data = df, aes(x=trip_rate.x, y=trip_rate.y)) +
    geom_point() + 
    geom_smooth(method = "lm", se=FALSE, color="red", formula=y~x) +
    labs(x = "NTEM trip rate", y = "TfN trip rate", title = paste0("NTEM vs TfN Trip Rates for purpose ", parent.frame()$i[])) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_y_continuous(breaks = round(seq(min(df$trip_rate.y), max(df$trip_rate.y), by = 1),1)) +
    stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)), parse = TRUE)  
}  

do.call(grid.arrange, c(lapply(Joined_trip_split, Comparison_Plot), nrow=4, ncol=2))

joined_traveller_split <- joined_trip_rates %>% group_split(traveller_type)

traveller_type_correlations <- sapply(joined_traveller_split, function(x){
  
  cor(x$trip_rate.x, x$trip_rate.y)
  
})

traveller_type_correlations %>% mean()

# Outliers
joined_trip_rates 
plot(joined_trip_rates$trip_rate.x, joined_trip_rates$trip_rate.y)
regression <- lm(joined_trip_rates$trip_rate.x ~ joined_trip_rates$trip_rate.y)
abline(regression)
joined_trip_rates <- joined_trip_rates %>% mutate(residuals = residuals(regression))
outlier_threshold <- 0.3

# Print only names of outliers
outliers <- joined_trip_rates[ abs(joined_trip_rates$residuals) > outlier_threshold, ]

###### Differentiation in SOC and NS-sec for working age people

### SOC

# Comparison of SOC for working people - Jack's parameters
Filtered_soc <- TfN_trip_rates_result %>%
  filter(soc_cat %in% c(1,2,3),
         purpose %in% c(1,2,3,4,5,6,7,7,8),
         traveller_type %in% c(seq(9,24,1), seq(49,64,1))) %>%
  group_by(purpose, soc_cat) %>%
  summarise(tfn_predictions = mean(tfn_predictions, na.rm = TRUE))

# Comparison of NS-SEC for working people - Jack's parameters

Filtered_sec <- TfN_trip_rates_result %>%
  filter(ns_sec != -9,
         traveller_type %in% c(seq(9,24,1), seq(49,64,1))) %>%
  group_by(purpose, ns_sec) %>%
  summarise(tfn_predictions = mean(tfn_predictions, na.rm = TRUE)) %>%
  pull(tfn_predictions)

### Jack's Neelum Parameters

# Filter for soc categories of interest and working age people
Filtered_soc <- TfN_trip_rates_result %>% 
  filter(soc_cat %in% c(1,2,3),
         purpose %in% c(1,2),
         traveller_type %in% c(seq(9,18,1), seq(49,64,1))) %>%
  group_by(purpose, soc_cat) %>%
  summarise(tfn_predictions = mean(tfn_predictions, na.rm = TRUE)) %>% 

# Filter for ns sec categories of interest and all not working people
Filter_sec <- TfN_trip_rates_result %>%
  filter(ns_sec != -9,
         traveller_type %in% c(seq(9,24,1), seq(49,64,1)),
         !(purpose %in% c(1,2))) %>%
  group_by(ns_sec) %>%
  summarise(mean = mean(tfn_predictions, na.rm = TRUE)) %>% t()

trip_rates_df %>%
  filter(nhb_purpose == 1, 
         soc_cat == 3, 
         age_work_status %in% c("16-74_fte","16-74_pte")) %>%
  pull(tfn_trip_rate) %>% hist(main = "NS SEC 3")



hb_trips <- read_csv("Y:/NTS/hb_weekly_trip_rates.csv")
nhb_trips <- read_csv("Y:/NTS/nhb_weekly_trip_rates.csv")

nhb_trips %>% View()

hb_trips %>% select(tfn_area_type) %>% distinct()

nhb_trips %>% colnames()

a <- Sys.time()
glm.nb(tfn_trip_rate ~ nhb_purpose + 
         nhb_purpose_hb_leg +
         age_work_status + 
         gender + 
         hh_adults +
         cars +
         soc_cat +
         ns_sec +
         tfn_area_type,
       data = nhb_trips)
b <- Sys.time()
b-a

glm.nb

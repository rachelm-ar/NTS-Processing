library(tidyverse)
library(caret)
library(modelr)
library(ggpmisc)
library(glmnet)

select <- dplyr::select

# Inputs ------------------------------------------------------------------

# Land Use
land_use <- read_csv("I:/NorMITs Land Use/base_land_use/iter3e/outputs/land_use_2018_pop.csv")

# MSOA to GOR
msoa_to_gor <- read_csv("I:/Data/Zone Translations/msoa_to_gor_correspondence.csv")

# NTS
nts <- read_csv("I:/NTS/classified builds/cb_tfn.csv")

# 2 digit sic codes
sic_jobs <- read_csv("I:/Data/Attractions Analysis/hsl_2digit_sic_msoa_2018.csv")

# TEMPRO productions
tempro <- read_csv("I:/Data/TEMPRO/tempro_pa_data_ave_week.csv")

# MSOA to UA
msoa_to_ua <- read_csv("Y:/Data Strategy/GIS Shapefiles/UA 1998/msoa_ua1998_lookup.csv")

# MSOA to LA
msoa_to_la <- read_csv("Y:/NTS/lad_2020.csv")
msoa_to_la2 <- read_csv("Y:/NTS/lads20_to_msoa_correspondence.csv")

# Preprocessing Inputs ----------------------------------------------------

### Lookups

# msoa to gor
msoa_to_gor <- msoa_to_gor %>%
  select(msoa_zone_id, gor)

# UA to GOR
ua_to_gor <- msoa_to_gor %>%
  left_join(msoa_to_ua) %>% 
  select(ua_1998_zone_id, gor) %>% 
  distinct()

# msoa to la
msoa_to_la <- msoa_to_la %>%
  select(LAD20CD, LAD20NM)

msoa_to_la3 <- msoa_to_la %>% 
  left_join(msoa_to_la2, by = c("LAD20CD" = "lads20_zone_id")) %>%
  filter(!is.na(msoa_to_lads20)) %>%
  select(msoa_zone_id, LAD20CD, LAD20NM)

### SIC Data

sic_jobs <- rename(sic_jobs, msoa_zone_id = geo_code)

### Land Use

land_use <- land_use %>%
  select(msoa_zone_id, people)

### NTS

# Align GOR categories with Land Use (i.e. use names)
nts <- nts %>%
  mutate(gor = case_when(
    TripDestGOR_B02ID == 1 ~ "north east",
    TripDestGOR_B02ID == 2 ~ "north west",
    TripDestGOR_B02ID == 3 ~ "yorkshire & the humber",
    TripDestGOR_B02ID == 4 ~ "east midlands",
    TripDestGOR_B02ID == 5 ~ "west midlands",
    TripDestGOR_B02ID == 6 ~ "east of england",
    TripDestGOR_B02ID == 7 ~ "london",
    TripDestGOR_B02ID == 8 ~ "south east",
    TripDestGOR_B02ID == 9 ~ "south west",
    TripDestGOR_B02ID == 10 ~ "wales",
    TripDestGOR_B02ID == 11 ~ "scotland"))

# Aggregate car/van and light/heavy rail
nts <- nts %>%
  mutate(main_mode = ifelse(main_mode == 4, 3, main_mode),
         main_mode = ifelse(main_mode == 7, 6, main_mode)) %>%
  filter(main_mode != 8)

# Derive population -------------------------------------------------------

# Calculate population by GOR
pop_by_gor <- land_use %>% 
  left_join(msoa_to_gor) %>% 
  group_by(gor) %>%
  summarise(people = sum(people)) %>% 
  ungroup() %>% 
  filter(gor != "#N/A")

# Calculate expansion factor at GOR level ---------------------------------

# nts individual sample for individuals with atleast one hb trip
nts_sample_stats <- nts %>% 
  filter(p %in% 1:8) %>% 
  distinct(IndividualID, gor) %>% 
  count(gor) %>%
  rename(people_sample = n) %>% 
  filter(!is.na(gor))

gor_stats <- left_join(nts_sample_stats, pop_by_gor)

gor_expand_factors <- gor_stats %>%
  mutate(expansion_factor = people/people_sample) %>% 
  select(gor, expansion_factor)

# Data merging and processing for model building --------------------------

# total jobs for sic codes by ua
ua_sic_totals <- msoa_to_ua %>%
  left_join(sic_jobs) %>%
  select(-c(msoa_zone_id, lads20_zone_id, lad_desc, ua_desc)) %>% 
  group_by(ua_1998_zone_id) %>%
  summarise_all(sum) %>%
  ungroup()

# nts hb outbound total trip by ua and purpose
ua_trips <- nts %>% 
  select(p, TripDestUA1998_B01ID, weighted_trips) %>% 
  filter(p %in% 1:8, TripDestUA1998_B01ID != -8) %>% 
  group_by(p, TripDestUA1998_B01ID) %>%
  summarise(trips = sum(weighted_trips)) %>% 
  ungroup() 

# sic code job totals with nts trips per ua zone
ua_sic_trips <- ua_sic_totals %>% 
  left_join(ua_trips, by = c('ua_1998_zone_id' = 'TripDestUA1998_B01ID')) %>% 
  select(ua_1998_zone_id, p, trips, everything()) %>% 
  na.omit()

ua_sic_trips <- ua_sic_trips %>%
  group_split(p)

### Find predictive 2 digit SIC's
ecat_names <- sic_jobs %>%
  colnames() %>% 
  str_subset("Wrkrs")

# Model Building ----------------------------------------------------------

df = ua_sic_trips[[1]]

build_models <- function(df){
  
  y <- df %>%
    select(trips) %>% pull()
  
  x <- df %>%
    select(-c(ua_1998_zone_id, p, trips))
  
  x <- data.matrix(x)
  
  cv_model <- cv.glmnet(x, y, alpha = 1)
  
  best_lambda <- cv_model$lambda.min
  final_lambda <- ceiling(log(best_lambda))
  
  best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
  
  coefs <- coef(best_model) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column(var = 'sic') %>%
    as_tibble() %>% 
    slice(2:nrow(.)) %>% 
    filter(s0 != 0) %>%
    mutate(s1 = abs(s0)) %>% 
    arrange(desc(s0)) %>%
    pull(sic)
  
}

model_outputs <- map(ua_sic_trips, build_models)  

#############################

build_model <- function(df, e_name, save_plots){
  
  new_df <- df %>%
    select(p, trips, e_name)
  
  purpose <- new_df %>% 
    distinct(p) %>% 
    pull()
  
  my_formula <- str_c("trips ~ ", e_name)
  my_formula <- as.formula(my_formula)
  
  model_summary <- lm(my_formula, data = new_df) %>% 
    summary()
  
  r2 <- model_summary$r.squared
  
  if(save_plots){
    
    r2_plot <- new_df %>%
      ggplot(aes_string(e_name, "trips")) +
      geom_point() + 
      geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
      stat_poly_eq(formula =  y ~ x, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +
      ggtitle(str_c(purpose)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # save
    nts_dir <- "Y:/NTS/outputs/attractions/2_digit/"
    p_dir <- str_c(nts_dir, purpose, "/")
    
    # create dir if not exists
    dir.create(p_dir, showWarnings = FALSE, recursive = TRUE)
    
    # file name
    output_dir <- str_c(p_dir, e_name, ".png")
    
    # save
    png(filename = output_dir,
        type = "cairo",
        units = "in", 
        width = 5, 
        height = 5, 
        pointsize = 12, 
        res = 300)
    
    print(r2_plot)
    dev.off()
    
  }
  
  #output 
  tibble(p = purpose, e_cat = e_name, r2 = r2)
  
}

output <- map(ua_sic_trips, function(x){
  
  map(ecat_names, function(y){
    
    build_model(x, y, FALSE)
    
  })
  
})

ua_sic = ua_sic_trips[[1]]
purpose = 1
best_ecats = model_outputs[[1]]
sic = sic_jobs
cut_off = 0.9 
greater_or_less = "greater"
output
ua_to_gor
gor_expand_factors


calculate_attractions <- function(ua_sic, purpose, best_ecats, sic, ua_to_gor, gor_expand_factors){
  
  # imagine the model for purpose 1 has been built and the following covariates are 'best'
  # these covariates have the individual highest R^2 univariate models
  total_jobs <- ua_sic %>% 
    select(contains(best_ecats)) %>% 
    sum()
  
  # attach a gor to ua and calculate total trips via expansion factor of nts people sample to population
  expand_trips_jobs <- ua_sic %>% 
    left_join(ua_to_gor) %>% 
    left_join(gor_expand_factors) %>% 
    select(ua_1998_zone_id, gor, expansion_factor, everything()) %>% 
    mutate(expanded_trips = trips * expansion_factor) %>% 
    select(expanded_trips, contains(best_ecats))
  
  # number of total jobs within predictive ecats
  total_jobs <- expand_trips_jobs %>% 
    select(-expanded_trips) %>% 
    sum()
  
  # number of total expanded trips
  total_expanded_trips <- expand_trips_jobs %>% 
    summarise(total_expanded_trips = sum(expanded_trips, na.rm = TRUE))
  
  # Calculate how many attractions a job generates
  attraction_weight <- total_expanded_trips %>% 
    mutate(total_jobs = total_jobs) %>% 
    mutate(attraction_weight = total_expanded_trips/total_jobs) %>% 
    pull(attraction_weight)
  
  #final_trips <- sic %>% 
  #  na.omit() %>%
  #  select(msoa_zone_id, contains(best_ecats)) %>% 
  #  mutate(attractions = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>% 
  #  mutate(attractions = attractions * attraction_weight,
  #         p = purpose) %>% 
  #  select(msoa_zone_id, p, attractions)
  
  final_trips <- sic %>% 
    na.omit() %>%
    select(msoa_zone_id, contains(best_ecats)) %>% 
    pivot_longer(-msoa_zone_id, names_to = "sic", values_to = "attractions") %>% 
    group_by(msoa_zone_id) %>%
    summarise(attractions = sum(attractions, na.rm = T) * attraction_weight) %>%
    ungroup() %>%
    mutate(p = purpose)
  
  sum_output <- final_trips %>% 
    select(attractions) %>% 
    sum(na.rm = TRUE)
  
  sum_output <- tibble(p = purpose, attractions = sum_output)
  
  output1 <- sic %>%
    select(contains(best_ecats)) %>% 
    summarise_all(sum) %>% 
    mutate(p = purpose) %>% 
    pivot_longer(-p, names_to = "sic", values_to = "jobs") %>%
    mutate(weight = attraction_weight)
  
  list(final_trips, sum_output) 
  
}

attractions <- pmap(list(ua_sic_trips, 1:8, model_outputs), calculate_attractions, sic, ua_to_gor, gor_expand_factors)

bind_rows(attractions) %>% 
  write_csv("I:/NTS/outputs/attractions/attraction_weights_v0.1.csv")

attractions <- map2(ua_sic_trips, 1:8, calculate_attractions, sic_jobs, 0.8, "greater", output, ua_to_gor, gor_expand_factors)

##################################




#write_csv(final_output, "I:/NTS/outputs/attractions/initial_results/attractions.csv")

final_output %>% 
  filter(p == 1) %>% summarise(sum(attractions))

totals <- attractions %>%
  map(2) %>%
  bind_rows()

### Check against LA productions

msoa_to_la3 <- msoa_to_la %>%
  select(LAD20CD, LAD20NM) %>%
  left_join(msoa_to_la2, by = c("LAD20CD" = "lads20_zone_id")) %>%
  filter(!is.na(msoa_to_lads20)) %>%
  select(msoa_zone_id, LAD20CD, LAD20NM)

tempro2 <- tempro %>%
  filter(Purpose %in% 1:8) %>%
  rename(year_2018 = `2018`) %>% 
  group_by(msoa_zone_id, trip_end_type, Purpose) %>%
  summarise(trips = sum(year_2018))

tempro3 <- tempro2 %>%
  pivot_wider(names_from = "trip_end_type", values_from = "trips") %>% 
  left_join(msoa_to_la3) %>%
  group_by(LAD20CD, LAD20NM, Purpose) %>%
  summarise(tempro_attractions = sum(attractions),
            tempro_productions = sum(productions))

lad_output <- final_output %>% 
  left_join(msoa_to_la3) %>%
  select(LAD20CD, LAD20NM, p, attractions) %>% 
  group_by(LAD20CD, LAD20NM, p) %>%
  summarise(tfn_attractions = sum(attractions))

lad_comparison <- lad_output %>%
  left_join(tempro3, by = c("LAD20CD", "LAD20NM", "p" = "Purpose")) 

lad_comparison %>% 
  select(-p) %>% 
  group_by(LAD20CD, LAD20NM) %>%
  summarise(across(tfn_attractions:tempro_productions, sum))

lad_comparison %>%
  filter( p == 5, tfn_attractions > 700000)

lad_comparison %>%
  ggplot(aes(x = tfn_attractions, y = tempro_attractions)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red ", formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),  
               parse = TRUE) +
  ggtitle("tfn attractions vs tempro attractions") +
  labs(x = "TfN", y = "Tempro") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ p, nrow = 4)

lad_comparison %>%
  mutate(diff = tempro_attractions - tfn_attractions) %>%
  mutate(ratio = tfn_attractions/tempro_attractions) %>%
  filter(LAD20CD %in% c("E09000019", "E09000007", "E09000033", "E09000028", "E09000012", "E09000013", "E09000020", "E09000022", "E09000030", "E09000032")) %>% 
  arrange(desc(ratio)) %>% 
  print(n= 30)


###############

lad_comparison %>%
  filter(p == 8) %>% 
  arrange(desc(tempro_attractions))
mutate(diff = tfn_attractions - tempro_attractions) %>%
  arrange(desc(diff))

# todo calculate expansion factor for working age people (16-74), for all purposes - try maybe just 1 and 2
# filter nts for this too
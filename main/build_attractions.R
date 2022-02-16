library(tidyverse)
library(caret)
library(modelr)
library(ggpmisc)
library(glmnet)

select <- dplyr::select

# Inputs ------------------------------------------------------------------

# Land Use
land_use <- read_csv("I:/NorMITs Land Use/base_land_use/iter3e/outputs/land_use_2018_pop.csv")

# NTS
nts <- read_csv("I:/NTS/classified builds/cb_tfn.csv")

# 2 digit sic codes
msoa_sic <- read_csv("I:/Data/Attractions Analysis/hsl_2digit_sic_msoa_2018.csv")

# TEMPRO attractions and productions
tempro <- read_csv("I:/Data/TEMPRO/tempro_pa_data_ave_week.csv")

# MSOA to GOR
msoa_to_gor <- read_csv("I:/Data/Zone Translations/msoa_to_gor_correspondence.csv")

# MSOA to UA
msoa_to_ua <- read_csv("Y:/Data Strategy/GIS Shapefiles/UA 1998/msoa_ua1998_lookup.csv")

# MSOA to LA
msoa_to_la <- read_csv("Y:/NTS/lads20_to_msoa_correspondence.csv")
la_names <- read_csv("Y:/NTS/lad_2020.csv")

### Save dir
attraction_version <- "v2.03"
save_dir <- "I:/NTS/outputs/attractions/"
attractions_output_dir <- str_c(save_dir, "attraction_weights_", attraction_version, ".csv")
attractions_summary_dir <- str_c(save_dir, "tfn_vs_tempro_", attraction_version, ".csv")

plots_dir <- str_c(save_dir, "plots/")

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
la_names <- la_names %>%
  select(LAD20CD, LAD20NM)

msoa_to_la <- la_names %>% 
  left_join(msoa_to_la, by = c("LAD20CD" = "lads20_zone_id")) %>%
  filter(!is.na(msoa_to_lads20)) %>%
  select(msoa_zone_id, LAD20CD, LAD20NM)

### SIC Data

msoa_sic <- rename(msoa_sic, msoa_zone_id = geo_code)

### Land Use

land_use <- land_use %>%
  select(msoa_zone_id, people)

### NTS

# GOR names
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

# Aggregate walk/cycle, car/van and light/heavy rail
nts <- nts %>%
  filter(main_mode != 8) %>%
  mutate(m_agg = case_when(
    main_mode %in% 1:2 ~ 1,
    main_mode %in% 3:4 ~ 3,
    main_mode == 5 ~ 5,
    main_mode %in% 6:7 ~ 6,
  ))

# Aggregate work/business and VF/holiday
nts <- nts %>%
  filter(p %in% 1:8) %>%
  mutate(p_agg = case_when(
    p %in% 1:2 ~ 1,
    p %in% 3:6 ~ p,
    p %in% 7:8 ~ 7
  ))

### TEMPRO
tempro <- tempro %>%
  filter(Purpose %in% 1:8) %>%
  rename(year_2018 = `2018`) %>% 
  group_by(msoa_zone_id, trip_end_type, Purpose, Mode) %>%
  summarise(trips = sum(year_2018)) %>%
  pivot_wider(names_from = "trip_end_type", values_from = "trips") %>%
  rename(p = Purpose,
         main_mode = Mode) 
  
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
  distinct(IndividualID, gor) %>% 
  count(gor) %>%
  rename(people_sample = n) %>% 
  filter(!is.na(gor))

gor_stats <- left_join(nts_sample_stats, pop_by_gor)

gor_expand_factors <- gor_stats %>%
  mutate(expansion_factor = people/people_sample) %>% 
  select(gor, expansion_factor)

# Data merging and processing for model building (agg p and m) -----------------

# total jobs for sic codes by ua
ua_sic_agg <- msoa_to_ua %>%
  left_join(msoa_sic) %>%
  select(-c(msoa_zone_id, lads20_zone_id, lad_desc, ua_desc)) %>% 
  group_by(ua_1998_zone_id) %>%
  summarise_all(sum) %>%
  ungroup()

# nts hb outbound trips by dest ua and purpose
ua_trips_agg <- nts %>% 
  select(p_agg, m_agg, TripDestUA1998_B01ID, weighted_trips) %>% 
  filter(TripDestUA1998_B01ID != -8) %>% 
  group_by(p_agg, TripDestUA1998_B01ID, m_agg) %>%
  summarise(trips = sum(weighted_trips)) %>% 
  ungroup() 

# sic code job totals with nts trips per ua zone
ua_sic_trips_agg <- ua_sic_agg %>% 
  left_join(ua_trips_agg, by = c('ua_1998_zone_id' = 'TripDestUA1998_B01ID')) %>% 
  select(ua_1998_zone_id, p_agg, m_agg, trips, everything()) %>% 
  na.omit()

ua_sic_trips_agg <- ua_sic_trips_agg %>%
  group_by(p_agg, m_agg) %>%
  nest() %>%
  ungroup()

### Find predictive 2 digit SIC's
ecat_names <- msoa_sic %>%
  colnames() %>% 
  str_subset("Wrkrs")

# Model Building ----------------------------------------------------------

" Uncomment to test function line by line "
# df = ua_sic_trips_agg %>% slice(1)
# p = df$p_agg
# m = df$m_agg
# data = df$data[[1]]

build_models <- function(p, m, data){
  
  y <- data %>%
    select(trips) %>% pull()
  
  x <- data %>%
    select(-c(ua_1998_zone_id, trips))
  
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

# Find predictive sics for aggregate purpose and mode combinations
super_structure <- ua_sic_trips_agg %>%
  mutate(predictive_sics = pmap(list(p_agg, m_agg, data), build_models)) %>%
  select(p_agg, m_agg, predictive_sics)

# Infill predictive sics for mode
super_structure <- super_structure %>% 
  complete(nesting(p_agg), m_agg = 1:7) %>%
  fill(predictive_sics)

# Infill predictive sics for purpose
super_structure <- super_structure %>%
  complete(nesting(m_agg), p_agg = 1:8) %>%
  fill(predictive_sics)

# rename for disagg purpose and mode names
super_structure <- super_structure %>%
  rename(p = p_agg,
         main_mode = m_agg) %>%
  select(p, main_mode, predictive_sics) %>%
  arrange(p, main_mode) 

# NTS processing for infilling purpose and mode ---------------------------

# nts hb outbound total trip by ua and purpose
ua_trips_all <- nts %>% 
  select(p, main_mode, TripDestUA1998_B01ID, weighted_trips) %>% 
  filter(TripDestUA1998_B01ID != -8) %>% 
  group_by(p, TripDestUA1998_B01ID, main_mode) %>%
  summarise(trips = sum(weighted_trips)) %>% 
  ungroup() 

# sic code job totals with nts trips per ua zone
ua_sic_trips_all <- ua_sic_agg %>% 
  left_join(ua_trips_all, by = c('ua_1998_zone_id' = 'TripDestUA1998_B01ID')) %>% 
  select(ua_1998_zone_id, p, main_mode, trips, everything()) %>% 
  na.omit()

# nest to tobain a data frame for each purpose/mode combination
ua_sic_trips_all <- ua_sic_trips_all %>%
  group_by(p, main_mode) %>%
  nest() %>%
  rename(trip_data = data) %>%
  ungroup()

# join predictive sics with NTS attractions trip data
super_structure <- super_structure %>%
  left_join(ua_sic_trips_all)

super_structure %>% print(n = 100)

# Calculate attraction weights --------------------------------------------

" Uncomment to test function line by line "

#test <- super_structure %>% slice(1)
#ua_sic = test$trip_data[[1]]
#purpose = test$p
#mm = test$main_mode
#best_ecats = test$predictive_sics[[1]]
#sic = msoa_sic

calculate_attractions <- function(ua_sic, purpose, mm, best_ecats, sic, ua_to_gor, gor_expand_factors){
  
  # sum of jobs for predictive sics
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
  
  # disaggregated output by msoa
  msoa_output <- sic %>% 
    na.omit() %>%
    select(msoa_zone_id, contains(best_ecats)) %>% 
    pivot_longer(-msoa_zone_id, names_to = "sic", values_to = "attractions") %>% 
    group_by(msoa_zone_id) %>%
    summarise(attractions = sum(attractions, na.rm = T) * attraction_weight) %>%
    ungroup()
  
  # summary output
  sum_output <- msoa_output %>% 
    select(attractions) %>% 
    sum(na.rm = TRUE)
  
  sum_output <- tibble(attractions = sum_output)
  
  # final output for attraction model
  final_output <- sic %>%
    select(contains(best_ecats)) %>% 
    summarise_all(sum) %>%
    pivot_longer(everything(),names_to = "sic", values_to = "jobs") %>%
    mutate(trip_rate = attraction_weight)
  
  list(final_output, 
       msoa_output,
       sum_output)
  
}

# Run function to obtain outputs
super_structure <-  super_structure %>%
  mutate(attractions = pmap(list(trip_data, p, main_mode, predictive_sics),
                            calculate_attractions,
                            msoa_sic,
                            ua_to_gor,
                            gor_expand_factors))

super_structure <- super_structure %>%
  mutate(final_output = map(attractions, 1),
         disagg_output = map(attractions, 2),
         summary_output = map(attractions, 3)) %>%
  select(p, main_mode, final_output, disagg_output, summary_output)

extract_and_unnest <- function(output_name, df){
  
  df %>%
    select(p, main_mode, all_of(output_name)) %>%
    unnest(cols = c(output_name))
  
}

final_output <- extract_and_unnest("final_output", super_structure)
disagg_output <- extract_and_unnest("disagg_output", super_structure)
summary_output <- extract_and_unnest("summary_output", super_structure)

final_output <- final_output %>%
  rename(purpose = p,
         mode = main_mode,
         sic_code = sic) %>% 
  mutate(sic_code = parse_number(sic_code))

write_csv(final_output, "I:/NTS/outputs/attractions/attraction_weights_v2.03.csv")

# Post processing to compare tfn vs tempro  ------------------------------

# tfn
tfn_la_attractions <- disagg_output %>% 
  mutate(main_mode = case_when(
    main_mode == 4 ~ 3,
    main_mode == 7 ~ 6,
    TRUE ~ main_mode
  )) %>%
  left_join(msoa_to_la) %>%
  select(LAD20CD, LAD20NM, p, main_mode, attractions) %>% 
  group_by(LAD20CD, LAD20NM, p, main_mode) %>%
  summarise(tfn_attractions = sum(attractions)) %>%
  ungroup()

# Tempro
tempro_la_attractions <- tempro %>%
  mutate(main_mode = case_when(
    main_mode == 4 ~ 3,
    TRUE ~ main_mode
  )) %>%
  left_join(msoa_to_la) %>%
  select(LAD20CD, LAD20NM, p, main_mode, attractions, productions) %>% 
  group_by(LAD20CD, LAD20NM, p, main_mode) %>%
  summarise(tempro_attractions = sum(attractions),
            tempro_productions = sum(productions)) %>%
  ungroup()

tfn_vs_tempro <- tfn_la_attractions %>%
  left_join(tempro_la_attractions) %>% 
  mutate(p = case_when(
    p == 1 ~ "Commute",
    p == 2 ~ "Business",
    p == 3 ~ "Education",
    p == 4 ~ "Shopping",
    p == 5 ~ "PB", 
    p == 6 ~ "Leisure",
    p == 7 ~ "Visit Friends",
    p == 8 ~ "Holiday/Day Trip")) %>% 
  mutate(p = factor(p, levels = c("Commute",
                                  "Business",
                                  "Education",
                                  "Shopping",
                                  "PB",
                                  "Leisure",
                                  "Visit Friends",
                                  "Holiday/Day Trip"))) %>% 
  mutate(main_mode = case_when(
    main_mode == 1 ~ "Walk",
    main_mode == 2 ~ "Cycle",
    main_mode == 3 ~ "Car/Van",
    main_mode == 5 ~ "Bus", 
    main_mode == 6 ~ "Rail")) %>% 
  mutate(main_mode = factor(main_mode, levels = c("Walk",
                                  "Cycle",
                                  "Car/Van",
                                  "Bus",
                                  "Rail")))

# save tfn vs tempro csv
write_csv(tfn_vs_tempro, attractions_summary_dir)

tfn_vs_tempro <- tfn_vs_tempro %>%
  mutate(across(tfn_attractions:tempro_productions, ~ ./10000))

# Compare Purpose -------------------------------------------------------------

main_plot <- tfn_vs_tempro %>% 
  ggplot(aes(x = tfn_attractions, y = tempro_attractions)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red ", formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..rr.label..)),  
               parse = TRUE) +
  ggtitle("TfN vs TEMPRO attractions") +
  labs(x = "TfN (/10,000)", y = "TEMPRO (/10,000)") +
  theme(plot.title = element_text(hjust = 0.5))

purposes_comparison <- main_plot +
  facet_wrap(~ p, nrow = 4, scales = "free")

purpose_mode_comparison <- main_plot +
  facet_wrap(main_mode ~ p, scales = "free", nrow = 5)

save_plot <- function(dir, plot_name, w, l){
  
  png(filename = dir,
      type = "cairo",
      units = "in", 
      width = w, 
      height = l, 
      pointsize = 12, 
      res = 300)
  
  print(plot_name)
  dev.off()
  
}

save_plot(dir = str_c(plots_dir, "purpose_comparison.png"),
          plot_name = purposes_comparison,
          w = 5,
          l = 8)


save_plot(dir = str_c(plots_dir, "purpose_mode_comparison.png"),
          plot_name = purpose_mode_comparison,
          w = 15,
          l = 10)
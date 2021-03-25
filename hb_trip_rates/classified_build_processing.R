library(tidyverse)
library(vroom)

# Initialise Directories --------------------------------------------------

# NTS folder
nts_dir <- "Y:/NTS/"

# Outputs folder
output_dir <- str_c(nts_dir, "outputs/trip_rates/")

# Import folder
import_dir <- str_c(nts_dir, "import/")

# Unclassified build
unclassified_build_dir <- str_c(import_dir, "tfn_unclassified_build_no_stage19.csv")

# Lookups
lookup_dir <- str_c(nts_dir, "lookups/")

# Column names
column_names_dir <- str_c(import_dir, "variables_hb_trip_rates.csv")

# Lookup functions
source(str_c("C:/Users/Pluto/Documents/GitHub/NTS-Processing/lookups.r"))

# Read in variable names
column_names <- read_csv(column_names_dir, col_names = FALSE)
column_names <- pull(column_names)
column_names <- c(column_names, "W5xHh")

# Read in unclassified_build 

unclassified_build <- data.table::fread(unclassified_build_dir)
unclassified_build <- as_tibble(unclassified_build)

unclassified_build1 <- unclassified_build
#unclassified_build <- unclassified_build1

# Recode purposes ---------------------------------------------------------

# Define trip origin, hb_purpose, nhb_purpose, nhb_purpose_hb_leg and trip_purpose
unclassified_build <- unclassified_build %>%
  lu_trip_origin() %>% 
  lu_hb_purpose() %>%
  lu_nhb_purpose() %>% 
  lu_nhb_purpose_hb_leg() %>% 
  mutate(trip_purpose = ifelse(trip_origin == "hb", hb_purpose, nhb_purpose))

# Recode exploratory variables --------------------------------------------
unclassified_build <- unclassified_build %>%
  lu_gender() %>% 
  lu_age_work_status() %>% 
  lu_cars() %>% 
  lu_hh_adults() %>% 
  lu_soc_cat() %>%
  lu_main_mode() %>%
  lu_start_time() %>% 
  lu_end_time() %>%
  lu_tfn_area_type() %>%
  mutate(ns_sec = ifelse(NSSec_B03ID == -9, 99, NSSec_B03ID))

# Reweight short walk trips by a factor of 7 ------------------------------
#classified_build_pre_weight <- unclassified_build %>%
#  mutate(W5xHh = ifelse(TripDisIncSW < 1 & main_mode == 1, 7, W5xHh)) %>%
#  filter(!is.na(W5xHh))

classified_build_pre_weight <- unclassified_build %>% 
  mutate(sw_weight = ifelse(TripDisIncSW < 1 & main_mode == 1, 7, 1))

#classified_build_pre_weight %>% write_csv("C:/Users/Pluto/Documents/classified_nts_pre-weighting_child_gender.csv")

classified_build_pre_weight %>% write_csv(str_c(import_dir,"classified_nts_pre-weighting.csv"))

classified_build_pre_weight %>% write_csv("C:/Users/Pluto/Documents/classified_nts_pre_weighting.csv")

classified_build_pre_weight %>% 
  select(EcoStat_B01ID, NumCarVan_B02ID, HHoldNumAdults, HHoldOSWard_B01ID, tfn_area_type, W1, W3) %>% 
  group_by(EcoStat_B01ID, HHoldOSWard_B01ID, HHoldNumAdults, NumCarVan_B02ID, tfn_area_type) %>% 
  summarise(weighted_count = sum(W1 * W3),
            count = n()) %>% 
  ungroup() %>% 
  arrange(EcoStat_B01ID, HHoldOSWard_B01ID, HHoldNumAdults, tfn_area_type) %>%
  write_csv("Y:/NTS/Car Availability Build_MS.csv")
  
# Weighting Methodology for hb trips -------------------------------------

weighted_trip_rates <- classified_build_pre_weight %>% 
  filter(trip_purpose %in% 1:8) %>%
  mutate(trip_weight = W1 * sw_weight * W5 * W2) %>% 
  group_by(IndividualID, trip_purpose, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, W2) %>% 
  summarise(trip_weights = sum(trip_weight),
            weekly_trips = sum(W1 * sw_weight)) %>% 
  ungroup() %>% 
  mutate(trip_rate = trip_weights)

classified_build_weight <- weighted_trip_rates %>%
  mutate(trip_purpose = as.integer(trip_purpose)) %>% 
  complete(nesting(IndividualID, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type),
           trip_purpose = 1:8,
           fill = list(weekly_trips = 0, trip_weights = 0, trip_rate = 0, W2 = 0))

classified_build_pre_weight %>%
  filter(trip_purpose %in% 1:8) %>% 
  group_by(trip_purpose, age_work_status) %>% 
  summarise(W2 = mean(W2, na.rm = TRUE),
            W5 = mean(W5, na.rm = TRUE),
            W5xHH = mean(W5xHH, na.rm = TRUE)) %>% 
  write_csv("Y:/NTS/import/mean weights_p_aws.csv")

classified_build_pre_weight %>%
  filter(trip_purpose %in% 1:8) %>% 
  group_by(trip_purpose, age_work_status, gender, hh_adults, cars) %>% 
  summarise(W2 = mean(W2, na.rm = TRUE),
            W5 = mean(W5, na.rm = TRUE),
            W5xHH = mean(W5xHH, na.rm = TRUE)) %>% 
  write_csv("Y:/NTS/import/mean weights_p_aws_g_hhadults_cars.csv")

#classified_build_weight %>% write_csv(str_c(output_dir,"classified_nts_trip_rates.csv"))
classified_build_weight %>% write_csv("C:/Users/Pluto/Documents/Trip_rate_testing/classified_nts_trip_rates_v10.csv")

testing <- read_csv("Y:/NTS/lookups/hb_purpose---TripPurpTo_B01ID.csv")
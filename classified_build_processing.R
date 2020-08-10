library("tidyverse")

# Initialise Directories --------------------------------------------------

# NTS folder
nts_dir <- "Y:/NTS/"

# Outputs folder
output_dir <- str_c(nts_dir, "outputs/trip_rates/")

# Import folder
import_dir <- str_c(nts_dir, "import/")

# Unclassified build
unclassified_build_dir <- str_c(import_dir, "tfn_unclassified_build.csv")

# Lookups
lookup_dir <- str_c(nts_dir, "lookups/")

# Lookup functions
source(str_c(lookup_dir,"lookups.r"))

# Read in unclassified_build
unclassified_build <- read_csv(unclassified_build_dir)

# Recode purposes ---------------------------------------------------------

# Adjust classifications to exclude walking trips in day trips
unclassified_build <- unclassified_build %>%
  filter(TripPurpTo_B01ID %in% 1:14 |
           (TripPurpTo_B01ID == 15 & MainMode_B04ID != 1) |
           TripPurpTo_B01ID %in% 16:23)

unclassified_build <- unclassified_build %>%
  filter(TripPurpFrom_B01ID %in% 1:14 |
           (TripPurpFrom_B01ID == 15 & MainMode_B04ID != 1) |
           TripPurpFrom_B01ID %in% 16:23)

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
  lu_area_type() %>%
  lu_soc_cat() %>%
  lu_main_mode() %>%
  lu_start_time() %>% 
  lu_end_time() %>%
  lu_tfn_area_type() %>%
  lu_ca() %>%
  rename(ns_sec = NSSec_B03ID)

# Reweight short walk trips by a factor of 7 ------------------------------
classified_build_pre_weight <- unclassified_build %>%
  mutate(W5xHh = ifelse(TripDisIncSW < 1 & main_mode == 1, 7, W5xHh)) %>%
  filter(!is.na(W5xHh))

classified_build_pre_weight %>% write_csv(str_c(import_dir,"classified_nts_pre-weighting.csv"))

# Weighting Methodology -------------------------------------
weighted_trip_rates <- classified_build_pre_weight %>%
  filter(trip_purpose %in% 1:8) %>%
  mutate(trip_weights = W1 * W5xHh * W2) %>%
  group_by(IndividualID, trip_purpose, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, W2) %>%
  summarise(trip_weights = sum(trip_weights),
            weekly_trips = n())%>%
  ungroup() %>%
  mutate(trip_rate = trip_weights/W2,
         trip_purpose = as.integer(trip_purpose)) %>%
  select(-trip_weights, -W2)

classified_build_weight <- weighted_trip_rates %>%
  complete(nesting(IndividualID, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type),
           trip_purpose = 1:8,
           fill = list(weekly_trips = 0, trip_rate = 0))

classified_build_weight %>% write_csv(str_c(output_dir,"classified_nts_trip_rates.csv"))

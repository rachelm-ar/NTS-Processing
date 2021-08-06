library(tidyverse)

# Initialise Directories --------------------------------------------------

# NTS folder
nts_dir <- "Y:/NTS/"

# Outputs folder
output_dir <- str_c(nts_dir, "outputs/trip_rates/")

# Import folder
import_dir <- str_c(nts_dir, "import/")

# Unclassified build
unclassified_build_dir <- str_c(import_dir, "tfn_unclassified_build19.csv")

# Lookups
lookup_dir <- str_c(nts_dir, "lookups/")

# Column names
column_names_dir <- str_c(import_dir, "variables_hb_trip_rates.csv")

# Lookup functions
source(str_c(lookup_dir,"lookups.r"))

# Read in variable names
column_names <- read_csv(column_names_dir, col_names = FALSE)
column_names <- pull(column_names)

# Read in unclassified_build 
unclassified_build <- data.table::fread("C:/Users/Pluto/Documents/R/tfn_unclassified_build19.csv")
unclassified_build <- as_tibble(unclassified_build)

unclassified_build1 <- unclassified_build
unclassified_build <- unclassified_build1

unclassified_build %>% 
  filter(SurveyYear == 2002, W1 == 1) %>% 
  distinct(IndividualID, W1, W2) %>% 
  summarise(sum(W1*W2))

# Recode purposes ---------------------------------------------------------

# Adjust classifications to exclude walking trips in day trips 
#unclassified_build <- unclassified_build %>%
#  filter(TripPurpTo_B01ID %in% 1:14 |
#           (TripPurpTo_B01ID == 15 & MainMode_B04ID != 1) |
#           TripPurpTo_B01ID %in% 16:23)
#
#unclassified_build <- unclassified_build %>%
#  filter(TripPurpFrom_B01ID %in% 1:14 |
#           (TripPurpFrom_B01ID == 15 & MainMode_B04ID != 1) |
#           TripPurpFrom_B01ID %in% 16:23)


# Define trip origin, hb_purpose, nhb_purpose, nhb_purpose_hb_leg and trip_purpose
#unclassified_build <- unclassified_build %>%
#  lu_trip_origin() %>% 
#  lu_hb_purpose() %>%
#  lu_nhb_purpose() %>% 
#  lu_nhb_purpose_hb_leg() %>% 
#  mutate(trip_purpose = ifelse(trip_origin == "hb", hb_purpose, nhb_purpose))

unclassified_build <- unclassified_build %>% 
  filter(TripPurpFrom_B01ID == 23) %>%  
  mutate(trip_purpose = case_when(
    TripPurpTo_B01ID == 1 ~ 1,
    TripPurpTo_B01ID == 2 ~ 2,
    TripPurpTo_B01ID == 3 ~ 3,
    TripPurpTo_B01ID == 4 ~ 4,
    TripPurpTo_B01ID == 5 ~ 4,
    TripPurpTo_B01ID == 6 ~ 5,
    TripPurpTo_B01ID == 7 ~ 5,
    TripPurpTo_B01ID == 8 ~ 5,
    TripPurpTo_B01ID == 9 ~ 6,
    TripPurpTo_B01ID == 10 ~ 7,
    TripPurpTo_B01ID == 11 ~ 6,
    TripPurpTo_B01ID == 12 ~ 6,
    TripPurpTo_B01ID == 13 ~ 6,
    TripPurpTo_B01ID == 14 ~ 8,
    TripPurpTo_B01ID == 15 & MainMode_B04ID == 1 ~ 6,
    TripPurpTo_B01ID == 15 & MainMode_B04ID != 1 ~ 8,
    TripPurpTo_B01ID == 16 ~ 6,
    TripPurpTo_B01ID == 17 ~ 7,
    TripPurpTo_B01ID == 18 ~ 1,
    TripPurpTo_B01ID == 19 ~ 2,
    TripPurpTo_B01ID == 20 ~ 3,
    TripPurpTo_B01ID == 21 ~ 4,
    TripPurpTo_B01ID == 22 ~ 6,
  ))

unclassified_build %>% 
  filter(TripPurpFrom_B01ID == 10, TripPurpTo_B01ID != 23) %>% 
  select(trip_origin, trip_purpose, hb_purpose, nhb_purpose, TripPurpTo_B01ID) %>% 
  distinct(hb_purpose)

# mutate(trip_purpose = ifelse(TripPurpFrom_B01ID == 10 & TripPurpTo_B01ID != 23, 7, trip_purpose))
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
  mutate(ns_sec = ifelse(NSSec_B03ID == -9, 99, NSSec_B03ID))
#gender = ifelse(age_work_status == 1, 2, gender) --- add to mutate if children are genderless

# Reweight short walk trips by a factor of 7 ------------------------------
#classified_build_pre_weight <- unclassified_build %>%
#  mutate(W5xHh = ifelse(TripDisIncSW < 1 & main_mode == 1, 7, W5xHh)) %>%
#  filter(!is.na(W5xHh))

classified_build_pre_weight <- unclassified_build %>% 
  mutate(sw_weight = ifelse(TripDisIncSW < 1 & main_mode == 1, 7, 1))

#classified_build_pre_weight %>% write_csv("C:/Users/Pluto/Documents/classified_nts_pre-weighting_child_gender.csv")

#classified_build_pre_weight %>% write_csv(str_c(import_dir,"classified_nts_pre-weighting.csv"))


#classified_build_pre_weight %>% 
#  select(EcoStat_B01ID, NumCarVan_B02ID, HHoldNumAdults, HHoldOSWard_B01ID, tfn_area_type, W1, W3) %>% 
#  group_by(EcoStat_B01ID, HHoldOSWard_B01ID, HHoldNumAdults, NumCarVan_B02ID, tfn_area_type) %>% 
#  summarise(weighted_count = sum(W1 * W3),
#            count = n()) %>% 
#  ungroup() %>% 
#  arrange(EcoStat_B01ID, HHoldOSWard_B01ID, HHoldNumAdults, tfn_area_type) %>%
#  write_csv("Y:/NTS/Car Availability Build_MS.csv")

test1 <- unclassified_build1 %>% 
  filter(TripPurpFrom_B01ID == 23, SurveyYear < 2013) %>%  
  mutate(trip_purpose = case_when(
    TripPurpTo_B01ID == 1 ~ 1, 
    TripPurpTo_B01ID == 2 ~ 2, 
    TripPurpTo_B01ID == 3 ~ 3,
    TripPurpTo_B01ID == 4 ~ 4, 
    TripPurpTo_B01ID == 5 ~ 4, 
    TripPurpTo_B01ID == 6 ~ 5,
    TripPurpTo_B01ID == 7 ~ 5, 
    TripPurpTo_B01ID == 8 ~ 5, 
    TripPurpTo_B01ID == 9 ~ 6,
    TripPurpTo_B01ID == 10 ~ 7, 
    TripPurpTo_B01ID == 11 ~ 6, 
    TripPurpTo_B01ID == 12 ~ 6,
    TripPurpTo_B01ID == 13 ~ 6, 
    TripPurpTo_B01ID == 14 ~ 8, 
    TripPurpTo_B01ID == 15 & MainMode_B04ID == 1 ~ 6, 
    TripPurpTo_B01ID == 15 & MainMode_B04ID != 1 ~ 8,
    TripPurpTo_B01ID == 16 ~ 6, 
    TripPurpTo_B01ID == 17 ~ 7, 
    TripPurpTo_B01ID == 18 ~ 2,
    TripPurpTo_B01ID == 19 ~ 2, 
    TripPurpTo_B01ID == 20 ~ 3, 
    TripPurpTo_B01ID == 21 ~ 4,
    TripPurpTo_B01ID == 22 ~ 6))


test1 %>%
  select(IndividualID, W5, W2, W5xHH, SurveyYear, trip_purpose) %>% 
  mutate(trip_weight = W5) %>% 
  group_by(IndividualID, trip_purpose, SurveyYear, W2) %>% 
  summarise(trip_weight = sum(trip_weight)) %>% 
  ungroup() %>% 
  complete(nesting(IndividualID, SurveyYear, W2),
           trip_purpose = 1:8,
           fill = list(trip_weight = 0)) %>% 
  group_by(trip_purpose, SurveyYear) %>% 
  summarise(trip_rate = sum(trip_weight)/sum(W2)) %>% 
  mutate(SurveyYear = as.factor(SurveyYear),
         trip_purpose = as.factor(trip_purpose)) %>% 
  ggplot() +
  geom_line(aes(SurveyYear, trip_rate, group = trip_purpose, colour = trip_purpose)) +
  ylim(0, 2.5) +
  scale_color_manual(values = c("black","yellow","red","green","blue","purple","orange","grey"))
  

test1 %>%
  select(IndividualID, W5, W2, W5xHH, SurveyYear, trip_purpose) %>% 
  complete(nesting(IndividualID, SurveyYear, W2),
           trip_purpose = 1:8,
           fill = list(W5 = 0, W5xHH = 0)) %>% 
  mutate(trip_weight = W5 * W2) %>% 
  group_by(IndividualID, trip_purpose, SurveyYear, W2) %>% 
  summarise(trip_weight = sum(trip_weight)) %>% 
  group_by(trip_purpose, SurveyYear) %>% 
  summarise(trip_rate = mean(trip_weight)) %>% 
  mutate(SurveyYear = as.factor(SurveyYear),
         trip_purpose = as.factor(trip_purpose)) %>% 
  ggplot() +
  geom_line(aes(SurveyYear, trip_rate, group = trip_purpose, colour = trip_purpose)) +
  scale_color_manual(values = c("black","yellow","red","green","blue","purple","orange","grey"))


  group_by(SurveyYear, trip_purpose) %>% 
  summarise(trip_rate = sum(trip_weight)/sum(W2)) %>% 
  ungroup() %>% 
  group_by(trip_purpose, SurveyYear) %>% 
  summarise(trip_rate = sum(trip_rate)/sum(W2)) %>% 
  mutate(SurveyYear = as.factor(SurveyYear),
         trip_purpose = as.factor(trip_purpose)) %>% 
  ggplot() +
  geom_line(aes(SurveyYear, trip_rate, group = trip_purpose, colour = trip_purpose)) +
  scale_color_manual(values = c("black","yellow","red","green","blue","purple","orange","grey"))
  
  
  
  mutate(trip_weight = W5 * W2) %>% 
  group_by(IndividualID, trip_purpose, SurveyYear, W2) %>% 
  summarise(weighted_trips = sum(trip_weight)) %>% 
  ungroup() %>% 
  complete(nesting(IndividualID, SurveyYear, W2),
           trip_purpose = 1:8,
           fill = list(weighted_trips = 0, W2 = 1)) %>% 
  ungroup() %>% 
  group_by(trip_purpose, SurveyYear) %>% 
  summarise(trip_rate = sum(weighted_trips)/sum(W2)) %>% 
  mutate(SurveyYear = as.factor(SurveyYear),
         trip_purpose = as.factor(trip_purpose)) %>% 
  ggplot() +
  geom_line(aes(SurveyYear, trip_rate, group = trip_purpose, colour = trip_purpose)) +
  scale_color_manual(values = c("black","yellow","red","green","blue","purple","orange","grey"))

# Weighting Methodology for hb trips -------------------------------------

  
  

classified_build_pre_weight %>% 
  filter(SurveyYear < 2013, trip_purpose %in% 1:8) %>% 
  mutate(trip_weight = W1 * W5xHH) %>% 
  group_by(IndividualID, trip_purpose, SurveyYear, W2) %>% 
  summarise(weighted_trips = sum(trip_weight)) %>% 
  ungroup() %>% 
  complete(nesting(IndividualID, SurveyYear, W2),
           trip_purpose = 1:8,
           fill = list(weighted_trips = 0)) %>% 
  group_by(trip_purpose, SurveyYear) %>% 
  summarise(trip_rate = sum(weighted_trips)/sum(W2)) %>% 
  mutate(SurveyYear = as.factor(SurveyYear),
         trip_purpose = as.factor(trip_purpose)) %>% 
  ggplot() +
  geom_line(aes(SurveyYear, trip_rate, group = trip_purpose, colour = trip_purpose)) +
  scale_color_manual(values = c("black","yellow","red","green","blue","purple","orange","grey"))

classified_build_pre_weight %>% 
  filter(SurveyYear < 2013, trip_purpose %in% 1:8) %>% 
  mutate(trip_weight = W1 * W5xHH) %>% 
  group_by(IndividualID, trip_purpose, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, W2) %>%
  summarise(weighted_trips = sum(trip_weight)) %>% 
  ungroup() %>% 
  distinct(IndividualID)
  complete(nesting(IndividualID, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, W2),
           trip_purpose = 1:8,
           fill = list(weighted_trips = 0)) %>% 
  group_by(trip_purpose, SurveyYear) %>% 
  summarise(trip_rate = sum(weighted_trips)/sum(W2)) %>% 
  mutate(SurveyYear = as.factor(SurveyYear),
         trip_purpose = as.factor(trip_purpose)) %>% 
  ggplot() +
  geom_line(aes(SurveyYear, trip_rate, group = trip_purpose, colour = trip_purpose)) +
  scale_color_manual(values = c("black","yellow","red","green","blue","purple","orange","grey"))


classified_build_pre_weight %>% 
  filter(SurveyYear < 2013, trip_purpose %in% 1:8) %>% 
  mutate(trip_weight = W1 * W5xHH) %>% 
  group_by(trip_purpose, SurveyYear) %>% 
  summarise(trip_rate = sum(trip_weight)) %>% 
  mutate(SurveyYear = as.factor(SurveyYear),
         trip_purpose = as.factor(trip_purpose)) %>% 
  ggplot() +
  geom_line(aes(SurveyYear, trip_rate, group = trip_purpose, colour = trip_purpose))

weighted_trip_rates <- classified_build_pre_weight %>% 
  filter(SurveyYear < 2013) %>% 
  filter(trip_purpose %in% 1:8) %>% 
  select(HouseholdID, IndividualID, trip_purpose, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, W2, W1, W5xHH, W5, sw_weight) %>% 
  mutate(trip_weight = W1 * sw_weight * W5 * W2) %>% 
  group_by(IndividualID, trip_purpose, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, W2) %>% 
  summarise(trip_weights = sum(trip_weight),
            weekly_trips = sum(W1 * sw_weight)) %>% 
  ungroup() %>% 
  mutate(trip_rate = trip_weights)

weighted_trip_rates %>% 
  group_by(trip_purpose, SurveyYear) %>% 
  summarise(trip_rate = mean(trip_rate, na.rm = TRUE)) %>%
  mutate(SurveyYear = as.factor(SurveyYear),
         trip_purpose = as.factor(trip_purpose)) %>% 
  ggplot() +
  geom_line(aes(SurveyYear, trip_rate, group = trip_purpose, colour = trip_purpose))

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


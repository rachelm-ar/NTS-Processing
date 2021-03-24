library(tidyverse)

nts_dir <- "Y:/NTS/"
lookup_dir <- str_c(nts_dir, "lookups/")

# Read in classified output
# Is this updated to the 19 data?
unclassified_build <- read_csv('Y:/NTS/import/tfn_unclassified_build_test.csv')
# Check trip records are unique

# Lookup functions
source(str_c(lookup_dir,"lookups.r"))

classified_build <- unclassified_build %>%
  mutate(trip_weights = W1 * W5 * W2) %>%
  lu_hb_purpose() %>%
  lu_nhb_purpose() %>%
  lu_trip_origin()
  
retain_cols <- c("IndividualID", "XSOC2000_B02ID", "NSSec_B03ID", "TripID", "TravDay", "MainMode_B04ID", "TripPurpFrom_B01ID",
                 "TripPurpTo_B01ID", "TripStart_B01ID", "TripEnd_B01ID", "hb_purpose", "nhb_purpose", "trip_weights")

cb_sub <- classified_build %>%
  select(retain_cols) %>%
  distinct()

### Proof of concept for NHB method ###
# Get 1 travel diary
# TODO: In prod retain some other survey characteristics or the join might fail
tour_groups <- cb_sub %>%
  #filter(IndividualID == 2015016037) %>% # IndividualID == 2019016037
  unique() %>%
  arrange(IndividualID, TripID) %>%
  group_by(IndividualID) %>%
  mutate(start_flag = case_when(TripPurpFrom_B01ID == 23 ~ 1,
                                TRUE ~ 0)) %>%
  mutate(end_flag = case_when(TripPurpTo_B01ID == 23 ~ 1,
                              TRUE ~ 0)) %>%
  mutate(trip_group = cumsum(start_flag)) %>%
  ungroup()

# Note - trip ID has to go
hb_trips <- tour_groups %>%
  select(IndividualID, hb_purpose, MainMode_B04ID, trip_group, start_flag, trip_weights) %>%
  filter(start_flag == 1) %>%
  rename(hb_mode = MainMode_B04ID)

nhb_trips <- tour_groups %>%
  select(IndividualID, nhb_purpose, MainMode_B04ID, trip_group, start_flag, end_flag, trip_weights) %>%
  filter(start_flag == 0 & end_flag == 0) %>%
  rename(nhb_mode = MainMode_B04ID)

hb_leg_info <- hb_trips %>%
  select(IndividualID, hb_purpose, hb_mode, trip_group)

nhb_w_hb <- nhb_trips %>%
  left_join(hb_leg_info, by=c("IndividualID", "trip_group"))

# Set up hb and nhb mode
classified_build <- classified_build %>%
  mutate(hb_mode = main_mode,
         nhb_mode = main_mode,
         )


trip_rates %>% write_csv(paste0(nts_dir, 'outputs/nhb_ave_wday_enh_trip_rates.csv'))

# Post processing - replace 99 with 'none' - rename soc_cat = soc, ns_sec to ns
# TODO: if you can do that in R is saves a job but might be awkwartd with data types

# TODO: Revisit time split - using at least variables from NTS

nhb_time_split <- classified_build %>%
  filter(trip_origin == 'nhb') %>%
  filter(nhb_purpose_hb_leg != 99) %>%
  filter(nhb_purpose != 99) %>%
  filter(TravDay %in% c(1,2,3,4,5)) %>%
  filter(!is.na(start_time)) %>%
  select(tfn_area_type,
         ca,
         nhb_purpose,
         main_mode,
         start_time,
         trip_weights) %>%
  group_by(tfn_area_type,
           ca,
           nhb_purpose,
           main_mode,
           start_time) %>%
  summarise(time_trips = sum(trip_weights)) %>%
  ungroup() %>%
  group_by(tfn_area_type,
           ca,
           nhb_purpose,
           main_mode) %>%
  mutate(total_trips = sum(time_trips)) %>%
  ungroup() %>%
  mutate(time_share = time_trips/total_trips) %>%
  select(-time_trips, -total_trips)

nhb_time_split %>% write_csv(paste0(nts_dir, 'outputs/nhb_ave_wday_time_split.csv'))

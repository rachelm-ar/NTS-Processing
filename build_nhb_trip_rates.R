library(tidyverse)

nts_dir <- "Y:/NTS/"
lookup_dir <- str_c(nts_dir, "lookups/")

# Read in classified output
classified_build <- read_csv('Y:/NTS/import/classified_nts_pre-weighting.csv')

# Lookup functions
source(str_c(lookup_dir,"lookups.r"))

classified_build <- classified_build %>%
  mutate(trip_weights = W1 * W5xHh * W2) %>%
  lu_ca()

hb_trips <- classified_build %>%
  filter(trip_origin == 'hb') %>%
  filter(TravDay %in% c(1,2,3,4,5)) %>%
  select(tfn_area_type,
         ca,
         trip_origin,
         hb_purpose,
         trip_weights) %>%
  group_by(tfn_area_type,
           ca,
           trip_origin,
           hb_purpose) %>%
  summarise(hb_trips = sum(trip_weights)) %>%
  ungroup() %>%
  select(-trip_origin)

nhb_trips <- classified_build %>%
  filter(trip_origin == 'nhb') %>%
  filter(TravDay %in% c(1,2,3,4,5)) %>%
  select(tfn_area_type,
         ca,
         trip_origin,
         nhb_purpose_hb_leg,
         nhb_purpose,
         trip_weights) %>%
  group_by(tfn_area_type,
           ca,
           trip_origin,
           nhb_purpose_hb_leg,
           nhb_purpose) %>%
  summarise(nhb_trips = sum(trip_weights)) %>%
  ungroup() %>%
  rename(hb_purpose = nhb_purpose_hb_leg) %>%
  select(-trip_origin)

trip_rates <- hb_trips %>%
  left_join(nhb_trips, by=c('tfn_area_type','ca', 'hb_purpose')) %>%
  mutate(trip_rate = (nhb_trips/hb_trips)) %>%
  filter(hb_purpose != 99) %>%
  filter(nhb_purpose != 99) %>%
  rename(p = hb_purpose) %>%
  rename(nhb_p = nhb_purpose) %>%
  rename(area_type = tfn_area_type) %>%
  select(-hb_trips, -nhb_trips)

trip_rates %>% write_csv(paste0(nts_dir, 'outputs/nhb_ave_wday_trip_rates.csv'))

nhb_mode_split <- classified_build %>%
  filter(trip_origin == 'nhb') %>%
  filter(nhb_purpose_hb_leg != 99) %>%
  filter(nhb_purpose != 99) %>%
  filter(TravDay %in% c(1,2,3,4,5)) %>%
  select(tfn_area_type,
         ca,
         nhb_purpose_hb_leg,
         nhb_purpose,
         main_mode,
         trip_weights) %>%
  group_by(tfn_area_type,
           ca,
           nhb_purpose_hb_leg,
           nhb_purpose,
           main_mode) %>%
  summarise(mode_trips = sum(trip_weights)) %>%
  ungroup() %>%
  group_by(tfn_area_type,
           ca,
           nhb_purpose_hb_leg,
           nhb_purpose) %>%
  mutate(total_trips = sum(mode_trips)) %>%
  ungroup() %>%
  mutate(mode_share = mode_trips/total_trips) %>%
  select(-mode_trips, -total_trips)
           
nhb_mode_split %>% write_csv(paste0(nts_dir, 'outputs/nhb_ave_wday_mode_split.csv'))

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

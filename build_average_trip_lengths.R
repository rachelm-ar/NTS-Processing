require(tidyverse)

# Code purpose: Build mode specific PA to OD factors

# Import ntem_build - NTEM segmented dataset 
nts_ntem_df <- read_csv('Y:/NTS/tfn_ntem_build.csv', guess_max = 10^9)

# Add trip weighting
nts_ntem_df <- nts_ntem_df %>%
  mutate(weighted_trip = W1 * W5xHh * W2)

# TODO: Going to need SOC & NS-SEC too
# TODO: Filter down to North

# Subset down
trip_length_subset <- nts_ntem_df %>%
  select(SurveyYear, TravDay, main_mode, hb_purpose, nhb_purpose, trip_origin, TripDisIncSW, weighted_trip) %>%
  mutate(trip_dist_km = TripDisIncSW*1.60934)

weekdays <- c(1,2,3,4,5)
years <- c(2017)

trip_length_subset <- trip_length_subset %>%
  filter(TravDay %in% weekdays) %>%
  filter(SurveyYear %in% years)

hb_trip_lengths <- trip_length_subset %>%
  filter(trip_origin == 'hb') %>%
  select(-nhb_purpose) %>%
  group_by(main_mode, hb_purpose, trip_dist_km) %>%
  summarise(weighted_trips = sum(weighted_trip,na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(main_mode, hb_purpose) %>%
  summarise(atl = weighted.mean(trip_dist_km, weighted_trips)) %>%
  ungroup() %>%
  filter(hb_purpose != 99) %>%
  filter(main_mode != 99)

nhb_trip_lengths <- trip_length_subset %>%
  filter(trip_origin == 'nhb') %>%
  select(-hb_purpose) %>%
  group_by(main_mode, nhb_purpose, trip_dist_km) %>%
  summarise(weighted_trips = sum(weighted_trip,na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(main_mode, nhb_purpose) %>%
  summarise(atl = weighted.mean(trip_dist_km, weighted_trips)) %>%
  ungroup() %>%
  filter(nhb_purpose != 99) %>%
  filter(main_mode != 99)

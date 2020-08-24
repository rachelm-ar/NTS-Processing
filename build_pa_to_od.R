require(tidyverse)

# Code purpose: Build mode specific PA to OD factors

# Import ntem_build - NTEM segmented dataset
# TODO: Update import
nts_ntem_df <- read_csv('Y:/NTS/import/classified_nts_pre-weighting.csv', guess_max = 10^9) #Why 10^9, its just checking the column type

# Add trip weighting
nts_ntem_df <- nts_ntem_df %>%
  mutate(weighted_trip = W1 * W5xHh * W2)

north_la <- c('E06000001', 'E06000002', 'E06000003', 'E06000004', 'E06000005', 'E06000006',
              'E06000007', 'E06000008', 'E06000009', 'E06000010', 'E06000011', 'E06000012',
              'E06000013', 'E06000014', 'E06000021', 'E06000047', 'E06000049', 'E06000050',
              'E06000057', 'E07000026', 'E07000027', 'E07000028', 'E07000029', 'E07000030',
              'E07000031', 'E07000033', 'E07000034', 'E07000035', 'E07000037', 'E07000038',
              'E07000117', 'E07000118', 'E07000119', 'E07000120', 'E07000121', 'E07000122',
              'E07000123', 'E07000124', 'E07000125', 'E07000126', 'E07000127', 'E07000128',
              'E07000137', 'E07000142', 'E07000163', 'E07000164', 'E07000165', 'E07000166',
              'E07000167', 'E07000168', 'E07000169', 'E07000170', 'E07000171', 'E07000174',
              'E07000175', 'E07000198', 'E08000001', 'E08000002', 'E08000003', 'E08000004',
              'E08000005', 'E08000006', 'E08000007', 'E08000008', 'E08000009', 'E08000010',
              'E08000011', 'E08000012', 'E08000013', 'E08000014', 'E08000015', 'E08000016',
              'E08000017', 'E08000018', 'E08000019', 'E08000021', 'E08000022', 'E08000023',
              'E08000024', 'E08000032', 'E08000033', 'E08000034', 'E08000035', 'E08000036',
              'E08000037', 'W06000001', 'W06000002', 'W06000003', 'W06000004', 'W06000005',
              'W06000006')
north_region <- c(1,2,3,4)

weekdays <- c(1,2,3,4,5)

tld_out <- nts_ntem_df %>%
  filter(TravDay %in% weekdays) %>%
  mutate(purpose = case_when(
    trip_origin == 'hb' ~ hb_purpose,
    trip_origin == 'nhb' ~ nhb_purpose,
    TRUE ~ 99
  )) %>%
  filter(HHoldOSLAUA_B01ID %in% north_la) %>%
  filter(TripOrigGOR_B02ID %in% north_region) %>%
  filter(TripDestGOR_B02ID %in% north_region) %>%
  select(main_mode, purpose, start_time, TripDisIncSW, weighted_trip) %>%
  group_by(main_mode, purpose, start_time, TripDisIncSW) %>%
  summarise(trips = sum(weighted_trip, na.rm=TRUE))

tld_out %>% write_csv('D:/tld_out.csv')

# Get a subset
trips <- nts_ntem_df %>%
  select(IndividualID, TripID, HHoldOSLAUA_B01ID, TravDay, TripPurpFrom_B01ID,
         TripPurpTo_B01ID, TripStart_B01ID, TripEnd_B01ID,
         main_mode, weighted_trip) %>%
  filter(!is.na(weighted_trip)) %>%
  filter(HHoldOSLAUA_B01ID %in% north_la) %>%
  filter(TravDay %in% weekdays) %>%
  group_by(IndividualID, TravDay) %>%
  mutate(trip_order = row_number()) %>%
  ungroup()

# Put return trips from previous day into previous day

trips <- trips %>%
  mutate(TravDay = case_when(
    TripPurpTo_B01ID == 23 & trip_order == 1 ~ TravDay-1, # 9	Elementary occupations
    TRUE ~ TravDay)
  )

# Get from home
from_home_trips <- trips %>%
  group_by(IndividualID, TravDay) %>%
  filter(TripPurpFrom_B01ID == 23) %>%
  mutate(in_out_count = row_number()) %>% # Add a number to out/in combos
  ungroup() %>%
  select(IndividualID, TripID, TravDay, in_out_count, TripPurpTo_B01ID, TripStart_B01ID, main_mode, weighted_trip) %>%
  rename(from_home_trip_id = TripID,
         purpose_from_home = TripPurpTo_B01ID,
         time_from_home = TripStart_B01ID,
         mode_from_home = main_mode,
         from_home_trip_weight = weighted_trip)

# Get to home
to_home_trips <- test %>%
  group_by(IndividualID, TravDay) %>%
  filter(TripPurpTo_B01ID == 23) %>%
  mutate(in_out_count = row_number()) %>%
  ungroup() %>%
  select(IndividualID, TripID, TravDay, in_out_count, TripPurpFrom_B01ID, TripStart_B01ID, main_mode, weighted_trip) %>%
  rename(to_home_trip_id = TripID,
         purpose_to_home = TripPurpFrom_B01ID,
         time_to_home = TripStart_B01ID,
         mode_to_home = main_mode,
         to_home_trip_weight = weighted_trip)

# Pair outward and return legs
from_to_trips <- from_home_trips %>%
  left_join(to_home_trips, by=c('IndividualID','TravDay', 'in_out_count'))

# Count non returns
nr <- from_to_trips %>%
  filter(is.na(purpose_to_home)) %>%
  count()
# 78k before, 57k after - could use some investigation

# Select final variable set
pa_to_od <- from_to_trips %>%
  filter(!is.na(purpose_to_home)) %>%
  select(mode_from_home, purpose_from_home, time_from_home, from_home_trip_weight,
         mode_to_home, purpose_to_home, time_to_home, to_home_trip_weight)

# Classify trips
# Classification type can be 'NTEM' or 'Aggregate'

purp_from_lookup <- read_csv('Y:/NTS/lookups/hb_purpose---TripPurpFrom_B01ID.csv')
purp_to_lookup <- read_csv('Y:/NTS/lookups/hb_purpose---TripPurpTo_B01ID.csv')

# Join from purpose
pa_to_od <- pa_to_od %>%
  left_join(purp_from_lookup, by=c("purpose_from_home"="TripPurpFrom_B01ID")) %>%
  rename(ntem_purpose_from_home = hb_purpose)
# Join to purpose
pa_to_od <- pa_to_od %>%
  left_join(purp_to_lookup, by=c("purpose_to_home"="TripPurpTo_B01ID")) %>%
  rename(ntem_purpose_to_home = hb_purpose)

time_lookup <- read_csv('Y:/NTS/lookups/start_time---TravDay--TripStart_B01ID.csv') %>%
  select(TripStart_B01ID, start_time) %>%
  filter(!start_time %in% c(5,6)) %>%
  distinct()

# Join from time
pa_to_od <- pa_to_od %>%
  left_join(time_lookup, by=c("time_from_home"="TripStart_B01ID")) %>%
  rename(ntem_time_from_home = start_time)
pa_to_od <- pa_to_od %>%
  left_join(time_lookup, by=c("time_to_home"="TripStart_B01ID")) %>%
  rename(ntem_time_to_home = start_time)

# Join to time

weight_out <- pa_to_od %>%
  group_by(mode_from_home, ntem_purpose_from_home, ntem_time_from_home,
           ntem_time_to_home) %>%
  summarise(total_weighted_trips = sum(from_home_trip_weight,na.rm=TRUE))

weight_out %>% write_csv('D:/weight_out.csv')

valid_purpose <- c(1,2,3,4,5,6,7,8)

valid_time <- c(1,2,3,4)

# Filter out outliers
pa_to_od <- pa_to_od %>%
  filter(ntem_purpose_from_home %in% valid_purpose) %>%
  filter(ntem_purpose_to_home %in% valid_purpose) %>%
  filter(ntem_time_from_home %in% valid_time) %>%
  filter(ntem_time_to_home %in% valid_time)

# TODO: Should be a function that does every mode (for mode in target_modes)


target_mode <- 3

# Purpose and time constrained pa to od
mode_pa_to_od <- pa_to_od %>%
  filter(mode_from_home == target_mode) %>%
  filter(mode_to_home == target_mode) %>%
  group_by(ntem_purpose_from_home, ntem_time_from_home,
          ntem_time_to_home) %>%
  summarise(from_home_trip_weight = sum(from_home_trip_weight,na.rm=TRUE)) %>%
  ungroup()

mode_pa_to_od %>% write_csv('Y:/NTS/agg_phi_car_gb.csv')

# Set totals
# Doing this on from trips only, should probably consolidate both sides first
from_home_trips <- mode_pa_to_od %>%
  group_by(purpose_from_home, time_from_home) %>%
  summarise(from_total = sum(from_home_trip_weight,na.rm=TRUE)) %>%
  ungroup()

mode_pa_to_od <- mode_pa_to_od %>%
  left_join(from_home_trips, by=c('purpose_from_home', 'time_from_home')) %>%
  mutate(direction_factor = from_home_trip_weight/from_total)

mode_pa_to_od <- mode_pa_to_od %>%
   select(purpose_from_home, time_from_home, purpose_to_home, time_to_home, direction_factor)

mode_pa_to_od <- acm %>%
  left_join(mode_pa_to_od, by = c(
    'purpose_from_home', 'time_from_home',
    'purpose_to_home', 'time_to_home')) %>%
  mutate(direction_factor = replace_na(direction_factor,0)) %>%
  mutate(direction_factor = round(direction_factor,5))

export_string <- paste0('mode_', target_mode, '_', classification_type, '_p_tp_pa_to_od.csv')

mode_pa_to_od %>% write_csv(paste0('Y:/NTS/', export_string))

# Time & from home constrained only pa to od
mode_pa_to_od <- pa_to_od %>%
  filter(mode_from_home == target_mode) %>%
  filter(mode_to_home == target_mode) %>%
  group_by(purpose_from_home, time_from_home,
           time_to_home) %>%
  summarise(from_home_trip_weight = sum(from_home_trip_weight,na.rm=TRUE)) %>%
  ungroup()

# Set totals
# Doing this on from trips only, should probably consolodate both sides first
p_acm <- acm %>%
  select(purpose_from_home, time_from_home, time_to_home) %>%
  distinct()

from_home_trips <- mode_pa_to_od %>%
  group_by(purpose_from_home, time_from_home) %>%
  summarise(from_total = sum(from_home_trip_weight,na.rm=TRUE)) %>%
  ungroup()

mode_pa_to_od <- mode_pa_to_od %>%
  left_join(from_home_trips, by=c('purpose_from_home', 'time_from_home')) %>%
  mutate(direction_factor = from_home_trip_weight/from_total)

mode_pa_to_od <- mode_pa_to_od %>%
  select(purpose_from_home, time_from_home,
         time_to_home, direction_factor)

mode_pa_to_od <- p_acm %>%
  left_join(mode_pa_to_od, by = c(
    'purpose_from_home',
    'time_from_home',
    'time_to_home')) %>%
  mutate(direction_factor = replace_na(direction_factor,0)) %>%
  mutate(direction_factor = round(direction_factor,5))

export_string <- paste0('mode_', target_mode, '_', classification_type, '_fhp_pa_to_od.csv')

mode_pa_to_od %>% write_csv(paste0('Y:/NTS/', export_string))
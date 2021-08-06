require(tidyverse)

join_lookup <- function(tibble, lookupDir,  key, columns,  fillNA = NA){
  
  
  # Check type of key columns
  leftKey <- names(key)[1]
  rightKey <- key[[1]]
  
  if (is.na(leftKey)|is.null(leftKey)){
    leftKey <- key
    rightKey <- key
  }

  convertKeyType <-
    tibble %>%
    pull(leftKey) %>%
    typeof() %>%
    paste0('as.', .)
  
  lookupTable <- read_csv(lookupDir, col_types = cols(.default = 'c')) %>%
    select(rightKey, columns)
  
  tibble <- tibble %>%
    mutate_at(vars(leftKey), 'as.character') %>%
    left_join(lookupTable, by = key) %>%
    mutate_at(vars(leftKey), convertKeyType) %>%
    mutate_at(vars(columns), ~replace_na(., fillNA))
}

# Code purpose: Build mode specific PA to OD factors

# Import ntem_build - NTEM segmented dataset
# TODO: Update import
nts_ntem_df <- read_csv('Y:/NTS/tfn_ntem_build.csv', guess_max = 10^5) #Why 10^9, its just checking the column type

# Add trip weighting
nts_ntem_df <- nts_ntem_df %>%
  mutate(weighted_trip = W1 * W5xHh * W2)

# Get a subset
test <- nts_ntem_df %>%
  select(IndividualID, TripID, TravDay, TripPurpFrom_B01ID,
         TripPurpTo_B01ID, TripStart_B01ID, TripEnd_B01ID,
         main_mode, weighted_trip) %>%
  filter(!is.na(weighted_trip)) %>%
  group_by(IndividualID, TravDay) %>%
  mutate(trip_order = row_number()) %>%
  ungroup()

# Put return trips from previous day into previous day

test <- test %>%
  mutate(TravDay = case_when(
    TripPurpTo_B01ID == 23 & trip_order == 1 ~ TravDay-1, # 9	Elementary occupations
    TRUE ~ TravDay)
  )
  

# Get from home
from_home_trips <- test %>%
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
classification_type <- 'NTEM'

if (classification_type == 'NTEM'){

  pa_to_od <- pa_to_od %>%
    join_lookup(
      lookupDir = 'lookup/travel_purpose.csv', 
      key = c('purpose_from_home'='travel_purpose'),
      columns = 'travel_purpose_NTEM',
      fillNA = 'unclassified') %>%
    select(everything(), -purpose_from_home, purpose_from_home = travel_purpose_NTEM)


  pa_to_od <- pa_to_od %>%
    join_lookup(
      lookupDir = 'lookup/travel_purpose.csv', 
      key = c('purpose_to_home'='travel_purpose'),
      columns = 'travel_purpose_NTEM',
      fillNA = 'unclassified') %>%
    select(everything(), -purpose_to_home, purpose_to_home = travel_purpose_NTEM)

  valid_purpose <- c('1','2','3','4','5','6','7','8')

} else if (classification_type == 'aggregate'){
  
  pa_to_od <- pa_to_od %>%
    join_lookup(
      lookupDir = 'lookup/travel_purpose.csv', 
      key = c('purpose_from_home'='travel_purpose'),
      columns = 'travel_purpose_aggr',
      fillNA = 'unclassified') %>%
    select(everything(), -purpose_from_home, purpose_from_home = travel_purpose_aggr)
  

 pa_to_od <- pa_to_od %>%
    join_lookup(
      lookupDir = 'lookup/travel_purpose.csv', 
      key = c('purpose_to_home'='travel_purpose'),
      columns = 'travel_purpose_aggr',
      fillNA = 'unclassified') %>%
   select(everything(), -purpose_to_home, purpose_to_home = travel_purpose_aggr)

  valid_purpose <- c('commute','business','other')

}
Time

valid_time <- c(1,2,3,4,5,6)

# Filter out outliers
pa_to_od <- pa_to_od %>%
  filter(purpose_from_home %in% valid_purpose) %>%
  filter(purpose_to_home %in% valid_purpose) %>%
  filter(time_from_home %in% valid_time) %>%
  filter(time_to_home %in% valid_time)

# Build all combo matrix
purpose_df <- tibble(valid_purpose) %>%
  mutate(ph = 1)
time_df <- tibble(valid_time) %>%
  mutate(ph = 1)
acm <- purpose_df %>%
  left_join(time_df, by='ph')
acm <- acm %>%
  left_join(acm, by='ph')
acm <- acm %>%
  rename(purpose_from_home = valid_purpose.x,
         time_from_home = valid_time.x,
         purpose_to_home = valid_purpose.y,
         time_to_home = valid_time.y) %>%
  select(-ph)

# TODO: Should be a function that does every mode (for mode in target_modes)

target_mode <- 3

# Purpose and time constrained pa to od
mode_pa_to_od <- pa_to_od %>%
  filter(mode_from_home == target_mode) %>%
  filter(mode_to_home == target_mode) %>%
  group_by(purpose_from_home, time_from_home,
           purpose_to_home, time_to_home) %>%
  summarise(from_home_trip_weight = sum(from_home_trip_weight,na.rm=TRUE)) %>%
  ungroup()

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
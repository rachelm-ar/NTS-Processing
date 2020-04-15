require(tidyverse)

# Code purpose: Build mode specific PA to OD factors

# Import ntem_build - NTEM segmented dataset 
nts_ntem_df <- read_csv('Y:/NTS/tfn_ntem_build.csv', guess_max = 10^9)

target_modes = c(3,6)

# Trip length in miles
miles_test <- nts_ntem_df %>%
  select(TripDisIncSW, main_mode) %>%
  filter(TripDisIncSW < 20 & main_mode == 3)
hist(miles_test$TripDisIncSW)
miles_test <- miles_test %>%
  group_by(TripDisIncSW) %>%
  count()

export <- 'Y:/NTS/trip_lengths/'

# Add trip weighting
nts_ntem_df <- nts_ntem_df %>%
  mutate(weighted_trip = W1 * W5xHh * W2)

# Distribute the whole numbers of miles around the average
whole_trips <- nts_ntem_df %>%
  filter(TripDisIncSW %% 1 == 0 & TripDisIncSW >=1) %>%
  arrange(TripDisIncSW) %>%
  mutate(dis_first_miles = TripDisIncSW-1,
         row_number = row_number(),
         mod_1 = case_when(
           row_number %% 3 == 0 ~ 3,
           row_number %% 2 == 0 ~ 2,
           TRUE ~ 1
         )) %>%
  mutate(TripDisIncSW_spread = case_when(
    mod_1 == 1 ~ (TripDisIncSW-1) + 0.75,
    mod_1 == 2 ~ (TripDisIncSW-1) + 1,
    mod_1 == 3 ~ (TripDisIncSW-1) + 1.25
  )) %>%
  select(-dis_first_miles, row_number, mod_1)

# test_mods <- whole_trips %>%
#  select(mod_1) %>%
#  group_by(mod_1) %>%
#  count()

partial_trips <- nts_ntem_df %>%
  filter(TripDisIncSW %% 1 != 0 | TripDisIncSW <1) %>%
  mutate(TripDisIncSW_spread = TripDisIncSW)

nts_ntem_df <- bind_rows(whole_trips, partial_trips) %>%
  arrange(TripDisIncSW_spread)

# Subset down
# Not spread just now
trip_length_subset <- nts_ntem_df %>%
  select(SurveyYear, TravDay, HHoldOSLAUA_B01ID, soc_cat, ns_sec, main_mode, hb_purpose, nhb_purpose,
         trip_origin, TripDisIncSW, TripDisIncSW_spread, weighted_trip) %>%
  filter(!is.na(weighted_trip)) %>%
  mutate(trip_dist_km = TripDisIncSW_spread*1.60934)

# North only
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

# Weekdays only
weekdays <- c(1,2,3,4,5)
# Last 3 years only
years <- c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
           2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)

trip_length_subset <- trip_length_subset %>%
  filter(HHoldOSLAUA_B01ID %in% north_la)
# Removed years & weekdays for fullest possible sample

## Working method
# max north trip based on internal distance measure
max_north_trip <- '500'
# Define band shares to give rough lognormal
# TODO: Can be done with lognorm fucntion based on mean & sd - just not sure how to get that into quartiles.
standard_quarts <- c(0.01, 0.31, 0.57, 0.76, 0.86, 0.92, 0.96, 0.98, 0.99, 1)
fallback_quarts <- c(0.01, 0.31, 0.57, 0.76, 0.86)

# Highway single bands
# TODO: Smart breaks

# Import target combos for parameters - loops will give segments that we don't want
target_params <- read_csv(paste0(export, "/target_output_params.csv"))

for(i in 1:nrow(target_params)){
  
  # Get params from row
  purpose_sub <- as.character(target_params$purpose[[i]])
  mode_sub <- target_params$mode[[i]]
  soc_cat_sub <- target_params$soc_cat[[i]]
  ns_sec_cat_sub <- target_params$ns_sec[[i]]

  # Give segment a name
  if(is.na(ns_sec_cat_sub)) {
    segment <- paste0('soc', soc_cat_sub)
  } else{
    segment <- paste0('ns', ns_sec_cat_sub)
  }

  # Define param name
  param_name <- paste0('p', purpose_sub,
                       '_m', mode_sub,
                       '_',
                       segment)
  print(param_name)

  # Subset to target distribution
  trip_lengths <- trip_length_subset %>%
    filter(trip_origin == 'hb' &
             hb_purpose == purpose_sub &
             main_mode==mode_sub &
             trip_dist_km <= max_north_trip)
  
  # Filter soc or sec
  if(is.na(ns_sec_cat_sub)) {
    trip_lengths <- trip_lengths %>%
      filter(soc_cat == soc_cat_sub)
  } else{
    trip_lengths <- trip_lengths %>%
      filter(ns_sec == ns_sec_cat_sub)
    segment <- paste0('ns', ns_sec_cat_sub)
  }
  
  # get set length for sample size oversight
  sample_size = nrow(trip_lengths)
  print(paste("Number of records:", sample_size))

  # If this is less than 20, reduce the number of quarts
  if(sample_size < 20){
    quarts <- fallback_quarts
  } else{
    quarts <- standard_quarts
  }

  # Get trip lengths only
  tlo <- trip_lengths$trip_dist_km
  tlo_mean <- mean(tlo)
  tlo_sd <- sd(tlo)

  breaks <- c(0, quantile(tlo, probs=quarts))
  names(breaks) <- NULL
  
  # Name breaks
  tags <- NULL
  for (b in 1:length(breaks)){
    tags <- c(tags, paste0("(", ifelse(b-1==0,0,breaks[[b-1]]), "-", breaks[[b]], "]"))
  }
  tags <- tags[2:length(tags)]
  
  # Build placeholders for join
  tlb_desc <- tags %>%
    as.tibble() %>%
    rename(tlb_desc = value) %>%
    mutate(ph=1)
  
  hb_purpose <- purpose_sub %>%
    as.tibble() %>%
    rename(hb_purpose = value) %>%
    mutate(ph=1)
  
  hb_placeholder <- tlb_desc %>%
    left_join(hb_purpose) %>%
    select(-ph) %>%
    distinct() %>%
    group_by(hb_purpose) %>%
    mutate(tlb_index = row_number(),
           main_mode = mode_sub) %>%
    ungroup() %>%
    select(tlb_index, tlb_desc, hb_purpose, main_mode)

  # bucketing values into bins
  trip_lengths <- trip_lengths %>%
    select(-nhb_purpose) %>%
    mutate(tlb_index = cut(trip_dist_km, 
                           breaks=breaks, 
                           include.lowest=TRUE, 
                           right=TRUE, 
                           labels=FALSE)) %>%
    filter(hb_purpose != 99 & !is.na(tlb_index)) %>%
    group_by(tlb_index, main_mode, hb_purpose) %>%
    summarise(trips = sum(weighted_trip, na.rm=TRUE),
              atl = weighted.mean(trip_dist_km, weighted_trip, na.rm=TRUE)) %>%
    ungroup() %>%
    select(tlb_index, main_mode, hb_purpose, trips, atl)

  trip_lengths <- hb_placeholder %>%
    full_join(trip_lengths) %>%
    mutate(trips = replace_na(trips, 0))
  
  # Get % share for each number of trips
  totals <- trip_lengths %>%
    select(tlb_desc, hb_purpose, trips) %>%
    group_by(hb_purpose) %>%
    summarise(total_trips = sum(trips,na.rm=TRUE))
  
  # Reattach - derive total
  trip_lengths <- trip_lengths %>%
    left_join(totals) %>%
    mutate(band_share = round(trips/total_trips, 3)) %>%
    select(-total_trips)

  hist(trip_lengths$band_share, breaks=breaks)

  trip_lengths %>% write_csv(paste0(export, "tlb_", param_name, ".csv"))
}

# TODO: same for NHB
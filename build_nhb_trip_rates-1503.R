library(tidyverse)

nts_dir <- "Y:/NTS/"
lookup_dir <- str_c(nts_dir, "lookups/")

# Read in classified output
classified_build <- read_csv('Y:/NTS/import/classified_nts_pre-weighting.csv')

# Lookup functions
source(str_c(lookup_dir,"lookups.r"))

classified_build <- classified_build %>%
  mutate(trip_weights = W1 * W5 * W2) %>%
  lu_ca()

# Set up hb and nhb mode
classified_build <- classified_build %>%
  mutate(hb_mode = main_mode,
         nhb_mode = main_mode,
         )


# What's going on with proportions
classified_build %>%
  group_by(trip_origin) %>%
  count()

# NEED home based mode
# SOC CATS
hb_trips <- classified_build %>%
  filter(trip_origin == 'hb') %>%
  filter(TravDay %in% c(1,2,3,4,5)) %>%
  select(trip_origin,
         hb_purpose,
         hb_mode,
         soc_cat,
         ns_sec,
         trip_weights) %>%
  group_by(trip_origin,
           hb_purpose,
           hb_mode,
           soc_cat,
           ns_sec) %>%
  summarise(hb_trips = sum(trip_weights)) %>%
  ungroup() %>%
  select(-trip_origin)

nhb_trips <- classified_build %>%
  filter(trip_origin == 'nhb') %>%
  filter(nhb_purpose != 99) %>%
  filter(TravDay %in% c(1,2,3,4,5)) %>%
  select(trip_origin,
         nhb_mode,
         nhb_purpose_hb_leg,
         nhb_purpose,
         soc_cat,
         ns_sec,
         trip_weights) %>%
  group_by(trip_origin,
           nhb_mode,
           nhb_purpose_hb_leg,
           nhb_purpose,
           soc_cat,
           ns_sec) %>%
  summarise(nhb_trips = sum(trip_weights)) %>%
  ungroup() %>%
  rename(hb_purpose = nhb_purpose_hb_leg) %>%
  select(-trip_origin)


# Separate out and retain soc somehow 

soc_hb <- hb_trips %>%
  select(-ns_sec) %>%
  filter(hb_purpose %in% c(1,2)) %>%
  group_by(hb_purpose, hb_mode, soc_cat) %>%
  summarise(hb_trips = sum(hb_trips, na.rm=TRUE)) %>%
  ungroup()

ns_sec_hb <- hb_trips %>%
  select(-soc_cat) %>%
  filter(hb_purpose %in% c(3,4,5,6,7,8)) %>%
  group_by(hb_purpose, hb_mode, ns_sec) %>%
  summarise(hb_trips=sum(hb_trips, na.rm = TRUE)) %>%
  ungroup()

soc_nhb <- nhb_trips %>%
  select(-ns_sec) %>%
  filter(nhb_purpose == 12) %>%
  group_by(nhb_mode, hb_purpose, nhb_purpose, soc_cat) %>%
  summarise(nhb_trips=sum(nhb_trips, na.rm = TRUE)) %>%
  ungroup()

ns_sec_nhb <- nhb_trips %>%
  select(-soc_cat) %>%
  filter(nhb_purpose != 12) %>%
  group_by(nhb_mode, hb_purpose, nhb_purpose, ns_sec) %>%
  summarise(nhb_trips = sum(nhb_trips, na.rm = TRUE)) %>%
  ungroup()

# Bind drop null segs, infill NA
# 99 == SOC 0, need join value for NA so using 999
hb_trips <- bind_rows(soc_hb, ns_sec_hb) %>%
  mutate(soc_cat = case_when(soc_cat == 99 ~ 0,
                             is.na(soc_cat) ~ 999,
                             TRUE ~ soc_cat)) %>%
  mutate(ns_sec = case_when(is.na(ns_sec) ~ 999,
                            TRUE ~ ns_sec))

nhb_trips <- bind_rows(soc_nhb, ns_sec_nhb) %>%
  mutate(soc_cat = case_when(soc_cat == 99 ~ 0,
                             is.na(soc_cat) ~ 999,
                             TRUE ~ soc_cat)) %>%
  mutate(ns_sec = case_when(is.na(ns_sec) ~ 999,
                            TRUE ~ ns_sec))

# Get trip rate
trip_rates <- hb_trips %>%
  left_join(nhb_trips, by=c('hb_purpose','soc_cat','ns_sec')) %>%
  mutate(trip_rate = (nhb_trips/hb_trips)) %>%
  filter(ns_sec != -9,
         hb_purpose != 99,
         nhb_purpose != 99) %>%
  rename(p = hb_purpose,
         nhb_p = nhb_purpose)

trip_rates %>% write_csv(paste0(nts_dir, 'outputs/nhb_ave_wday_enh_trip_rates_TEST.csv'))


####
## SOC 0 - default

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

soc_zero_trip_rates <- hb_trips %>%
  left_join(nhb_trips, by=c('tfn_area_type','ca', 'hb_purpose')) %>%
  mutate(trip_rate = (nhb_trips/hb_trips)) %>%
  filter(nhb_purpose %in% c(12)) %>%
  filter(hb_purpose != 99) %>%
  filter(nhb_purpose != 99) %>%
  rename(p = hb_purpose) %>%
  rename(nhb_p = nhb_purpose) %>%
  rename(area_type = tfn_area_type) %>%
  select(-hb_trips, -nhb_trips) %>%
  mutate(ns_sec = 99, soc_cat = 0)

trip_rates <- bind_rows(soc_zero_trip_rates, soc_trip_rates, ns_trip_rates)

# Benchmarking
# Should be:
# SOC - 8 area type * 2 ca * 8 hb purpose * 1 * nhb purpose * 4 soc cat
# NS - 8 area type * 2 ca * 8 hb purpose * 5 nhb purpose * 5 ns cat
(8*2*8*1*4) + (8*2*8*5*5)

group_count <- trip_rates %>%
  group_by(area_type, ca, p, nhb_p) %>%
  count()

# TODO: Build all combos - look for missing values, infill with lowest value from next highest level
# Outputs 1 - filled in all combo nhb tr vector length = total above = 3712
#         2 - Infill values only - just for sense check - biggest risk is infill value is too high

trip_rates %>% write_csv(paste0(nts_dir, 'outputs/nhb_ave_wday_enh_trip_rates.csv'))

# Post processing - replace 99 with 'none' - rename soc_cat = soc, ns_sec to ns
# TODO: if you can do that in R is saves a job but might be awkwartd with data types

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

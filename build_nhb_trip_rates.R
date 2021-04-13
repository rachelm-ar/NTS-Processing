library(tidyverse)

nts_dir <- "Y:/NTS/"
lookup_dir <- str_c(nts_dir, "lookups/")

# Lookup functions
source(str_c(lookup_dir,"lookups.r"))

# BACKLOG: Look at getting in SIC & SOC (can be in time split, not here)

# Read in classified output
classified_build <- read_csv('Y:/NTS/import/classified builds/classified_build.csv', guess_max=1000) %>%
  # Apply standard trip weighting
  mutate(trip_weights = W1 * W5 * W2)

# Read in NTEM CTripEnd rates
ntem_nhb <- read_csv('I:/NorMITs Synthesiser/Docs/Ecosystem DfT NTEM/TripRates_for_TfN/IgammaNMHM/IgammaNMHM.csv')
# Rename cols to match
ntem_nhb <- ntem_nhb %>%
  rename(nhb_purpose = N) %>%
  rename(nhb_mode = M) %>%
  rename(hb_purpose = H) %>%
  rename(hb_mode = HBM) %>%
  rename(trip_rate_ntem = Gamma)

classified_build %>% count(SurveyYear)

retain_cols <- c("IndividualID", 
                 "soc_cat", 
                 "ns_sec", 
                 "TripID", 
                 "TravelWeekDay_B01ID", 
                 "main_mode", 
                 "TripPurpFrom_B01ID",
                 "TripPurpTo_B01ID", 
                 "TripStart_B01ID", 
                 "TripEnd_B01ID", 
                 "hb_purpose", 
                 "nhb_purpose", 
                 "trip_weights",
                 "tfn_area_type")

cb_sub <- classified_build %>%
  select(retain_cols) %>%
  distinct()

### Proof of concept for NHB method ###
# Get 1 travel diary - # IndividualID == 2019016037

# TODO: Weekday only

tour_groups <- cb_sub %>%
  filter(TravelWeekDay_B01ID %in% 1:5) %>% 
  unique() %>%
  arrange(IndividualID, TripID) %>%
  group_by(IndividualID) %>%
  mutate(start_flag = case_when(TripPurpFrom_B01ID == 23 ~ 1,
                                TRUE ~ 0)) %>%
  mutate(end_flag = case_when(TripPurpTo_B01ID == 23 ~ 1,
                              TRUE ~ 0)) %>%
  mutate(trip_group = cumsum(start_flag)) %>%
  ungroup()

### Spotcheck some individuals
individuals <- tour_groups %>% distinct(IndividualID)

# Note - trip ID has to go
hb_trips <- tour_groups %>%
  select(IndividualID, hb_purpose, tfn_area_type ,main_mode, trip_group, start_flag, trip_weights) %>%
  filter(start_flag == 1) %>%
  rename(hb_mode = main_mode)

nhb_trips <- tour_groups %>%
  select(IndividualID, nhb_purpose, tfn_area_type, main_mode, trip_group, start_flag, end_flag, trip_weights) %>%
  filter(start_flag == 0 & end_flag == 0) %>%
  rename(nhb_mode = main_mode)

hb_leg_info <- hb_trips %>%
  select(IndividualID, hb_purpose, tfn_area_type, hb_mode, trip_group)

nhb_w_hb <- nhb_trips %>%
  left_join(hb_leg_info, by=c("IndividualID", "trip_group", "tfn_area_type"))

# Group and sum out the identifiers and the people
hb_totals <- hb_trips %>%
  group_by(hb_purpose, hb_mode, tfn_area_type) %>%
  summarise(hb_trips = sum(trip_weights, na.rm=TRUE)) %>%
  ungroup()

nhb_totals <- nhb_w_hb %>%
  group_by(nhb_purpose, nhb_mode, hb_purpose, hb_mode, tfn_area_type) %>%
  summarise(nhb_trips = sum(trip_weights, na.rm=TRUE)) %>%
  ungroup()

# Turn into trip rate (nhb by p/m/nhbp/nhbm divided by hb by p/m)
nhb_trip_rates <- nhb_totals %>%
  left_join(hb_totals, by=c('hb_purpose', 'hb_mode', 'tfn_area_type')) %>%
  mutate(nhb_trip_rate = nhb_trips/hb_trips)

# Format for output
nhb_trip_rates <- nhb_trip_rates %>%
  filter(!is.na(hb_purpose))

# Check against NTEM rates (IgammaNHBH)
ntem_comparison <- nhb_trip_rates %>%
  left_join(ntem_nhb, by=c('hb_purpose', 'hb_mode', 'nhb_purpose', 'nhb_mode'))

# Format for output

# Write out
ntem_comparison %>% write_csv(paste0(nts_dir, 'outputs/nhb_ave_wday_ntem.csv'))

nhb_trip_rates %>% write_csv(paste0(nts_dir, 'outputs/nhb_ave_wday_enh_trip_rates.csv'))

# TODO: Revisit time split - using at least variables from NTS
# DfT variables: params [n = nhb_p, m = nhb_m, r = tfn area type of hb origin], var [d = time split (1:4 AM, IP, PM, OP)]
# TODO: Peg hb area type

time_splits <- process # placeholder



time_splits %>% write_csv(paste0(nts_dir, 'outputs/tfn_nhb_ave_wday_time_split_18.csv'))

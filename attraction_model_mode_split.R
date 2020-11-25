library(tidyverse)
library(lemon)

# NTS folder
nts_dir <- "Y:/NTS/"

# Imports folder
import_dir <- str_c(nts_dir, "import/")

# Outputs folder
output_dir <- str_c(nts_dir, "outputs/attraction_mode_split/")

# Lookups
lookup_dir <- str_c(nts_dir, "lookups/")

# Lookup functions
source(str_c(lookup_dir,"lookups.r"))

# Unclassified build
classified_build_dir <- str_c(import_dir, "classified_nts_pre-weighting_child_gender.csv") 

# At to MSOA lookup
at_to_msoa_csv <- str_c(lookup_dir, 'at_to_msoa_lookup.csv') 

# Read classified_pre_weighted trip rates
classified_build <- read_csv(classified_build_dir)

# Read in AT to MSOA lookup
at_to_msoa_lookup <- read_csv(at_to_msoa_csv)


# By Area Type ------------------------------------------------------------
# Filter NA & DNA area types and HB trips only
# Find number of trips for each area by mode and trip purpose
# For each zone and trip purpose calculate the mode share split 
at_mode_share <- classified_build %>% 
  select(TripDestAreaType2_B01ID, trip_purpose, main_mode, W5xHh) %>% 
  filter(!TripDestAreaType2_B01ID %in% c(-10,-8), trip_purpose %in% 1:8) %>% 
  group_by(TripDestAreaType2_B01ID, main_mode, trip_purpose) %>% 
  summarise(trips = sum(W5xHh, na.rm = TRUE)) %>% 
  group_by(TripDestAreaType2_B01ID, trip_purpose) %>% 
  mutate(mode_share = trips/sum(trips)) %>% 
  ungroup()

# Area type to MSOA lookup
at_to_msoa <- at_to_msoa_lookup %>% 
  select(msoa11cd, TripDestAreaType2_B01ID)

# Join so each msoa should a unique area type
at_mode_share <- at_analysis %>%
  mutate(TripDestAreaType2_B01ID = case_when(
    TripDestAreaType2_B01ID %in% 12:13 ~ '12+13',
    TripDestAreaType2_B01ID %in% 14:15 ~ '14+15',
    TRUE ~ as.character(TripDestAreaType2_B01ID)
  ))

attractions_mode_split <- at_mode_share %>%
  left_join(at1, by = 'TripDestAreaType2_B01ID') %>% 
  select(msoa11cd, trip_purpose, main_mode, mode_share) %>%
  rename(msoa = msoa11cd, p = trip_purpose, m = main_mode) %>% 
  complete(nesting(msoa, p),
           m = c(1,2,3,5,6),
           fill = list(mode_share = 0))

attractions_mode_split %>% write_csv(str_c(output_dir, 'attraction_mode_split.csv'))

at_mode_share %>% 
  ggplot(aes(mode_share)) +
  geom_histogram() +
  facet_wrap(TripDestAreaType2_B01ID ~ main_mode)

# Other Analysis including UA ---------------------------------------------

# UA
ua_analysis <- classified_build %>%
  select(TripDestUA2009_B01ID, trip_purpose, main_mode, W5xHh) %>% 
  filter(!TripDestUA2009_B01ID %in% c(-10,-8), trip_purpose %in% 1:8) %>% 
  group_by(TripDestUA2009_B01ID, main_mode, trip_purpose) %>% 
  summarise(trips = sum(W5xHh, na.rm = TRUE)) %>% 
  group_by(TripDestUA2009_B01ID, trip_purpose) %>% 
  mutate(mode_share = trips/sum(trips)) %>% 
  ungroup()

ua_plots <- ua_analysis %>%
  ggplot(aes(mode_share)) +
  geom_histogram() +
  facet_rep_wrap(. ~ main_mode, repeat.tick.labels = TRUE) +
  ggtitle("UA Mode Share") +
  theme(plot.title = element_text(hjust = 0.5))

ua_plots +
  ggsave(paste0(output_dir, 'ua_mode_share.jpg'))

ua_analysis %>% 
  group_by(main_mode) %>% 
  summarise(mean_mode_share = mean(mode_share))

ua_analysis %>% 
  group_by(TripDestUA2009_B01ID, main_mode) %>% 
  summarise(trips_at = sum(trips),
            mean_at_mode_share = mean(mode_share)) %>%
  mutate(main_mode = as.factor(main_mode)) %>% 
  ggplot(aes(main_mode,mean_at_mode_share)) +
  geom_bar(stat = 'identity') +
  facet_rep_wrap(. ~ TripDestUA2009_B01ID, repeat.tick.labels = TRUE)

# AT
at_poc <- at_analysis %>%
  filter(main_mode == 3, trip_purpose ==4)

at_plots <- at_analysis %>%
  ggplot(aes(mode_share)) +
  geom_histogram() +
  facet_rep_wrap(. ~ main_mode, repeat.tick.labels = TRUE) +
  ggtitle("AT Mode Share") +
  theme(plot.title = element_text(hjust = 0.5))

at_plots +
  ggsave(paste0(output_dir, 'at_mode_share.jpg'))

at_analysis %>% 
  group_by(main_mode) %>% 
  summarise(mean_mode_share = mean(mode_share))

# UA BY AT
ua_by_area_type1 <- classified_build %>%
  filter(!TripDestUA2009_B01ID %in% c(-10,-8), trip_purpose %in% 1:8) %>% 
  select(TripDestUA2009_B01ID, TripDestAreaType1_B01ID) %>%
  distinct() %>%
  count(TripDestUA2009_B01ID)

ua_at_analysis <- classified_build %>% 
  select(TripDestAreaType2_B01ID, TripDestUA2009_B01ID, trip_purpose, main_mode, W5xHh) %>% 
  filter(!TripDestUA2009_B01ID %in% c(-10,-8), trip_purpose %in% 1:8) %>% 
  group_by(TripDestUA2009_B01ID, TripDestAreaType2_B01ID, main_mode, trip_purpose) %>% 
  summarise(trips = sum(W5xHh, na.rm=TRUE), sample = n()) %>% 
  group_by(TripDestUA2009_B01ID, TripDestAreaType2_B01ID, trip_purpose) %>% 
  mutate(mode_share = trips/sum(trips)) %>% 
  ungroup()

manchester_poc <- ua_at_analysis %>%
  filter(TripDestUA2009_B01ID == 260 & trip_purpose==1 & main_mode == 3)

ua_at_plots <- ua_at_analysis %>% 
  ggplot(aes(mode_share)) +
  geom_histogram() +
  facet_rep_wrap(. ~ main_mode, repeat.tick.labels = TRUE) +
  ggtitle("UA AT Mode Share") +
  theme(plot.title = element_text(hjust = 0.5))

ua_at_plots +
  ggsave(paste0(output_dir, 'ua_at_mode_share.jpg'))

ua_at_analysis %>%
  group_by(TripDestAreaType2_B01ID, main_mode) %>% 
  summarise(trips_at = sum(trips),
            mean_at_mode_share = mean(mode_share)) %>%
  mutate(main_mode = as.factor(main_mode)) %>% 
  ggplot(aes(main_mode,mean_at_mode_share)) +
  geom_bar(stat = 'identity') +
  facet_rep_wrap(. ~ TripDestAreaType2_B01ID, repeat.tick.labels = TRUE)

ua_at_analysis %>%
  group_by(TripDestUA2009_B01ID, main_mode) %>% 
  summarise(trips_at = sum(trips),
            mean_at_mode_share = mean(mode_share)) %>%
  mutate(main_mode = as.factor(main_mode)) %>% 
  ggplot(aes(mean_at_mode_share)) +
  geom_histogram() +
  facet_rep_wrap(. ~ TripDestUA2009_B01ID, repeat.tick.labels = TRUE)


ua_by_area_type2 <- classified_build %>%
  filter(!TripDestUA2009_B01ID %in% c(-10,-8), trip_purpose %in% 1:8) %>% 
  select(TripDestUA2009_B01ID, TripDestAreaType2_B01ID) %>%
  distinct() %>%
  count(TripDestAreaType2_B01ID)



# Look at outputs by mode share - what is telling us?
# Try again with trip purpose in (1:8)
# Potentially combine best of both
# Expecting that UA has good 'local flavour' - local differences
# Expecting area type will align with tfn area type
# Factor that describes mode share of attractions, ie. does certain areas have certain attraction profiles
# Can you interweave UA and area type to get to seomthing that translates to MSOA

# Current attracn function is: A = Jobs*AttrWeight * SOC weight (1 = 0.2, 2 = .4, 3. .4)
# Wants to be: A = Jobs*AttrWeight * SOC weight * Mode weight (Car = .6, Rail.0.5 ,etc) - if at UA, then fine

# Joining the two zone systems
# Get results at combo level
# Infill any missing with average of UA & AT
# Get correspondence between UA 2009 and NTS Area types
# Get correspondence between MSOA and UA (have) and MSOA and NTS AT
# Assign mode split to MSOA based on best fit or population match, use pure UA or pure AT where there's gaps

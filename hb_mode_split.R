library("tidyverse")

# NTS folder
nts_dir <- "Y:/NTS/"

# Imports folder
import_dir <- str_c(nts_dir, "import/")

# Outputs folder
output_dir <- str_c(nts_dir, "outputs/hb_mode_split/")

# Lookups
lookup_dir <- str_c(nts_dir, "lookups/")

# Lookup functions
source(str_c(lookup_dir,"lookups.r"))

# Unclassified build
classified_build_dir <- str_c(import_dir, "classified_nts_pre-weighting.csv") 

# Read classified_pre_weighted trip rates
classified_build <- read_csv(classified_build_dir)

classified_build %>% count(main_mode)

# Filter for hb purposes, remove london underground, redefine area types and define car availability
classified_build1 <- classified_build %>%
  filter(trip_purpose %in% 1:8) %>%
  mutate(tfn_area_type = ifelse(tfn_area_type %in% c(1,2), 1, tfn_area_type)) %>%
  mutate(ca = ifelse(cars == 1, 0, 1))

# Redefine area type with both 1 and 2 as the same
merge_temp <- classified_build1 %>% filter(tfn_area_type == '1') %>% mutate(tfn_area_type = '2')
classified_build1 <- rbind(classified_build1, merge_temp)

classified_build1 %>%
  count(tfn_area_type)

# Weight and summarise to mode splits
mode_df <- classified_build %>%
  mutate(weighted_trips = W1 * W2 * W5xHh) %>%
  select(IndividualID, hb_purpose, ca, tfn_area_type, main_mode, weighted_trips, W2) %>%
  group_by(IndividualID, hb_purpose, ca, tfn_area_type, main_mode, W2) %>%
  summarise(weighted_trips = sum(weighted_trips, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(trips = weighted_trips/W2) %>%
  select(hb_purpose, ca, tfn_area_type, main_mode, trips) %>%
  group_by(hb_purpose, ca, tfn_area_type, main_mode, .drop = FALSE) %>%
  summarise(mode_trips = sum(trips, na.rm = TRUE)) %>%
  group_by(hb_purpose, ca, tfn_area_type, .drop = FALSE) %>%
  mutate(total_trips = sum(mode_trips), 
         mode_split = mode_trips/total_trips) %>%
  ungroup()

# Transform into wide format
mode_df <- mode_df %>%
  rename(p = hb_purpose, area_type = tfn_area_type) %>%
  select(p, ca, area_type, main_mode, mode_split) %>%
  pivot_wider(names_from = main_mode, values_from = mode_split, names_prefix = "m") %>%
  select(area_type, ca, p, everything()) %>%
  arrange(area_type, ca, p)

mode_df %>% write_csv(str_c(output_dir, "hb_mode_split.csv"))
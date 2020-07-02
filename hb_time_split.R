library("tidyverse")

# NTS folder
nts_dir <- "Y:/NTS/"

# Imports folder
import_dir <- str_c(nts_dir, "import/")

# Outputs folder
output_dir <- str_c(nts_dir, "outputs/hb_time_split/")

# Lookups
lookup_dir <- str_c(nts_dir, "lookups/")

# Lookup functions
source(str_c(lookup_dir,"lookups.r"))

# Unclassified build
classified_build_dir <- str_c(import_dir, "classified_nts_pre-weighting.csv") 

# Read classified_pre_weighted trip rates
classified_build <- read_csv(classified_build_dir)

# Filter for hb purposes, remove london underground, redefine area types and gender for children
classified_build1 <- classified_build %>%
  filter(trip_purpose %in% 1:8) %>%
  mutate(tfn_area_type = ifelse(tfn_area_type %in% c(1,2), 1, tfn_area_type),
         gender = ifelse(age_work_status == 1, 2, gender))

# Redefine area type with both 1 and 2 as the same
merge_temp <- classified_build1 %>% filter(tfn_area_type == '1') %>% mutate(tfn_area_type = '2')
classified_build1 <- rbind(classified_build1, merge_temp)

# Add traveller types by combining underlying variables and joining a lookup
classified_build1 <- classified_build1 %>%
  unite("traveller_type_char", "age_work_status", "gender", "hh_adults", "cars", remove=FALSE, sep="_") %>%
  lu_traveller_type()

time_df <- classified_build1 %>%
  mutate(weighted_trips = W1 * W2 * W5xHh) %>%
  select(IndividualID, hb_purpose, traveller_type, tfn_area_type, start_time, weighted_trips, W2) %>%
  group_by(IndividualID, hb_purpose, traveller_type, tfn_area_type, start_time, W2) %>%
  summarise(weighted_trips = sum(weighted_trips, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(trips = weighted_trips/W2) %>%
  select(hb_purpose, traveller_type, tfn_area_type, start_time, trips) %>%
  group_by(hb_purpose, traveller_type, tfn_area_type, start_time, .drop = FALSE) %>%
  summarise(tp_trips = sum(trips, na.rm = TRUE)) %>%
  group_by(hb_purpose, traveller_type, tfn_area_type, .drop = FALSE) %>%
  mutate(total_trips = sum(tp_trips),
         time_split = tp_trips/total_trips) %>%
  ungroup()

# Transform into wide format and fill in missing values
time_df <- time_df %>%
  rename(p = hb_purpose, area_type = tfn_area_type) %>%
  select(p, traveller_type, area_type, start_time, time_split) %>%
  mutate_at(.vars = vars("p", "traveller_type", "area_type", "start_time"),
            .funs = as.integer) %>%
  mutate(p = as.integer(p), area_type = as.integer(area_type), start_time = as.integer(start_time)) %>%
  complete(p = 1:8, traveller_type = 1:88, start_time = 1:6, area_type = 1:8, fill = list(time_split = 0)) %>%
  pivot_wider(names_from = start_time, values_from = time_split, names_prefix = "tp") %>%
  arrange(area_type, traveller_type, p) %>%
  select(area_type, traveller_type, p, tp1, tp2, tp3, tp4, tp5, tp6)

time_df %>% write_csv(str_c(output_dir, "hb_time_split.csv"))
# Compare Unclassified_build processed data to ntem

library("readr")
library("dplyr")
library("reshape2")

# Read in processed data before applying weighting methodology
processed_data <- read_csv("Y:/NTS/classified_nts_walk_infill.csv")

# Read in ntem
ntem <- read_csv("Y:/NorMITs Synthesiser/import/ntem_trip_rates_2016.csv")

# Recode mode for when reshaping to wide format
processed_data <- processed_data %>%
  filter(main_mode != 99) %>%
  mutate(main_mode = paste0("M",main_mode))

# Apply weighting methodology and include mode
weighted_trip_rates <- processed_data %>%
  filter(trip_purpose %in% c(1:8)) %>%
  mutate(trip_weights = W1 * W5xHh * W2) %>%
  group_by(IndividualID, main_mode, trip_purpose, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, W2) %>%
  summarise(trip_weights = sum(trip_weights),
            weekly_trips = n())%>%
  ungroup() %>%
  mutate(trip_rate = trip_weights/W2,
         trip_purpose = as.integer(trip_purpose)) %>%
  select(-trip_weights, -W2)

processed_trips <- weighted_trip_rates %>%
  complete(nesting(IndividualID, main_mode, SurveyYear, age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type),
           trip_purpose = 1:8,
           fill = list(weekly_trips = 0, trip_rate = 0))

processed_trips %>%
  group_by(trip_purpose, main_mode) %>%
  summarise(trip_rate = mean(trip_rate)) %>%
  reshape2::dcast(trip_purpose ~ main_mode, value.var = "trip_rate") %>%
  as_tibble()



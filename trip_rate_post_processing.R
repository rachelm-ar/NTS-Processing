require(tidyverse)

# Read in trip rates from tfn segments
ntem_trip_rates <- read_csv('Y:/NTS/TfN_Trip_Rates/tfn_trip_rates.csv')

# Rename and filter
ntem_trip_rates <- ntem_trip_rates %>% 
  rename(area_type = tfn_area_type, trip_rate = tfn_predictions) %>%
  filter(ns_sec != -9) %>%
  mutate(soc_cat = case_when(
    soc_cat == -9 ~ 0,
    soc_cat == 1 ~ 1,
    soc_cat == 2 ~ 2,
    soc_cat == 3 ~ 3
  ))

# Recode Childrens
children_sorted <- ntem_trip_rates %>%
  filter(traveller_type %in% c(1,2,3,4,5,6,7,8)) %>%
  mutate(soc_cat = 0) %>%
  group_by(purpose, traveller_type, ns_sec, area_type) %>%
  summarise(trip_rate = mean(trip_rate)) %>%
  ungroup() %>%
  mutate(soc_cat = 0) %>% 
  select(purpose, traveller_type, soc_cat, ns_sec, area_type, trip_rate) %>%
  arrange(purpose, traveller_type, soc_cat, area_type, ns_sec)

# Remove children from trip rates
new_trip_rates <- ntem_trip_rates %>% filter(!traveller_type %in% c(1,2,3,4,5,6,7,8))

# Add children back with updated recoding
trip_rates_out <- bind_rows(children_sorted, new_trip_rates)

trip_rates_out %>% write_csv('Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates.csv')

# OLD METHOD --------------------------------------------------------------

#ns_sec_ph = tibble(ns_sec = 1:5) %>%
#  mutate(ph = 1)
#
#children_tr <- children_tr %>%
#  left_join(ns_sec_ph) %>%
#  mutate(soc_cat = NA) %>%
#  select(-ph)
#
#trip_rates <- read_csv('Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates.csv')
#
#soc_cat <- trip_rates %>%
#  select(soc_cat) %>%
#  distinct()
#
#ns_sec <- trip_rates %>%
#  select(ns_sec) %>%
#  distinct()
#
#trip_rates_format <- trip_rates %>%
#  filter(soc_cat != -8) %>%
#  filter(ns_sec != -9) %>%
#  filter(purpose != 9)
#
#soc_cat <- trip_rates_format %>%
#  select(soc_cat) %>%
#  distinct()
#
#ns_sec <- trip_rates_format %>%
#  select(ns_sec) %>%
#  distinct()
#
#trip_rates_format <- trip_rates_format %>%
#  mutate(soc_cat = ifelse(soc_cat == -9, NA, soc_cat))
#
#trip_rates_out <- trip_rates_format %>%
#  bind_rows(children_tr) %>%
#  arrange(purpose, traveller_type)
#
#trip_rates_out %>% write_csv('Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates.csv')#
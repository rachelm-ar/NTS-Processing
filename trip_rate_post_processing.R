require(tidyverse)

# Get children's trip rates from NTEM trip rates
ntem_trip_rates <- read_csv('Y:/NTS/TfN_Trip_Rates/hb_TfN_trip_rates.csv')

children_tr <- ntem_trip_rates %>%
  filter(traveller_type %in% c(1, 2, 3, 4, 5, 6, 7, 8)) %>%
  rename(tfn_area_type = area_type) %>%
  mutate(ph = 1)

ns_sec_ph = tibble(ns_sec = 1:5) %>%
  mutate(ph = 1)

children_tr <- children_tr %>%
  left_join(ns_sec_ph) %>%
  mutate(soc_cat = NA) %>%
  select(-ph)

trip_rates <- read_csv('Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates.csv')

soc_cat <- trip_rates %>%
  select(soc_cat) %>%
  distinct()

ns_sec <- trip_rates %>%
  select(ns_sec) %>%
  distinct()

trip_rates_format <- trip_rates %>%
  filter(soc_cat != -8) %>%
  filter(ns_sec != -9) %>%
  filter(purpose != 9)

soc_cat <- trip_rates_format %>%
  select(soc_cat) %>%
  distinct()

ns_sec <- trip_rates_format %>%
  select(ns_sec) %>%
  distinct()

trip_rates_format <- trip_rates_format %>%
  mutate(soc_cat = ifelse(soc_cat == -9, NA, soc_cat))

trip_rates_out <- trip_rates_format %>%
  bind_rows(children_tr) %>%
  arrange(purpose, traveller_type)

trip_rates_out %>% write_csv('Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates.csv')
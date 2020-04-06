require(tidyverse)

trip_rates <- read_csv('Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates.csv')

soc_cat <- trip_rates %>%
  select(soc_cat) %>%
  distinct()

ns_sec <- trip_rates %>%
  select(ns_sec) %>%
  distinct()

trip_rates_format <- trip_rates %>%
  filter(soc_cat != -9) %>%
  filter(soc_cat != -8) %>%
  filter(ns_sec != -9)

soc_cat <- trip_rates_format %>%
  select(soc_cat) %>%
  distinct()

ns_sec <- trip_rates_format %>%
  select(ns_sec) %>%
  distinct()

trip_rates_format %>% write_csv('Y:/NorMITs Synthesiser/import/tfn_segment_production_params/hb_trip_rates_format.csv')

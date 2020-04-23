library(tidyverse)

# Read in nhb weekly trip rates
nhb_trip_rates <- read_csv("C:/Users/Pluto/Documents/nhb_trip_rates.csv")

nhb_trip_rates <- nhb_trip_rates %>% filter(soc_cat != -8, main_mode != 99)

## Add in missing NTEM classifications - Mode = 4 and TP = 11 --------
#
# First we need to split main_mode 3 into both 3 and 4
main_mode4 <- nhb_trip_rates %>% 
  filter(main_mode == 3) %>%
  mutate(main_mode = 4)

## Add this to original df
#nhb_trip_rates <- nhb_trip_rates %>% bind_rows(main_mode4)
#
## Now we need to split purpose 11 into both 11 and 12
#purp11 <- nhb_trip_rates %>%
#  filter(trip_purpose == 12) %>%
#  mutate(trip_purpose = 11)
#
## Add this to original df
#nhb_trip_rates <- bind_rows(nhb_trip_rates, purp11)

# Purpose 12 SOC and NS SEC for all others --------------------------------
purp12 <- nhb_trip_rates %>%
  filter(trip_purpose == 12) %>%
  mutate(soc_cat = case_when(
    soc_cat == -9 ~ 0,
    TRUE ~ as.double(soc_cat)
  )) %>%
  group_by(trip_purpose, nhb_purpose_hb_leg, main_mode, soc_cat) %>%
  summarise(trip_rate = mean(trip_weights))

# Purpose for rest
purprest <- nhb_trip_rates %>%
  filter(trip_purpose != 12, ns_sec != -9) %>%
  group_by(trip_purpose, nhb_purpose_hb_leg, main_mode, ns_sec) %>%
  summarise(trip_rate = mean(trip_weights))

purpall <- bind_rows(purprest, purp12)

mode <- tibble(mode = 1:6, ph = 1)

purpall <- purpall %>% 
  mutate(ph = 1) %>% 
  left_join(mode) %>%
  ungroup()

purpall <- purpall %>% 
  select(trip_purpose, nhb_purpose_hb_leg, main_mode, soc_cat, ns_sec, mode, trip_rate) %>%
  rename(nhb_purpose = trip_purpose,
         purpose = nhb_purpose_hb_leg,
         nhb_mode = main_mode,
         nhb_trip_rate = trip_rate) %>%
  arrange(nhb_purpose, purpose, nhb_mode, soc_cat, ns_sec, mode)

write_csv(purpall, "Y:/NTS/nhb_factor_tr.csv")

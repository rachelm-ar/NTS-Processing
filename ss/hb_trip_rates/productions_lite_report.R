calculate_productions <- function(df, tps, mds){
  
  df %>% 
    transmute(!!paste(tps, mds, sep = "_") := !!sym(tps) * !!sym(mds) * people * trip_rate / 5)
  
}

report_productions <- function(){
  
  select <- dplyr::select

  # Read in trip rates, land use, time splits, mode splits and NTEM  --------
  trip_rates_out <- read_csv("C:/Users/Pluto/Documents/NTS_C/outputs/hb/hb_trip_rates/hb_trip_rates_ntem.csv")
  tp_split <- read_csv('C:/Users/Pluto/Documents/Trip_rate_testing/hb_time_split.csv')
  mode_split <- read_csv('C:/Users/Pluto/Documents/Trip_rate_testing/hb_mode_split.csv')
  ntem_control <- read_csv('C:/Users/Pluto/Documents/Trip_rate_testing/ntem_control.csv')
  
  tpm_split <- read_csv("C:/Users/Pluto/Documents/NTS_C/outputs/hb/hb_mode_time_split_v2.0_ntem.csv")
  
  land_use <- read_csv("C:/Users/Pluto/Documents/Trip_rate_testing/land_use_output_msoa.csv")
  land_use <- select(land_use, area_type, traveller_type, ns, soc, people)
  
  tt_lu <- read_csv("C:/Users/Pluto/Documents/NTS_C/lookups/tfn_traveller_type.csv")
  tt_lu <- tt_lu %>%
    rename(ns = ns_sec,
           soc = soc_cat)
  
  # Land use pre processing to adjust new SOC classifications 
  land_use <- land_use %>%
    mutate(soc = ifelse(traveller_type %in% c(1:8, 25:48, 65:88), 2, soc)) %>%
    rename(ntem_traveller_type = traveller_type) %>%
    left_join(tt_lu) %>%
    select(tfn_traveller_type, ntem_traveller_type, area_type, hh_cars, people)
  
  ### NTEM
  
  land_use_ntem <- land_use %>%
    ungroup() %>%
    group_by(ntem_traveller_type, area_type, hh_cars) %>%
    summarise(people = sum(people))
  
  p_join <- tibble(p = rep(1, 8),
                   p2 = seq(1,8))
  
  land_use2 <- land_use_ntem %>%
    select(ntem_traveller_type, area_type, hh_cars, people) %>%
    group_by(ntem_traveller_type, area_type, hh_cars) %>%
    summarise(people = sum(people)) %>%
    ungroup() %>%
    mutate(p = 1) %>%
    left_join(p_join) %>%
    select(-p) %>%
    rename(p = p2,
           cars = hh_cars)
  
  tt_lu <- tt_lu %>%
    select(ntem_traveller_type, hh_cars) %>%
    rename(ntem_tt = ntem_traveller_type,
           cars = hh_cars)
  
  tt_lu <- tt_lu %>%
    distinct()
  
  trip_rates <- trip_rates_out %>%
    left_join(tt_lu)
  
  productions_all <- land_use2 %>%
    left_join(trip_rates, by = c('ntem_traveller_type' = 'ntem_tt',
                                 'p',
                                 'area_type' = 'ntem_at',
                                 'cars')) %>%
    mutate(trips = people * trip_rates) %>%
    left_join(tpm_split, by = c('p',
                                'area_type' = 'tfn_at',
                                'ntem_traveller_type' = 'ntem_tt')) %>%
    select(p, trips, tp1_m1:tp4_m6) %>%
    mutate(across(tp1_m1:tp4_m6, ~ .x * trips/5)) %>%
    mutate(m1 = tp1_m1 + tp2_m1 + tp3_m1 + tp4_m1,
           m2 = tp1_m2 + tp2_m2 + tp3_m2 + tp4_m2,
           m3 = tp1_m3 + tp2_m3 + tp3_m3 + tp4_m3,
           m5 = tp1_m5 + tp2_m5 + tp3_m5 + tp4_m5,
           m6 = tp1_m6 + tp2_m6 + tp3_m6 + tp4_m6) %>% 
    select(p, m1:m6) %>% 
    group_by(p) %>% 
    summarise_at(vars(m1:m6), list(~ sum(., na.rm = TRUE))) %>% 
    ungroup() %>% 
    mutate(all = m1 + m2 + m3 + m5 + m6,
           p = as.character(p))
  
  report_output <- productions_all %>%
    summarise_at(vars(m1:all), sum) %>% 
    mutate(p = "all") %>% 
    bind_rows(productions_all) %>% 
    arrange(p) %>% 
    select(p, everything())
  
  report_output
  
  ###
  
  # Filler df
  p_join <- tibble(p = rep(1, 8),
                   p2 = seq(1,8))
  
  land_use2 <- land_use %>%
    select(tfn_traveller_type, ntem_traveller_type, area_type, hh_cars, people) %>%
    group_by(tfn_traveller_type, ntem_traveller_type, area_type, hh_cars) %>%
    summarise(people = sum(people)) %>%
    ungroup() %>%
    mutate(p = 1) %>%
    left_join(p_join) %>%
    select(-p) %>%
    rename(p = p2,
           cars = hh_cars)
  
  tt_lu <- tt_lu %>%
    select(tfn_traveller_type, hh_cars) %>%
    rename(tfn_tt = tfn_traveller_type,
           cars = hh_cars)
  
  trip_rates <- trip_rates_out %>%
    rename(tfn_tt = traveller_type,
           tfn_at = area_type) %>%
    left_join(tt_lu)

  productions_all <- land_use2 %>%
    left_join(trip_rates, by = c('tfn_traveller_type' = 'tfn_tt',
                                 'p',
                                 'area_type' = 'tfn_at',
                                 'cars')) %>%
    mutate(trips = people * trip_rate) %>%
    left_join(tpm_split, by = c('p',
                                'area_type' = 'tfn_at',
                                'ntem_traveller_type' = 'ntem_tt')) %>%
    select(p, trips, tp1_m1:tp4_m6) %>%
    mutate(across(tp1_m1:tp4_m6, ~ .x * trips/5)) %>%
    mutate(m1 = tp1_m1 + tp2_m1 + tp3_m1 + tp4_m1,
           m2 = tp1_m2 + tp2_m2 + tp3_m2 + tp4_m2,
           m3 = tp1_m3 + tp2_m3 + tp3_m3 + tp4_m3,
           m5 = tp1_m5 + tp2_m5 + tp3_m5 + tp4_m5,
           m6 = tp1_m6 + tp2_m6 + tp3_m6 + tp4_m6) %>% 
    select(p, m1:m6) %>% 
    group_by(p) %>% 
    summarise_at(vars(m1:m6), list(~ sum(., na.rm = TRUE))) %>% 
    ungroup() %>% 
    mutate(all = m1 + m2 + m3 + m5 + m6,
           p = as.character(p))
  
  report_output <- productions_all %>%
    summarise_at(vars(m1:all), sum) %>% 
    mutate(p = "all") %>% 
    bind_rows(productions_all) %>% 
    arrange(p) %>% 
    select(p, everything())
  
  report_output
  
  #productions <- land_use2 %>%
  #  left_join(trip_rates, by = c('tfn_traveller_type' = 'tfn_tt',
  #                               'p',
  #                               'area_type' = 'tfn_at',
  #                               'cars')) %>%
  #  left_join(tp_split, by = c("area_type",
  #                             'ntem_traveller_type' = 'traveller_type',
  #                             "p")) %>%
  #  mutate(ca = ifelse(cars == "0", 1, 2)) %>% 
  #  left_join(mode_split, by = c("area_type",
  #                               "ca",
  #                               "p")) %>%
  #  select(p, msoa_zone_id, people, trip_rate, tp1:tp4, m1:m6)
  
  #tps <- c('tp1', 'tp2', 'tp3', 'tp4')
  #mds <- c('m1', 'm2', 'm3', 'm5', 'm6')
  #cross_vars <- crossing(tps, mds)
  #
  #productions_all <- productions %>% 
  #  bind_cols(map2_dfc(cross_vars$tps, cross_vars$mds, calculate_productions, df = productions)) %>% 
  #  mutate(m1 = tp1_m1 + tp2_m1 + tp3_m1 + tp4_m1,
  #         m2 = tp1_m2 + tp2_m2 + tp3_m2 + tp4_m2,
  #         m3 = tp1_m3 + tp2_m3 + tp3_m3 + tp4_m3,
  #         m5 = tp1_m5 + tp2_m5 + tp3_m5 + tp4_m5,
  #         m6 = tp1_m6 + tp2_m6 + tp3_m6 + tp4_m6) %>% 
  #  select(p, msoa_zone_id, m1:m6) %>% 
  #  group_by(p, msoa_zone_id) %>% 
  #  summarise_at(vars(m1:m6), list(~ sum(., na.rm = TRUE))) %>% 
  #  ungroup() %>% 
  #  mutate(all = m1 + m2 + m3 + m5 + m6,
  #         p = as.character(p))

  report_output <- productions_all %>%
    summarise_at(vars(m1:all), sum) %>% 
    mutate(p = "all") %>% 
    bind_rows(productions_all) %>% 
    arrange(p) %>% 
    select(p, everything())
  
  write_csv(report_output, "C:/Users/Pluto/Documents/NTS_C/outputs/hb/hb_trip_rates/testing/tfn_production_topline_v1.9.csv")
  
  # Join and calculate productions ------------------------------------------
  
  productions <- land_use %>% 
    left_join(trip_rates, by = c("traveller_type",
                                 "p",
                                 "soc",
                                 "ns",
                                 "area_type",
                                 "cars")) %>% 
    left_join(tp_split, by = c("area_type",
                               "traveller_type",
                               "p")) %>% 
    mutate(ca = ifelse(cars == "0", 1, 2)) %>% 
    left_join(mode_split, by = c("area_type",
                                 "ca",
                                 "p")) %>% 
    select(p, people, trip_rate, tp1:tp4, m1:m6)
  
  # Create a tibble to loop over combinations of tp and m
  tps <- c('tp1', 'tp2', 'tp3', 'tp4')
  mds <- c('m1', 'm2', 'm3', 'm5', 'm6')
  cross_vars <- crossing(tps, mds)
  
  # Iterates over combinations of tp and m to multiply
  productions_all <- productions %>% 
    bind_cols(map2_dfc(cross_vars$tps, cross_vars$mds, calculate_productions, df = productions)) %>% 
    mutate(m1 = tp1_m1 + tp2_m1 + tp3_m1 + tp4_m1 + tp5_m1 + tp6_m1,
           m2 = tp1_m2 + tp2_m2 + tp3_m2 + tp4_m2 + tp5_m2 + tp6_m2,
           m3 = tp1_m3 + tp2_m3 + tp3_m3 + tp4_m3 + tp5_m3 + tp6_m3,
           m5 = tp1_m5 + tp2_m5 + tp3_m5 + tp4_m5 + tp5_m5 + tp6_m5,
           m6 = tp1_m6 + tp2_m6 + tp3_m6 + tp4_m6 + tp5_m6 + tp6_m6) %>% 
    select(p, m1:m6) %>% 
    group_by(p) %>% 
    summarise_at(vars(m1:m6), list(~ sum(., na.rm = TRUE))) %>% 
    ungroup() %>% 
    mutate(all = m1 + m2 + m3 + m5 + m6,
           p = as.character(p))
  
  # Post processing and write -----------------------------------------------
  report_output <- productions_all %>%
    summarise_at(vars(m1:all), sum) %>% 
    mutate(p = "all") %>% 
    bind_rows(productions_all) %>% 
    arrange(p) %>% 
    select(p, everything())
  
  write_csv(report_output, "C:/Users/Pluto/Documents/NTS_C/outputs/hb/hb_trip_rates/testing/tfn_production_topline_v2.1.csv")
  
}


# NTS Comparison

report_productions <- function(){
  
  select <- dplyr::select
  
  # Read in trip rates, land use, time splits, mode splits and NTEM  --------
  trip_rates_out <- read_csv("C:/Users/Pluto/Documents/Trip_rate_testing/hb_trip_rates_ntsv2.csv")
  tp_split <- read_csv('C:/Users/Pluto/Documents/Trip_rate_testing/hb_time_split.csv')
  mode_split <- read_csv('C:/Users/Pluto/Documents/Trip_rate_testing/hb_mode_split.csv')
  ntem_control <- read_csv('C:/Users/Pluto/Documents/Trip_rate_testing/ntem_control.csv')
  
  land_use <- data.table::fread('C:/Users/Pluto/Documents/Trip_rate_testing/land_use_output_msoa.csv',
                                select = c('msoa_zone_id',
                                           'area_type',
                                           'cars',
                                           'traveller_type',
                                           'people'))
  land_use <- as_tibble(land_use)
  
  # Pre process land use and trip rates preparing for merge -----------------
  p_join <- tibble(p = rep(1, 8),
                   p2 = seq(1,8))
  
  land_use <- land_use %>% 
    select(msoa_zone_id, area_type, cars, traveller_type, people) %>% 
    group_by(msoa_zone_id, area_type, cars, traveller_type) %>% 
    summarise(people = sum(people)) %>% 
    ungroup() %>% 
    mutate(p = 1) %>% 
    left_join(p_join, by = "p") %>% 
    select(-p) %>%
    rename(p = p2)
  
  tt_lu <- read_csv("Y:/NTS/lookups/traveller_type---age_work_status--gender--hh_adults--cars.csv")
  
  trip_rates <- trip_rates_out %>% 
    left_join(tt_lu, by = c('traveller_type' = 'tt')) %>% 
    select(-c(age_work_status, gender, hh_adults)) %>%
    mutate(cars = case_when(
      cars == 1 ~ "0",
      cars == 2 ~ "1",
      cars == 3 ~ "1+",
      cars == 4 ~ "2+"
    ))
  
  # Join and calculate productions ------------------------------------------
  productions <- land_use %>% 
    left_join(trip_rates, by = c("traveller_type",
                                 "p",
                                 "area_type",
                                 "cars")) %>% 
    left_join(tp_split, by = c("area_type",
                               "traveller_type",
                               "p")) %>% 
    mutate(ca = ifelse(cars == "0", 1, 2)) %>% 
    left_join(mode_split, by = c("area_type",
                                 "ca",
                                 "p")) %>% 
    select(p, people, trip_rate, tp1:tp4, m1:m6)
  
  # Create a tibble to loop over combinations of tp and m
  tps <- c('tp1', 'tp2', 'tp3', 'tp4')
  mds <- c('m1', 'm2', 'm3', 'm5', 'm6')
  cross_vars <- crossing(tps, mds)
  
  # Iterates over combinations of tp and m to multiply
  productions_all <- productions %>% 
    bind_cols(map2_dfc(cross_vars$tps, cross_vars$mds, calculate_productions, df = productions)) %>% 
    mutate(m1 = tp1_m1 + tp2_m1 + tp3_m1 + tp4_m1,
           m2 = tp1_m2 + tp2_m2 + tp3_m2 + tp4_m2,
           m3 = tp1_m3 + tp2_m3 + tp3_m3 + tp4_m3,
           m5 = tp1_m5 + tp2_m5 + tp3_m5 + tp4_m5,
           m6 = tp1_m6 + tp2_m6 + tp3_m6 + tp4_m6) %>% 
    select(p, m1:m6) %>% 
    group_by(p) %>% 
    summarise_at(vars(m1:m6), list(~ sum(., na.rm = TRUE))) %>% 
    ungroup() %>% 
    mutate(all = m1 + m2 + m3 + m5 + m6,
           p = as.character(p))
  
  # Post processing and write -----------------------------------------------
  report_output <- productions_all %>%
    summarise_at(vars(m1:all), sum) %>% 
    mutate(p = "all") %>% 
    bind_rows(productions_all) %>% 
    arrange(p) %>% 
    select(p, everything())
  
  write_csv(report_output, "C:/Users/Pluto/Documents/Trip_rate_testing/tfn_production_topline_vnts2.csv")
  
}

land_use <- land_use %>%
  mutate(soc = ifelse(traveller_type %in% c(1:8, 25:48, 65:88), 4, soc)) %>%
  rename(ntem_traveller_type = traveller_type) %>%
  left_join(tt_lu) %>%
  select(msoa_zone_id, area_type, tfn_traveller_type, ns, soc, people, hh_cars) %>%
  rename(traveller_type = tfn_traveller_type,
         cars = hh_cars) 

# Pre process land use and trip rates preparing for merge -----------------
p_join <- tibble(p = rep(1, 8),
                 p2 = seq(1,8))

land_use <- land_use %>% 
  select(msoa_zone_id, area_type, cars, traveller_type, soc, ns, people) %>% 
  group_by(msoa_zone_id, area_type, cars, traveller_type, soc, ns) %>% 
  summarise(people = sum(people)) %>% 
  ungroup() %>% 
  mutate(p = 1) %>% 
  left_join(p_join, by = "p") %>% 
  select(-p) %>%
  rename(p = p2)

tt_lu <- read_csv("C:/Users/Pluto/Documents/NTS_C/lookups/tfn_traveller_type.csv")

tt_lu <- tt_lu %>% 
  select(tfn_traveller_type, hh_cars) %>%
  rename(traveller_type = tfn_traveller_type,
         cars = hh_cars)

trip_rates <- trip_rates_out %>% 
  left_join(tt_lu)

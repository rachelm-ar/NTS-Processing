calculate_productions <- function(df, tps, mds){
  
  df %>% 
    transmute(!!paste(tps, mds, sep = "_") := !!sym(tps) * !!sym(mds) * people * trip_rate / 5)
  
}

report_productions <- function(){
  

  # Read in trip rates, land use, time splits, mode splits and NTEM  --------
  trip_rates_out <- read_csv("C:/Users/Pluto/Documents/hb_trip_rates_v2.5.csv")
  tp_split <- read_csv('C:/Users/Pluto/Documents/Trip_rate_testing/hb_time_split.csv')
  mode_split <- read_csv('C:/Users/Pluto/Documents/Trip_rate_testing/hb_mode_split.csv')
  ntem_control <- read_csv('C:/Users/Pluto/Documents/Trip_rate_testing/ntem_control.csv')
  
  land_use <- read_csv('Y:/NorMITs Land Use/iter3/land_use_output_msoa.csv',
                       col_types = cols_only('msoa_zone_id' = col_character(),
                                             'area_type' = col_double(),
                                             'cars' = col_character(),
                                             'traveller_type' = col_double(),
                                             'soc' = col_character(),
                                             'ns' = col_character(),
                                             'people' = col_double()))
  
  # Pre process land use and trip rates preparing for merge -----------------
  p_join <- tibble(p = rep(1, 8),
                   p2 = seq(1,8))
  
  land_use <- land_use %>% 
    mutate(ca = ifelse(cars == "0", 1, 2)) %>% 
    select(msoa_zone_id, area_type, ca, traveller_type, soc, ns, people) %>% 
    group_by(msoa_zone_id, area_type, ca, traveller_type, soc, ns) %>% 
    summarise(people = sum(people)) %>% 
    ungroup() %>% 
    mutate(p = 1) %>% 
    left_join(p_join, by = "p") %>% 
    select(-p) %>%
    rename(p = p2) %>%
    mutate(soc = ifelse(p %in% 1:2, as.character(as.integer(soc)), "none"),
           ns = ifelse(p %in% 1:2, "none", as.character(as.integer(ns))))
  
  trip_rates <- trip_rates_out %>% 
    mutate(soc = ifelse(p %in% 1:2, as.character(soc), "none"),
           soc = ifelse(p %in% 1:2 & soc == "99", "0", as.character(soc)))
  

  # Join and calculate productions ------------------------------------------
  productions <- land_use %>% 
    left_join(trip_rates, by = c("traveller_type",
                                 "p",
                                 "soc",
                                 "ns",
                                 "area_type")) %>% 
    left_join(tp_split, by = c("area_type",
                               "traveller_type",
                               "p")) %>% 
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
  
  write_csv(report_output, "C:/Users/Pluto/Documents/Trip_rate_testing/tfn_production_topline.csv")
  
}












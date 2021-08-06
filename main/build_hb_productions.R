# Add titles to plots!
switch_mts_names <- function(mts){
  
  mts_names <- colnames(mts)[4:length(colnames(mts))]
  mts_names <- str_split(mts_names, "_") 
  mts_names <- lapply(mts_names, sort)
  mts_names <- sapply(mts_names, paste, collapse = "_")
  mts_names <- unlist(mts_names)
  mts_names <- c("p", "area_type", "ntem_tt", mts_names)
  
  colnames(mts) <- mts_names
  
  mts
  
}

calculate_productions <- function(tr, lu, mts){
  
  if(all(!colnames(tr) %in% "tfn_tt")){
    
    lu <- lu %>% 
      group_by(p, msoa, area_type, ntem_tt) %>% 
      summarise(people = sum(people)) %>% 
      ungroup()
    
  }
  
  tr %>% 
    left_join(lu) %>% 
    left_join(mts) %>% 
    mutate(people = ifelse(is.na(people), 0, people)) %>% 
    mutate(across(m1_tp1:m6_tp6, ~ .x * trip_rates * people)) %>% 
    group_by(p, msoa, area_type) %>%
    summarise(across(m1_tp1:m6_tp6, ~ sum(.))) %>% 
    ungroup() %>% 
    pivot_longer(m1_tp1:m6_tp6, names_to =  "m_tp", values_to = "trips") %>% 
    separate(m_tp, c("m", "tp"), sep = "_")
  
}

r1_aggregation <- function(df, type){
  
  df %>%
    group_by(p, msoa, area_type) %>%
    summarise(!!type := sum(trips)) %>% 
    ungroup()
  
}

r3_aggregation <- function(df){
  
  df <- df %>% 
    filter(tp %in% c("AM", "IP", "PM", "OP")) %>% 
    group_by(p, m) %>%
    summarise(trips = sum(trips)/5) %>% 
    ungroup() %>% 
    pivot_wider(names_from = "m", values_from = "trips") %>% 
    mutate(all = Walk + Cycle + `Car/Van` + Bus + Rail,
           p = as.character(p))
  
  df %>%
    summarise_at(vars(Walk:all), sum) %>% 
    mutate(p = "all") %>% 
    bind_rows(df, .) %>% 
    select(p, everything())
  
}

r3_diff <- function(r3, tempro = cte_control){
  
  r3 %>%
    pivot_longer(Walk:all, names_to = "m", values_to = "trips") %>% 
    left_join(tempro) %>% 
    mutate(diff = (trips-tempro_trips)/tempro_trips * 100) %>% 
    pivot_wider(c(-trips, -tempro_trips), names_from = "m", values_from = "diff")
  
}

build_hb_productions <- function(user, drive, landuse_preprocess = FALSE){
  
  library_list <- c("dplyr",
                    "stringr",
                    "readr",
                    "tidyr",
                    "purrr",
                    "ggplot2",
                    "ggpmisc",
                    "Cairo",
                    "ggthemes",
                    "openxlsx")
  
  library_c(library_list)
  
  # NTS directory
  y_dir <- "Y:/NTS/"
  c_dir <- str_c("C:/Users/", user, "/Documents/NTS_C/")
  nts_dir <- ifelse(drive == "Y", y_dir, c_dir)
  hb_dir <- str_c(nts_dir, "outputs/hb/")
  
  # Land Use Directory
  landuse_dir <- str_c(nts_dir, "import/land_use/land_use_output_msoa.csv")
  
  # HB Trip Rates
  tfn_tr_dir <- str_c(hb_dir, "hb_trip_rates/hb_trip_rates_tfn_v2.0.csv")
  ntem_tr_dir <- str_c(hb_dir, "hb_trip_rates/hb_trip_rates_ntem.csv")
  cte_tr_dir <- str_c(nts_dir, "import/ctripend/TR_NTEM_TrendSnr1_Year2016_NTEMT1.csv")
  
  # Mode time splits
  tfn_mts_dir <- str_c(hb_dir, "hb_mode_time_split/hb_mts_tfn_wide.csv")
  ntem_mts_dir <- str_c(hb_dir, "hb_mode_time_split/hb_mts_ntem_wide.csv")
  cte_mts_dir <- str_c(nts_dir, "import/ctripend/IRHOmdshr/IRHOmdhsr_FINAL.csv")

  # Traveller Type lookup
  tt_lookup_dir <- str_c(nts_dir, "lookups/tfn_traveller_type.csv")
  
  # TEMPRO Productions
  tempro_productions_dir <- str_c(nts_dir, "import/ctripend/tempro_productions.csv")
  
  # Export Directory
  r1_dir <- str_c(hb_dir, "Productions reporting/P - MSOA/Purpose/")
  r2_dir <- str_c(hb_dir, "Productions reporting/P - MSOA/Mode Time Split/")
  r3_dir <- str_c(hb_dir, "Productions reporting/Purpose Mode Report/")
  
  sapply(c(r1_dir, r2_dir, r3_dir), dir.create, showWarnings = FALSE)
  
  # Read
  landuse <- read_csv(landuse_dir)
  
  tfn_tr <- read_csv(tfn_tr_dir)
  ntem_tr <- read_csv(ntem_tr_dir)
  cte_tr <- read_csv(cte_tr_dir)
  
  tfn_mts <- read_csv(tfn_mts_dir)
  ntem_mts <- read_csv(ntem_mts_dir)
  cte_mts <- read_csv(cte_mts_dir)
  
  tt_lookup <- read_csv(tt_lookup_dir)
  
  tempro_productions <- read_csv(tempro_productions_dir)
  
# Pre-Processing ----------------------------------------------------------

  ### Land use
  if(landuse_preprocess){
    
    # Aggregate by segmentation
    landuse <- landuse %>% 
      group_by(msoa_zone_id, area_type, traveller_type, soc_cat, ns_sec) %>% 
      summarise(people = sum(people)) %>% 
      ungroup() %>% 
      rename(msoa = msoa_zone_id,
             soc = soc_cat,
             ns = ns_sec,
             ntem_tt = traveller_type) 
    
    # Change SOC for non working
    landuse <- mutate(landuse, soc = ifelse(soc == 0, 2, soc))
    
    # Add in traveller type
    landuse <- landuse %>%
      left_join(tt_lookup) %>% 
      select(msoa, area_type, tfn_tt, ntem_tt, people)
    
    write_csv(landuse, landuse_dir)
    
  }
  
  ### Trip Rates
  
  # TfN
  tfn_tr <- rename(tfn_tr, area_type = tfn_at)
  
  # NTEM
  ntem_tr <- rename(ntem_tr, area_type = ntem_at)
  
  # CTripEnd
  cte_tr <- cte_tr %>%
    rename(p = h,
           ntem_tt = s,
           area_type = r,
           trip_rates = TripRates)
  
  ### Mode Time Splits
  
  # TfN
  tfn_mts <- switch_mts_names(tfn_mts)
  
  # NTEM
  ntem_mts <- switch_mts_names(ntem_mts)
  
  # CTripEnd
  cte_mts <- cte_mts %>% 
    rename(p = h,
           ntem_tt = s,
           area_type = r)
  
  colnames(cte_mts) <- str_replace(colnames(cte_mts), "d", "_tp")
  
  ### Land Use
  landuse <- landuse %>%
    mutate(p = 1) %>% 
    complete(nesting(msoa, area_type, ntem_tt, tfn_tt),
             p = 1:8) %>%
    fill(people)
  
# Calculate Productions ---------------------------------------------------
  
  tfn <- calculate_productions(tfn_tr, landuse, tfn_mts)
  ntem <- calculate_productions(ntem_tr, landuse, ntem_mts)
  cte <- calculate_productions(cte_tr, landuse, cte_mts)

  # Reformat vars and convert to factors
  tfn <- tfn %>% 
    factor_p() %>% 
    factor_m("tfn") %>% 
    factor_tp()
  
  ntem <- ntem %>% 
    factor_p() %>% 
    factor_m("ntem") %>% 
    factor_tp()
  
  cte <- cte %>% 
    factor_p() %>% 
    factor_m("cte") %>% 
    factor_tp()
  
# Report 1 - Comparsion of Productions Plots ------------------------------
  
  r1_agg <- map2(list(tfn, ntem, cte), list("tfn", "ntem", "cte"), r1_aggregation)
  r1_agg <- reduce(r1_agg, left_join)
  
  ### By P, MSOA
  
  # TfN vs CTE
  r1_cte_tfn <- plot_comparison(r1_agg, "cte", "tfn")
  r1_cte_tfn_p <- plot_purpose_comparison(r1_cte_tfn, "p")
  
  r1_cte_ntem <- plot_comparison(r1_agg, "cte", "ntem")
  r1_cte_ntem_p <- plot_purpose_comparison(r1_cte_ntem, "p")
  
  r1_ntem_tfn <- plot_comparison(r1_agg, "ntem", "tfn")
  r1_ntem_tfn_p <- plot_purpose_comparison(r1_ntem_tfn, "p")
  
  save_plot(str_c(r1_dir, "cte_vs_tfn.png"), r1_cte_tfn, 4 , 4)
  save_plot(str_c(r1_dir, "cte_vs_tfn_purposes.png"), r1_cte_tfn_p, 6 , 12)

  save_plot(str_c(r1_dir, "cte_vs_ntem.png"), r1_cte_ntem, 4 , 4)
  save_plot(str_c(r1_dir, "cte_vs_ntem_purposes.png"), r1_cte_ntem_p, 6 , 12)
  
  save_plot(str_c(r1_dir, "ntem_vs_tfn.png"), r1_ntem_tfn, 4 , 4)
  save_plot(str_c(r1_dir, "ntem_vs_tfn_purposes.png"), r1_ntem_tfn_p, 6 , 12)
  
  ### By P, MSOA, AT
  
  r1_cte_tfn_at <- plot_comparison_at(r1_cte_tfn)
  r1_cte_tfn_p_at <- plot_purpose_comparison(r1_cte_tfn_at, "p")
  
  r1_cte_ntem_at <- plot_comparison_at(r1_cte_ntem)
  r1_cte_ntem_p_at <- plot_purpose_comparison(r1_cte_ntem_at, "p")
  
  r1_ntem_tfn_at <- plot_comparison_at(r1_ntem_tfn)
  r1_ntem_tfn_p_at <- plot_purpose_comparison(r1_ntem_tfn_at, "p")
  
  save_plot(str_c(r1_dir, "By Area Type/cte_vs_tfn.png"), r1_cte_tfn_at, 4 , 4)
  save_plot(str_c(r1_dir, "By Area Type/cte_vs_tfn_purposes.png"), r1_cte_tfn_p_at, 6 , 12)
  
  save_plot(str_c(r1_dir, "By Area Type/cte_vs_ntem.png"), r1_cte_ntem_at, 4 , 4)
  save_plot(str_c(r1_dir, "By Area Type/cte_vs_ntem_purposes.png"), r1_cte_ntem_p_at, 6 , 12)
  
  save_plot(str_c(r1_dir, "By Area Type/ntem_vs_tfn.png"), r1_ntem_tfn_at, 4 , 4)
  save_plot(str_c(r1_dir, "By Area Type/ntem_vs_tfn_purposes.png"), r1_ntem_tfn_p_at, 6 , 12)
  
# Report 2 - Plots for Mode Time Split ------------------------------------

  r2_tfn <- rename(tfn, tfn = trips)
  r2_ntem <- rename(ntem, ntem = trips)
  r2_cte <- rename(cte, cte = trips)
  
  r2_ntem_cte <- left_join(r2_ntem, r2_cte)
  
  r2_ntem <- r2_ntem %>%
    mutate(m = as.character(m)) %>% 
    mutate(m = ifelse(m %in% c("Car/Van Driver", "Car/Van Passenger"), "Car/Van", m)) %>%
    mutate(m = factor(m, levels = c("Walk", "Cycle", "Car/Van", "Bus", "Rail"))) %>% 
    group_by(p, msoa, area_type, m, tp) %>% 
    summarise(ntem = sum(ntem)) %>% 
    ungroup()
  
  r2_cte <- r2_cte %>%
    mutate(m = as.character(m)) %>% 
    mutate(m = ifelse(m %in% c("Car/Van Driver", "Car/Van Passenger"), "Car/Van", m)) %>%
    mutate(m = factor(m, levels = c("Walk", "Cycle", "Car/Van", "Bus", "Rail"))) %>% 
    group_by(p, msoa, area_type, m, tp) %>% 
    summarise(cte = sum(cte)) %>% 
    ungroup()
  
  r2_tfn_ntem_cte <- r2_tfn %>% 
    left_join(r2_ntem) %>% 
    left_join(r2_cte)
  
  ### By P, MSOA, m, tp
  
  r2_ntem_cte <- plot_comparison(r2_ntem_cte, "cte", "ntem")
  r2_ntem_cte_mts <- plot_purpose_comparison(r2_ntem_cte, "tp", "m", 6)
  
  r2_tfn_cte <- plot_comparison(r2_tfn_ntem_cte, "cte", "tfn")
  r2_tfn_cte_mts <- plot_purpose_comparison(r2_tfn_cte, "tp", "m", 5)
  
  r2_tfn_ntem <- plot_comparison(r2_tfn_ntem_cte, "ntem", "tfn")
  r2_tfn_ntem_mts <- plot_purpose_comparison(r2_tfn_ntem, "tp", "m", 5)

  save_plot(str_c(r2_dir, "cte_ntem.png"), r2_ntem_cte, 4, 4)
  save_plot(str_c(r2_dir, "cte_ntem_mts.png"), r2_ntem_cte_mts, 12, 12)
  
  save_plot(str_c(r2_dir, "cte_tfn.png"), r2_tfn_cte, 4, 4)
  save_plot(str_c(r2_dir, "cte_tfn_mts.png"), r2_tfn_cte_mts, 12, 12)
  
  save_plot(str_c(r2_dir, "ntem_tfn.png"), r2_tfn_ntem, 4, 4)
  save_plot(str_c(r2_dir, "ntem_tfn_mts.png"), r2_tfn_ntem_mts, 12, 12)
  
  ### By P, MSOA, AT, m, tp
  
  r2_ntem_cte_at <- plot_comparison_at(r2_ntem_cte)
  r2_ntem_cte_mts_at <- plot_purpose_comparison(r2_ntem_cte_at, "tp", "m", 6)
  
  r2_tfn_cte_at <- plot_comparison_at(r2_tfn_cte)
  r2_tfn_cte_mts_at <- plot_purpose_comparison(r2_tfn_cte_at, "tp", "m", 5)
  
  r2_tfn_ntem_at <- plot_comparison_at(r2_tfn_ntem)
  r2_tfn_ntem_mts_at <- plot_purpose_comparison(r2_tfn_ntem_at, "tp", "m", 5)
  
  save_plot(str_c(r2_dir, "By Area Type/cte_ntem.png"), r2_ntem_cte_at, 4, 4)
  save_plot(str_c(r2_dir, "By Area Type/cte_ntem_mts.png"), r2_ntem_cte_mts_at, 12, 12)
  
  save_plot(str_c(r2_dir, "By Area Type/cte_tfn.png"), r2_tfn_cte_at, 4, 4)
  save_plot(str_c(r2_dir, "By Area Type/cte_tfn_mts.png"), r2_tfn_cte_mts_at, 12, 12)
  
  save_plot(str_c(r2_dir, "By Area Type/ntem_tfn.png"), r2_tfn_ntem_at, 4, 4)
  save_plot(str_c(r2_dir, "By Area Type/ntem_tfn_mts.png"), r2_tfn_ntem_mts_at, 12, 12)
  
# Report 3 - Comparison of Purpose by Mode --------------------------------

  cte <- cte %>% 
    mutate(m = as.character(m)) %>% 
    mutate(m = ifelse(m %in% c("Car/Van Driver", "Car/Van Passenger"), "Car/Van", m)) %>%
    mutate(m = factor(m, levels = c("Walk", "Cycle", "Car/Van", "Bus", "Rail"))) %>% 
    group_by(p, msoa, area_type, m, tp) %>%
    summarise(trips = sum(trips))
  
  ntem <- ntem %>% 
    mutate(m = as.character(m)) %>% 
    mutate(m = ifelse(m %in% c("Car/Van Driver", "Car/Van Passenger"), "Car/Van", m)) %>%
    mutate(m = factor(m, levels = c("Walk", "Cycle", "Car/Van", "Bus", "Rail"))) %>% 
    group_by(p, msoa, area_type, m, tp) %>%
    summarise(trips = sum(trips))
  
  r3_cte <- r3_aggregation(cte)
  r3_tfn <- r3_aggregation(tfn)
  r3_ntem <- r3_aggregation(ntem)  
  
  # Tempro Long format
  tempro_long <- tempro_productions %>% 
    factor_p() %>%
    mutate(p = as.character(p),
           p = ifelse(is.na(p), "all", p)) %>% 
    rename(Walk = m1,
           Cycle = m2,
           `Car/Van` = m3,
           Bus = m5,
           Rail = m6) %>% 
    pivot_longer(Walk:all, names_to = "m", values_to = "tempro_trips")
    
  # CTE Long Format
  cte_control <- r3_cte %>% 
    pivot_longer(Walk:all, names_to = "m", values_to = "tempro_trips")
  
  r3_cte_diff <- r3_diff(r3_cte, tempro_long)
  r3_tfn_diff <- r3_diff(r3_tfn)
  r3_ntem_diff <- r3_diff(r3_ntem)
  
  # Save individual outputs
  write_csv(r3_cte_diff, str_c(r3_dir, "cte_vs_tempro.csv"))
  write_csv(r3_tfn_diff, str_c(r3_dir, "cte_vs_tfn.csv"))
  write_csv(r3_ntem_diff, str_c(r3_dir, "cte_vs_ntem.csv"))
  
  ### XSLX Output
  
  # Create workbook to add worksheets
  wb <- createWorkbook()
  
  # Add worksheet
  addWorksheet(wb, "Productions")
  
  # Styles
  style <- createStyle(
    textDecoration = "bold", halign = "center", border = c("top", "bottom", "left", "right")
  )
  
  ### TEMPRO Output
  writeData(wb, "Productions", "TEMPRO OUTPUT", startRow = 1, startCol = 5)
  mergeCells(wb, "Productions", rows = 1, cols = 5:11)
  addStyle(wb, "Productions", style, rows = 1, cols = 5:11)
  
  writeData(wb, "Productions", tempro_productions, startRow = 2, startCol = 5)
  
  ### CTE
  
  writeData(wb, "Productions", "CTE OUTPUT", startRow = 13, startCol = 1)
  mergeCells(wb, "Productions", rows = 13, cols = 1:7)
  addStyle(wb, "Productions", style, rows = 13, cols = 1:7)

  writeData(wb, "Productions", "CTE vs TEMPRO", startRow = 13, startCol = 9)
  mergeCells(wb, "Productions", rows = 13, cols = 9:15)
  addStyle(wb, "Productions", style, rows = 13, cols = 9:15)
  
  writeData(wb, "Productions", r3_cte, startRow = 14, startCol = 1)
  
  writeData(wb, "Productions", r3_cte_diff, startRow = 14, startCol = 9)
  
  ### TfN
  writeData(wb, "Productions", "TfN OUTPUT", startRow = 25, startCol = 1)
  mergeCells(wb, "Productions", rows = 25, cols = 1:7)
  addStyle(wb, "Productions", style, rows = 25, cols = 1:7)
  
  writeData(wb, "Productions", "TfN vs CTE", startRow = 25, startCol = 9)
  mergeCells(wb, "Productions", rows = 25, cols = 9:15)
  addStyle(wb, "Productions", style, rows = 25, cols = 9:15)
  
  writeData(wb, "Productions", r3_tfn, startRow = 26, startCol = 1)
  
  writeData(wb, "Productions", r3_tfn_diff, startRow = 26, startCol = 9)
  
  # NTEM
  writeData(wb, "Productions", "NTEM OUTPUT", startRow = 37, startCol = 1)
  mergeCells(wb, "Productions", rows = 37, cols = 1:7)
  addStyle(wb, "Productions", style, rows = 37, cols = 1:7)
  
  writeData(wb, "Productions", "NTEM vs CTE", startRow = 37, startCol = 9)
  mergeCells(wb, "Productions", rows = 37, cols = 9:15)
  addStyle(wb, "Productions", style, rows = 37, cols = 9:15)
  
  writeData(wb, "Productions", r3_ntem, startRow = 38, startCol = 1)
  
  writeData(wb, "Productions", r3_ntem_diff, startRow = 38, startCol = 9)
  
  saveWorkbook(wb, str_c(r3_dir, "Comparison Report.xlsx"), overwrite = TRUE)
  
}

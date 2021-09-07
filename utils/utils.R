# Create NTS C Folder in C Drive ------------------------------------------
create_nts_c <- function(user = Sys.info()[[6]]){
  
  main_dir <- paste0("C:/Users/", user, "/Documents/NTS_D/")
  
  # Unclassified build
  dir.create(paste0(main_dir, "unclassified builds/"), showWarnings = FALSE, recursive = TRUE)
  
  # Classified build
  dir.create(paste0(main_dir, "classified builds/"), showWarnings = FALSE, recursive = TRUE)
  
  # Imports
  
  dir.create(paste0(main_dir, "import/ub_columns/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(main_dir, "import/land_use/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(main_dir, "import/hb_trip_rates/"), showWarnings = FALSE, recursive = TRUE)
  
  # Outputs
  
  dir.create(paste0(main_dir, "outputs/hb/hb_trip_rates/Reports/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(main_dir, "outputs/hb/hb_trip_rates/hb_mode_time_split/"), showWarnings = FALSE, recursive = TRUE)
  
  # Productions Reporting output
  dir.create(paste0(main_dir, "outputs/hb/Productions reporting/P - AT - NTEM_TT/Production Comparisons"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(main_dir, "outputs/hb/Productions reporting/P - AT - NTEM_TT/Production Purpose Mode"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(main_dir, "outputs/hb/Productions reporting/P - MSOA/Mode Time Split/By Area Type/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(main_dir, "outputs/hb/Productions reporting/P - MSOA/Purpose/By Area Type/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(main_dir, "outputs/hb/Productions reporting/Purpose Mode Report/"), showWarnings = FALSE, recursive = TRUE)
  
  
  # NHB
  dir.create(paste0(main_dir, "outputs/nhb/Reports/"), showWarnings = FALSE, recursive = TRUE)
  
  # Lookups Copy
  
  lookups_y <- list.files("Y:/NTS/lookups", recursive = TRUE)
  lookups_y <- paste0("Y:/NTS/lookups/", lookups_y)

  dir.create(paste0(main_dir, "lookups/ntem/"), showWarnings = FALSE, recursive = TRUE)
  
  file.copy(lookups_y, paste0(main_dir, "lookups/"), overwrite = TRUE, recursive = FALSE)
  
}

create_nts_c()

# Install (if needed) and load libraries --------

library_c <- function(packages_list) {
  "
  Description
  ----------
  - Install packages if not previously installed
  - Load in packages
  
  Parameters
  ----------
  packages_list:
    A vector of packages required to run the main function
  
  Return
  ----------
  A boolean list of TRUE indicating all packages are loaded
  
  "
  
  # Packages not installed
  packages_new <- packages_list[!(packages_list %in% installed.packages()[,"Package"])]
  
  # Install packages
  if(length(packages_new)) install.packages(packages_new)
  
  # Load packages
  lapply(packages_list, require, character.only = TRUE)
  
}

# Source lookup -----------------------------------------------------------

source_lookups <- function(username){
  
  lookups_f_dir <- str_c("C:/Users/", username, 
                         "/Documents/GitHub/NTS-Processing/lookups.R")
  
  if(file.exists(lookups_f_dir)){
    
    source(lookups_f_dir)
    
  } else {
    
    stop("Github Folder not in documents - add custom path 'lookups_dir' to lookups.R in NTS Processing")
    
  }
  
}

factor_p <- function(df){
  
  df %>% 
    mutate(p = case_when(
      p == 1 ~ "Commute",
      p == 2 ~ "Business",
      p == 3 ~ "Education",
      p == 4 ~ "Shopping",
      p == 5 ~ "PB", 
      p == 6 ~ "Leisure",
      p == 7 ~ "Visit Friends",
      p == 8 ~ "Holiday/Day Trip")) %>% 
    mutate(p = factor(p, levels = c("Commute",
                                    "Business",
                                    "Education",
                                    "Shopping",
                                    "PB",
                                    "Leisure",
                                    "Visit Friends",
                                    "Holiday/Day Trip")))
  
}

factor_m <- function(df, which_version){
  
  if(which_version == "tfn"){
    
    df %>% 
      mutate(m = case_when(
        m == "m1" ~ "Walk",
        m == "m2" ~ "Cycle",
        m == "m3" ~ "Car/Van",
        m == "m5" ~ "Bus", 
        m == "m6" ~ "Rail")) %>% 
      mutate(m = factor(m, levels = c("Walk",
                                      "Cycle",
                                      "Car/Van",
                                      "Bus",
                                      "Rail")))
    
  } else if(which_version %in% c("ntem", "cte")){
    
    df %>% 
      mutate(m = case_when(
        m == "m1" ~ "Walk",
        m == "m2" ~ "Cycle",
        m == "m3" ~ "Car/Van Driver",
        m == "m4" ~ "Car/Van Passenger",
        m == "m5" ~ "Bus", 
        m == "m6" ~ "Rail")) %>% 
      mutate(m = factor(m, levels = c("Walk",
                                      "Cycle",
                                      "Car/Van Driver",
                                      "Car/Van Passenger",
                                      "Bus",
                                      "Rail")))
    
  }
  
}

factor_tp <- function(df, which_version){
  
  df %>% 
    mutate(tp = case_when(
      tp == "tp1" ~ "AM",
      tp == "tp2" ~ "IP",
      tp == "tp3" ~ "PM",
      tp == "tp4" ~ "OP", 
      tp == "tp5" ~ "SAT", 
      tp == "tp6" ~ "SUN")) %>% 
    mutate(tp = factor(tp, levels = c("AM",
                                      "IP",
                                      "PM",
                                      "OP",
                                      "SAT",
                                      "SUN")))
}

plot_comparison <- function(productions, name1, name2){
  
  xlabel <- ifelse(name1 == "tfn", "TfN",
                   ifelse(name1 == "ntem", "NTEM (TfN Classified)",
                          "CTripEnd"))
  
  ylabel <- ifelse(name2 == "tfn", "TfN",
                   ifelse(name2 == "ntem", "NTEM",
                          "CTripEnd"))
  
  xlabel <- str_c(xlabel, " Productions")
  ylabel <- str_c(ylabel, " Productions")
  
  single_plot <- productions %>%
    mutate(area_type = factor(area_type, levels = 1:8)) %>% 
    ggplot(aes_string(x = name1, y = name2)) +
    geom_smooth(method = "lm", se = FALSE, colour = "red", formula = y ~ x) +
    stat_poly_eq(formula = y ~ x, 
                 aes(label = paste0(..eq.label..)), 
                 parse = TRUE,
                 colour = "red") +
    stat_poly_eq(formula = y ~ x, 
                 aes(label = paste0(..rr.label..)), 
                 parse = TRUE,
                 colour = "red",
                 label.y = 0.9) +
    geom_point() +
    # geom_point(aes(colour = area_type)) +
    # scale_fill_manual(values = c("Red", "DarkBlue", "Cyan", "lime", "darkgreen", "purple", "orange", "black")) +
    xlab(xlabel) +
    ylab(ylabel) +
    geom_rangeframe() +
    theme_tufte()
  
}

plot_comparison_at <- function(single_plot){
  
  single_plot +
    geom_point(aes(colour = area_type)) +
    scale_fill_manual(values = c("Red", "DarkBlue", "Cyan", "lime", "darkgreen", "purple", "orange", "black"))
  
}

plot_purpose_comparison <- function(single_plot, 
                                    wrap_var1, 
                                    wrap_var2 = ".",
                                    ncolumns = 2){
  
  wrap_formula <- as.formula(paste(wrap_var1, "~", wrap_var2))
  
  single_plot +
    facet_wrap(wrap_formula , ncol = ncolumns, scales = "free") +
    geom_blank() +
    theme(strip.text = element_text(face="bold", size = 12),
          axis.title.x = element_text(size = 17),
          axis.title.y = element_text(size = 17))
  
}

save_plot <- function(dir, plot_name, w, l){
  
  png(filename = dir,
      type = "cairo",
      units = "in", 
      width = w, 
      height = l, 
      pointsize = 12, 
      res = 300)
  
  print(plot_name)
  dev.off()
  
}

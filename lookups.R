join_lookup <- function(df, lookup_csv, keys, id,
                        filter_id = FALSE, 
                        filter_na = FALSE,
                        variable_expansion = FALSE) {
  
  lookup_dir <- "C:/Users/Pluto/Documents/NTS_C/lookups/"
  
  # Read in lookup csv
  lookup <- read_csv(str_c(lookup_dir, lookup_csv, ".csv"))
  
  # Expand variable if reqired
  
  if(variable_expansion[1] != FALSE) {
    
    lookup <- reduce(variable_expansion, function(...) separate_rows(..., sep = "_"), .init = lookup)
    
  }
  
  # Convert to double
  lookup <- mutate_at(lookup, all_of(keys), as.double)
  
  # Merge df with lookup by keys
  df <- df %>%
    left_join(lookup, by = keys)
  
  # Any extra filters?
  if(filter_id != FALSE) {
    
    df <- df %>%
      filter(is.na(get(id)) | get(id) != filter_id)
    
  }
  
  if(filter_na != FALSE) {
    
    df <- df %>%
      filter(!is.na(get(id)))
    
  }
  
  return(df)
  
}


# Trip Purposes -----------------------------------------------------------

lu_trip_origin <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "trip_origin---TripPurpFrom_B01ID",
              keys = "TripPurpFrom_B01ID",
              id = "trip_origin",
              variable_expansion = "TripPurpFrom_B01ID")
  
}

lu_hb_purpose <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "hb_purpose---TripPurpTo_B01ID--MainMode_B11ID",
              keys = c("TripPurpTo_B01ID", "MainMode_B11ID"),
              id = "hb_purpose",
              variable_expansion = c("TripPurpTo_B01ID", "MainMode_B11ID"))
  
}

lu_nhb_purpose <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "nhb_purpose---TripPurpTo_B01ID--MainMode_B11ID",
              keys = c("TripPurpTo_B01ID", "MainMode_B11ID"),
              id = "nhb_purpose",
              variable_expansion = c("TripPurpTo_B01ID", "MainMode_B11ID"))
  
}

lu_nhb_purpose_hb_leg <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "nhb_purpose_hb_leg---TripPurpFrom_B01ID--MainMode_B11ID",
              keys = c("TripPurpFrom_B01ID", "MainMode_B11ID"),
              id = "nhb_purpose_hb_leg",
              variable_expansion = c("TripPurpFrom_B01ID", "MainMode_B11ID"))
  
}

# Other variables ---------------------------------------------------------

lu_gender <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "gender---Sex_B01ID--Age_B01ID",
              keys = c("Sex_B01ID",  "Age_B01ID"),
              id = "gender",
              variable_expansion = c("Sex_B01ID",  "Age_B01ID"))
  
}

lu_age_work_status <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "age_work_status--Age_B01ID---EcoStat_B01ID",
              keys = c("Age_B01ID", "EcoStat_B01ID"),
              id = "age_work_status",
              filter_na = TRUE,
              variable_expansion = c("Age_B01ID", "EcoStat_B01ID"))
  
}

lu_hh_type <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "hh_type--HHoldNumAdults---NumCarVan_B02ID",
              keys = c("HHoldNumAdults", "NumCarVan_B02ID"),
              id = "hh_type",
              variable_expansion = c("HHoldNumAdults", "NumCarVan_B02ID"),
              filter_na = TRUE)
  
}

lu_traveller_type <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "traveller_type---age_work_status--gender--hh_type",
              keys = c("age_work_status", "gender", "hh_type"),
              id = "traveller_type")
}



lu_soc_cat<- function(df){
  
  join_lookup(df = df,
              lookup_csv = "soc_cat---XSOC2000_B02ID--Age_B01ID",
              keys = c("XSOC2000_B02ID", "Age_B01ID"),
              id = "soc_cat",
              variable_expansion = c("XSOC2000_B02ID", "Age_B01ID"),
              filter_na = TRUE)
  
}

lu_main_mode <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "main_mode---MainMode_B11ID",
              keys = "MainMode_B11ID",
              id = "main_mode",
              variable_expansion = "MainMode_B11ID",
              filter_na = TRUE)
  
}

lu_start_time <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "start_time---TravelWeekDay_B01ID--TripStart_B01ID",
              keys = c("TravelWeekDay_B01ID", "TripStart_B01ID"),
              id = "start_time",
              variable_expansion = c("TravelWeekDay_B01ID", "TripStart_B01ID"),
              filter_na = TRUE)
  
}

lu_end_time<- function(df){
  
  join_lookup(df = df,
              lookup_csv = "end_time---TravelWeekDay_B01ID--TripEnd_B01ID",
              keys = c("TravelWeekDay_B01ID","TripEnd_B01ID"),
              id = "end_time",
              variable_expansion = c("TravelWeekDay_B01ID", "TripEnd_B01ID"),
              filter_na = TRUE)
  
}

lu_tfn_area_type <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "tfn_area_type---PSUPSect",
              keys = "PSUPSect",
              id = "tfn_area_type",
              filter_na = TRUE)
  
}

tt1 <- read_csv("C:/Users/Pluto/Documents/NTS_C/lookups/traveller_type---age_work_status--gender--hh_type.csv")

lookup = tt1
keys = c("age_work_status", "gender", "hh_type", "hh_adults", "hh_cars", "ns_sec", "soc_cat")
id = "traveller_type"
variable_expansion <- keys

reduce(variable_expansion, function(...) separate_rows(..., sep = "_"), .init = lookup)

age_work_status	gender	hh_type	hh_adults	hh_cars	ns_sec	soc_cat


join_lookup(df = df,
            lookup_csv = "traveller_type---age_work_status--gender--hh_type",
            keys = c("age_work_status", "gender", "hh_type"),
            id = "traveller_type")

rep(9:40, each = 4)

9_10_11_12
13_14_15_16



output <- list()

for(i in 9:88){
  
  if(i %% 4 == 1){
    
    output[i] <- str_c(i, "_", i+1, "_", i+2, "_", i+3)
    
  }
  
}



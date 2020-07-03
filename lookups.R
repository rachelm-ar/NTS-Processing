join_lookup <- function(df, lookup_csv, keys, id,
                        filter_id = FALSE, filter_na = FALSE) {
  
  # Read in lookup csv
  lookup <- read_csv(str_c(lookup_dir, lookup_csv, ".csv"))
  
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

lu_trip_origin <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "trip_origin---TripPurpFrom_B01ID",
              keys = "TripPurpFrom_B01ID",
              id = "trip_origin")
  
}

lu_hb_purpose <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "hb_purpose---TripPurpTo_B01ID",
              keys = "TripPurpTo_B01ID",
              id = "hb_purpose")
  
}

lu_nhb_purpose_hb_leg <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "nhb_purpose_hb_leg---TripPurpFrom_B01ID",
              keys = "TripPurpFrom_B01ID",
              id = "nhb_purpose_hb_leg")
  
}

lu_nhb_purpose <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "nhb_purpose---TripPurpTo_B01ID",
              keys = "TripPurpTo_B01ID",
              id = "nhb_purpose")
  
}

lu_gender <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "gender---Sex_B01ID",
              keys = "Sex_B01ID",
              id = "gender")
  
}

lu_age_work_status <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "age_work_status--Age_B01ID---EcoStat_B01ID",
              keys = c("Age_B01ID","EcoStat_B01ID"),
              id = "age_work_status",
              filter_na = TRUE)
  
}

lu_cars <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "cars---NumCarVan_B02ID--HHoldNumAdults",
              keys = c("NumCarVan_B02ID","HHoldNumAdults"),
              id = "cars",
              filter_na = TRUE)
  
}

lu_hh_adults <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "hh_adults---HHoldNumAdults",
              keys = "HHoldNumAdults",
              id = "hh_adults")
  
}

lu_main_mode <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "main_mode---MainMode_B04ID",
              keys = "MainMode_B04ID",
              id = "main_mode",
              filter_id = 99,
              filter_na = TRUE)
  
}

lu_soc_cat<- function(df){
  
  join_lookup(df = df,
              lookup_csv = "soc_cat---XSOC2000_B02ID",
              keys = "XSOC2000_B02ID",
              id = "soc_cat",
              filter_id = -8)
  
}

lu_start_time <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "start_time---TravDay--TripStart_B01ID",
              keys = c("TravDay", "TripStart_B01ID"),
              id = "start_time")
  
}

lu_end_time<- function(df){
  
  join_lookup(df = df,
              lookup_csv = "end_time---TravDay--TripEnd_B01ID",
              keys = c("TravDay","TripEnd_B01ID"),
              id = "end_time")
  
}

lu_area_type <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "area_type---HHoldAreaType1_B01ID",
              keys = "HHoldAreaType1_B01ID",
              id = "area_type",
              filter_na = TRUE)
  
}

lu_tfn_area_type <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "tfn_area_type---HHoldOSWard_B01ID",
              keys = "HHoldOSWard_B01ID",
              id = "tfn_area_type",
              filter_na = TRUE)
  
}

lu_ca <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "ca---NumCarVan_B02ID",
              keys = "NumCarVan_B02ID",
              id = "ca",
              filter_na = TRUE)
  
}

lu_traveller_type <- function(df){
  
  join_lookup(df = df,
              lookup_csv = "traveller_type---traveller_type_char",
              keys = "traveller_type_char",
              id = "traveller_type")
}

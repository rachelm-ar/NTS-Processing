require(tidyverse)

# Set export path
export_path <- "Y:/NTS/i_e_factors/"


nts_ntem_df <- read_csv("Y:/NTS/import/classified_nts_pre-weighting.csv",
                        guess_max = 10^7)

trip_origin = 'hb'

internal_external <- nts_ntem_df %>%
  mutate(trip_weights = W1 * W5xHh * W2) %>%
  filter(trip_origin == trip_origin) %>%
  group_by(hb_purpose, main_mode, TripOrigUA2009_B01ID,
           TripDestUA2009_B01ID, TripOrigGOR_B02ID, TripDestGOR_B02ID) %>%
  summarise(trips = sum(trip_weights, na.rm = TRUE))

remove_null <- TRUE
write <- TRUE

# TODO: Region filter

# County filter
# Filter to county
target_counties <- list(c(16))
target_names <- list("Teesside")

if(length(target_counties) == length(target_names)){

  for(i in 1:length(target_counties)){

    target_dat <- internal_external %>%
      mutate(agg_origin = case_when(
        TripOrigCounty_B01ID %in% target_counties ~ 'internal',
        TRUE ~ 'external'),
        agg_destination = case_when(
          TripDestCounty_B01ID %in% target_counties ~ 'internal',
          TRUE ~ 'external')) %>%
      ungroup() %>%
      group_by(hb_purpose, main_mode, agg_origin, agg_destination) %>%
      summarise(trips = sum(trips, na.rm = TRUE)) %>%
      ungroup() %>%
      spread(key = agg_destination, value = trips) %>%
      mutate(internal = replace_na(internal, 0),
             external = replace_na(external, 0),
             total = internal + external,
             agg_d_internal = internal/total,
            agg_d_external = external/total) %>%
      select(hb_purpose, main_mode, agg_origin,
             agg_d_internal, agg_d_external) %>%
      rename(p = hb_purpose,
             m = main_mode)

    if(remove_null){
      target_dat <- target_dat %>%
        filter(p != 99) %>%
        filter(m != 99)
    }
    # TODO: Check total length is 80 (8*5*2) or something dropped out

    if(write){
      write_path <- paste0(export_path, target_names[[i]], '_i_e_factors.csv')
      target_dat %>% write_csv(write_path)
    }
  }
}

library(tidyverse)

test <- rnorm(100, mean = 7, sd = 5)

test <- test[test > 0]

#####

library(scales)

nts %>%
  select(main_mode, TripDisIncSW) %>%
  mutate(dist_km = TripDisIncSW * 1.60934) %>%
  mutate(bands_km = cut(dist_km, c(0,1,2,5,10,15,25,50,100,200,400,10000))) %>%
  group_by(main_mode, bands_km) %>%
  summarise(trips = n()) %>%
  ungroup() %>%
  mutate(main_mode = factor(main_mode, levels = 1:8, labels = c("Walk",
                                                                "Cycle", 
                                                                "Car",
                                                                "Van",
                                                                "Bus",
                                                                "Surface Rail",
                                                                "Light rail", 
                                                                "Air"))) %>%
  mutate(trips = trips) %>%
  ggplot(aes(bands_km, trips)) + 
  xlab("Distance Bands (KM)") +
  ylab("# of Trips (in thousands)") +
  labs(color='Mode')  +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  scale_color_manual(values=c("red", "blue", "green", "purple", "orange", "cyan", "grey", "black")) +
  geom_line(aes(group = main_mode, colour = main_mode))


  group_by(main_mode, bands_km) %>%
  summarise(trips = n()) %>%
  ungroup() %>%
  geom_d


#####

nts <- read_csv("C:/Users/Pluto/Documents/NTS_C/classified builds/cb_tfn.csv")

nts <- nts %>%
  mutate(passenger = ifelse(MainMode_B03ID %in% c(7, 8, 11, 12, 15, 16, 26, 27), 1, 0),
         driver = ifelse(MainMode_B03ID %in% c(4, 5, 6, 9, 10, 13, 14, 17), 1, 0))
  
# Day type
nts <- nts %>%
  mutate(day_type = ifelse(start_time %in% 1:4, "weekday", "weekend"))

# Define trip from, trip to
nts <- nts %>%
  filter(!(TripPurpFrom_B01ID == 17 & TripPurpTo_B01ID == 23),
         !(TripPurpFrom_B01ID == 23 & TripPurpTo_B01ID == 17),
         !(TripPurpFrom_B01ID == 17 & TripPurpFrom_B01ID == 17)) %>%
  mutate(trip_direction = ifelse(TripPurpFrom_B01ID %in% c(17,23), "FH",
                                 ifelse(TripPurpTo_B01ID %in% c(17,23), "TH", "NHB")))

nts <- nts %>%
  filter(!is.na(trip_purpose))

tohometrips <- nts %>%
  filter(TripPurpTo_B01ID %in% c(17,23)) %>%
  filter(!(TripPurpFrom_B01ID %in% c(16,22) & TripPurpTo_B01ID %in% c(17,23))) %>%
  mutate(TripPurpFrom_B01ID = case_when(
         TripPurpFrom_B01ID %in% c(1, 18) ~ 1,
         TripPurpFrom_B01ID %in% c(2, 19) ~ 2,
         TripPurpFrom_B01ID %in% c(3, 20) ~ 3,
         TripPurpFrom_B01ID %in% c(4, 5, 21) ~ 4,
         TripPurpFrom_B01ID %in% c(6, 7, 8) ~ 5,
         TripPurpFrom_B01ID %in% c(9, 11, 12, 13, 22, 16) ~ 6,
         TripPurpFrom_B01ID %in% c(10) ~ 7,
         TripPurpFrom_B01ID %in% c(14) ~ 8,
         TripPurpFrom_B01ID %in% c(15) ~ 8,
       )) %>%
  mutate(trip_purpose = TripPurpFrom_B01ID)

nts2 <- nts %>%
  filter(!TripPurpTo_B01ID %in% c(17,23)) %>%
  bind_rows(tohometrips)

nts3 <- nts2 %>%
  mutate(weights = W1 * W2 * W5 * sw_weight) %>%
  mutate(passenger = passenger * W2 * W5 * sw_weight,
         driver = driver * W2 * W5 * sw_weight) %>%
  select(main_mode, trip_purpose, tfn_area_type, start_time, TripOrigGOR_B02ID, TripDestGOR_B02ID, day_type, trip_direction, TripDisIncSW, passenger, driver, weights) %>%
  group_by(main_mode, trip_purpose, tfn_area_type, start_time, TripOrigGOR_B02ID, TripDestGOR_B02ID, day_type, trip_direction, driver, TripDisIncSW) %>%
  summarise(trips = sum(weights),
            passenger = sum(passenger),
            driver = sum(driver)) %>%
  ungroup() %>%
  mutate(trip_mile = TripDisIncSW * trips)

nts3 %>%
  write_csv("C:/Users/Pluto/Documents/NTS_C/nts_tld_all_vars.csv")

## CO

library(tidyverse)

co <- read_csv("C:/Users/Pluto/Documents/NTS_C/nts_tld_all_vars_car_occupancy.csv")

co2 <- co %>%
  na.omit() %>% 
  filter(main_mode == 3, day_type == "weekday") %>% 
  mutate(trip_purpose_agg = case_when(
    trip_purpose == 1 ~ "HBW",
    trip_purpose == 2 ~ "HBB",
    trip_purpose == 3 ~ "HBE",
    trip_purpose == 4 ~ "HBS",
    trip_purpose == 5 ~ "HBPB",
    trip_purpose == 6 ~ "HBS",
    trip_purpose == 7 ~ "HBVF",
    trip_purpose == 8 ~ "HBH",
    trip_purpose %in% 12:18 ~ "NHB",
  ))

p1 <- co2 %>% 
  unite(purpose_direction, c(trip_purpose_agg, trip_direction), sep = "_") %>% 
  mutate(purpose_direction = ifelse(trip_purpose %in% 12:18, "NHB", purpose_direction),
         purpose_direction = factor(purpose_direction)) %>% 
  select(purpose_direction, TripDisIncSW, trips, passenger, driver, trip_mile) %>% 
  mutate(bands = cut(TripDisIncSW, c(0,1,2,3,4,5,6,7,8,9,10,12.5,15,17.5,20,25,30,35,40,50,75,100,150,200,250,300,400,600,999), 
                     right = FALSE,
                     labels = FALSE,
                     ordered_result = TRUE)) %>%
  mutate(bands = factor(bands)) %>% 
  group_by(purpose_direction, bands) %>%
  summarise(passenger = sum(passenger),
            driver = sum(driver)) %>% 
  ungroup() %>%
  mutate(band2 = case_when(
    bands %in% 1:2 ~ 1,
    bands %in% 3:5 ~ 2,
    bands %in% 6:10 ~ 3,
    bands %in% 11:15 ~ 4,
    bands %in% 16:19 ~ 5,
    bands %in% 20:21 ~ 6,
    bands %in% 22:24 ~ 7,
    bands %in% 25:28 ~ 8
  ))

p1 %>%
  mutate(occupancy = 1 + passenger/driver) %>%
  group_by(purpose_direction) %>%
  summarise(mean(occupancy, na.rm = TRUE))
  

p2 <- p1 %>%
  mutate(occupancy = 1 + passenger/driver) %>% 
  filter(str_detect(purpose_direction, "^HB")) %>%
  mutate(overall_purpose = str_sub(purpose_direction, 1, 3)) %>%
  mutate(overall_purpose = ifelse(overall_purpose == "HBV", "HBVF",
                                  ifelse(overall_purpose == "HBP", "HBPB", overall_purpose))) %>% 
  mutate(overall_purpose = factor(overall_purpose)) %>%
  mutate(purpose_direction = str_sub(purpose_direction, -2, -1)) %>% 
  ggplot(aes(x = bands, y = occupancy)) +
  geom_path(aes(colour = purpose_direction, group = purpose_direction)) +
  facet_wrap(overall_purpose ~ ., ncol = 2)

p3 <- p1 %>%
  group_by(purpose_direction, band2) %>%
  mutate(passenger2 = sum(passenger),
         driver2 = sum(driver)) %>% 
  mutate(occupancy = 1 + passenger2/driver2) %>% 
  filter(str_detect(purpose_direction, "^HB")) %>%
  mutate(overall_purpose = str_sub(purpose_direction, 1, 3)) %>%
  mutate(overall_purpose = ifelse(overall_purpose == "HBV", "HBVF",
                                  ifelse(overall_purpose == "HBP", "HBPB", overall_purpose))) %>% 
  mutate(overall_purpose = factor(overall_purpose)) %>%
  mutate(purpose_direction = str_sub(purpose_direction, -2, -1)) %>%
  ggplot(aes(x = bands, y = occupancy)) +
  geom_path(aes(colour = purpose_direction, group = purpose_direction)) +
  facet_wrap(overall_purpose ~ ., ncol = 2)

library(Cairo)  

png(filename="Y:/NTS/TLD_by_purpose_plot_agg.png",
    type="cairo",
    units="in", 
    width=8, 
    height=8, 
    pointsize=12, 
    res=300)
print(p3)
dev.off()

##


## SS


nts2 %>%
  mutate(weights = W1 * W2 * W5 * sw_weight) %>%
  select(main_mode, trip_purpose, tfn_area_type, start_time, TripOrigCounty_B01ID, TripOrigGOR_B02ID, TripDestCounty_B01ID, TripDestGOR_B02ID, day_type, trip_direction, TripDisIncSW, weights) %>%
  group_by(main_mode, trip_purpose, tfn_area_type, start_time, TripOrigCounty_B01ID, TripOrigGOR_B02ID, TripDestCounty_B01ID, TripDestGOR_B02ID, day_type, trip_direction, TripDisIncSW) %>%
  summarise(trips = sum(weights)) %>%
  ungroup() %>%
  mutate(trip_mile = TripDisIncSW * trips) %>%
  write_csv("C:/Users/Pluto/Documents/NTS_C/nts_tld_all_vars.csv")

##

  
nts3 <- nts2 %>%
  select(main_mode, trip_purpose, TripOrigCounty_B01ID, TripDestCounty_B01ID, TripOrigGOR_B02ID, TripDestGOR_B02ID,
         trip_direction, day_type, W1, W2, W5, sw_weight) %>%
  mutate(weights = W1 * W2 * W5 * sw_weight) %>%
  select(-c(W1, W2, W5, sw_weight))

nts3 %>%
  group_by(main_mode, trip_purpose, TripOrigCounty_B01ID, TripDestCounty_B01ID, TripOrigGOR_B02ID, TripDestGOR_B02ID, trip_direction, day_type) %>%
  summarise(trips = sum(weights, na.rm = TRUE)) %>%
  ungroup() %>%
  write_csv("Y:/NTS/nts_tld_county_gor.csv")

nts2 %>%
  group_by(main_mode, trip_purpose, TripOrigCounty_B01ID, TripDestCounty_B01ID, TripOrigGOR_B02ID, TripDestGOR_B02ID, trip_direction) %>%
  summarise(trips = sum(weights, na.rm = TRUE)) %>%
  ungroup() %>%
  count(trip_purpose)
  filter(!is.na(trip_purpose)) %>%
  filter(trip_purpose != 99) %>%
  write_csv("C:/Users/Pluto/Documents/NTS_C/nts_tld_county_gor.csv")


# Code purpose: Build mode specific PA to OD factors

# Def mile to km const
M_TO_KM = 1.60934

# Import ntem_build - NTEM segmented dataset
nts_ntem_df <- read_csv('Y:/NTS/import/classified_nts_pre-weighting.csv', guess_max = 10^9)

# Trip length in miles
miles_test <- nts_ntem_df %>%
  select(TripDisIncSW, main_mode) %>%
  filter(TripDisIncSW < 20 & main_mode == 3)
hist(miles_test$TripDisIncSW)
miles_test <- miles_test %>%
  group_by(TripDisIncSW) %>%
  count()

export <- 'Y:/NTS/trip_lengths/at_hb/'

# Add trip weighting
nts_ntem_df <- nts_ntem_df %>%
  mutate(weighted_trip = W1 * W5xHh * W2)

# Fill in null SOC cats with 0, for zero segments
nts_ntem_df <- nts_ntem_df %>%
  mutate(soc_cat = case_when(
    soc_cat == -9 ~ 0,
    TRUE ~ soc_cat
  ))

### Start of HB tlb run

# Subset down
# Not spread just now

# North only
north_la <- c('E06000001', 'E06000002', 'E06000003', 'E06000004', 'E06000005', 'E06000006',
              'E06000007', 'E06000008', 'E06000009', 'E06000010', 'E06000011', 'E06000012',
              'E06000013', 'E06000014', 'E06000021', 'E06000047', 'E06000049', 'E06000050',
              'E06000057', 'E07000026', 'E07000027', 'E07000028', 'E07000029', 'E07000030',
              'E07000031', 'E07000033', 'E07000034', 'E07000035', 'E07000037', 'E07000038',
              'E07000117', 'E07000118', 'E07000119', 'E07000120', 'E07000121', 'E07000122',
              'E07000123', 'E07000124', 'E07000125', 'E07000126', 'E07000127', 'E07000128',
              'E07000137', 'E07000142', 'E07000163', 'E07000164', 'E07000165', 'E07000166',
              'E07000167', 'E07000168', 'E07000169', 'E07000170', 'E07000171', 'E07000174',
              'E07000175', 'E07000198', 'E08000001', 'E08000002', 'E08000003', 'E08000004',
              'E08000005', 'E08000006', 'E08000007', 'E08000008', 'E08000009', 'E08000010',
              'E08000011', 'E08000012', 'E08000013', 'E08000014', 'E08000015', 'E08000016',
              'E08000017', 'E08000018', 'E08000019', 'E08000021', 'E08000022', 'E08000023',
              'E08000024', 'E08000032', 'E08000033', 'E08000034', 'E08000035', 'E08000036',
              'E08000037', 'W06000001', 'W06000002', 'W06000003', 'W06000004', 'W06000005',
              'W06000006')

teesside_la <- c('E06000001', 'E06000002', 'E06000003', 'E06000004', 'E06000005')

# 1 = North East
# 2 = North West
# 3 = Yorkshire and the Humber
# 4 = East Midlands - yes please
north_region <- c(1,2,3,4)
north_east_region <- c(1)

# Region count
region_count <- nts_ntem_df %>%
  select(TripOrigGOR_B02ID, TripDestGOR_B02ID) %>%
  group_by(TripOrigGOR_B02ID, TripDestGOR_B02ID) %>%
  count()
# Looks quite okay

# Fix target trip origin hb/nhb
target_trip_origin <- 'hb'

# Weekdays only
weekdays <- c(1,2,3,4,5)
# Last 3 years only
years <- c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
           2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)

# Removed years & weekdays for fullest possible sample

# Bin for atl (average trip length)

build_atl <- function(nts_ntem_df,
                      target_trip_origin,
                      target_params,
                      target_bins
                      ) {

  for(i in 1:nrow(target_params)){

    # Filter down nts NTEM df
    trip_lengths <- nts_ntem_df %>%
      select(SurveyYear, TravDay, HHoldOSLAUA_B01ID, soc_cat, ns_sec, main_mode, hb_purpose, nhb_purpose,
             trip_origin, start_time, TripDisIncSW, TripOrigGOR_B02ID, TripDestGOR_B02ID, tfn_area_type, weighted_trip) %>%
      filter(!is.na(weighted_trip)) %>%
      filter(HHoldOSLAUA_B01ID %in% north_la) %>%
      # filter(TripOrigGOR_B02ID %in% north_region) %>%
      # filter(TripDestGOR_B02ID %in% north_region) %>%
      filter(trip_origin == target_trip_origin) %>%
      filter(TravDay %in% weekdays)

    # Match purpose to target purpose
    if(target_trip_origin == 'hb') {
      trip_lengths <- trip_lengths %>%
        rename(purpose = hb_purpose)
    } else if(target_trip_origin == 'nhb') {
      trip_lengths <- trip_lengths %>%
        rename(purpose = nhb_purpose)
    }

    atl_bin <- c(param_name=NULL, atl=NULL)
    sz_bin <- c(param_name=NULL, sample=NULL)

    # Define param name
    param_name <- ''
    print(param_name)

    # Get params from row
    if("purpose" %in% colnames(target_params)){
      purpose_sub <- as.character(target_params$purpose[[i]])
      param_name <- paste0(param_name, "p", purpose_sub)
      trip_lengths <- trip_lengths %>%
        filter(purpose == purpose_sub)
    }
    if("mode" %in% colnames(target_params)){
      mode_sub <- target_params$mode[[i]]
      param_name <- paste0(param_name, "_m", mode_sub)
      trip_lengths <- trip_lengths %>%
        filter(main_mode == mode_sub)
    }
    if("start_time" %in% colnames(target_params)){
      time_sub <- target_params$start_time[[i]]
      param_name <- paste0(param_name, "_tp", time_sub)
      trip_lengths <- trip_lengths %>%
        filter(start_time == time_sub)
    }
    if("soc_cat" %in% colnames(target_params)){
      soc_cat_sub <- target_params$soc_cat[[i]]
      param_name <- paste0(param_name, "_soc", soc_cat_sub)
      trip_lengths <- trip_lengths %>%
        filter(soc_cat == soc_cat_sub)
    }
    if("ns_sec" %in% colnames(target_params)){
      ns_sec_cat_sub <- target_params$ns_sec[[i]]
      param_name <- paste0(param_name, "_ns", ns_sec_cat_sub)
      trip_lengths <- trip_lengths %>%
        filter(ns_sec == ns_sec_cat_sub)
    }
    if("tfn_area_type" %in% colnames(target_params)){
      at_sub <- target_params$tfn_area_type[[i]]
      param_name <- paste0(param_name, "_area_type", at_sub)
      trip_lengths <- trip_lengths %>%
        filter(tfn_area_type == at_sub)
    }

    # Print unique values of mode and purpose, eyeball if the filter worked
    if(exists("mode_sub")){
      mode_bin <- trip_lengths %>%
        select(main_mode) %>%
        distinct()
      print('Unique mode:')
      print(mode_bin)
    }
    if(exists("purpose_sub")){
      purpose_bin <- trip_lengths %>%
        select(purpose) %>%
        distinct()
      print('Unique purpose:')
      print(purpose_bin)
    }
    if(exists("time_sub")){
      time_bin <- trip_lengths %>%
        select(start_time) %>%
        distinct()
      print('Unique time')
      print(time_bin)
    }
    if(exists("at_sub")){
      at_bin <- trip_lengths %>%
        select(tfn_area_type) %>%
        distinct()
      print('Unique area type')
      print(at_bin)
    }

    # get set length for sample size oversight
    sample_size = nrow(trip_lengths)
    print(paste("Number of records:", sample_size))

    if(sample_size>0){

      # Append to sample bin
      sz_bin <- bind_rows(sz_bin, c(param_name = param_name, sample=sample_size))

      # Get trip lengths only
      tlo <- trip_lengths$TripDisIncSW
      hist(tlo, breaks = c(25))

      tlo_mean <- mean(tlo)*M_TO_KM
      tlo_sd <- sd(tlo)*M_TO_KM

      atl_bin <- bind_rows(atl_bin, c(param_name = param_name, atl=tlo_mean))

      breaks <- target_bins$min
      names(breaks) <- NULL

      # Name breaks
      tags <- NULL
      for (b in 1:nrow(target_bins)){
        target_bins$min[b]
        tags <- c(tags, paste0("(", target_bins$min[[b]], "-", target_bins$max[[b]], "]"))
      }

      breaks <- c(breaks, target_bins$max[length(target_bins$max)])

      # Build placeholders for join
      tlb_desc <- tags %>%
        as.tibble() %>%
        rename(tlb_desc = value) %>%
        mutate(ph=1)

      purpose_frame <- purpose_sub %>%
        as.tibble() %>%
        rename(purpose = value) %>%
        mutate(ph=1)

      placeholder_frame <- tlb_desc %>%
        left_join(purpose_frame) %>%
        select(-ph) %>%
        distinct() %>%
        group_by(purpose) %>%
        mutate(tlb_index = row_number(),
               main_mode = mode_sub) %>%
        ungroup() %>%
        select(tlb_index, tlb_desc, purpose, main_mode) %>%
        mutate(purpose = as.integer(purpose))

      # bucketing values into bins
      trip_lengths <- trip_lengths %>%
        mutate(tlb_index = cut(TripDisIncSW,
                               breaks=breaks,
                               include.lowest=TRUE,
                               right=FALSE,
                               labels=FALSE)) %>%
        filter(purpose != 99 & !is.na(tlb_index)) %>%
        group_by(tlb_index, main_mode, purpose) %>%
        summarise(trips = sum(weighted_trip, na.rm=TRUE),
                  count = n(),
                  atl = weighted.mean(TripDisIncSW, weighted_trip, na.rm=TRUE)) %>%
        ungroup() %>%
        select(tlb_index, main_mode, purpose, trips, atl, count)

      trip_lengths <- placeholder_frame %>%
        full_join(trip_lengths) %>%
        mutate(trips = replace_na(trips, 0))

      # Get % share for each number of trips
      totals <- trip_lengths %>%
        select(tlb_desc, purpose, trips) %>%
        group_by(purpose) %>%
        summarise(total_trips = sum(trips,na.rm=TRUE))

      # Reattach - derive total
      trip_lengths <- trip_lengths %>%
        left_join(totals) %>%
        mutate(band_share = round(trips/total_trips, 3)) %>%
        select(-total_trips)

      # Adjust to km
      for (b in 1:nrow(target_bins)){
          km_tag <- paste0("(", target_bins$min[[b]]*M_TO_KM, "-", target_bins$max[[b]]*M_TO_KM, "]")
          trip_lengths$tlb_desc[b] <- km_tag
        }
      trip_lengths$atl <- trip_lengths$atl*M_TO_KM

      hist(trip_lengths$band_share)

      trip_lengths %>% write_csv(paste0(export, target_trip_origin, "_tlb_", param_name, ".csv"))
      }
    }

  # Write atl bin
  atl_bin %>% write_csv(paste0(export, target_trip_origin, "_ave_distance.csv"))
  sz_bin %>% write_csv(paste0(export, target_trip_origin, "_tld_sample_sizes.csv"))
}

# Run it

# Import target combos for parameters - loops will give segments that we don't want
target_params <- read_csv(paste0(export, "/target_output_params_hb.csv")) %>%
  filter(purpose %in% c(1,2,3,4,5,6,7,8))

target_bins <- read_csv(paste0(export, "/target_bins.csv"))

build_atl(nts_ntem_df,
          'hb',
          target_params,
          target_bins)

target_params <- read_csv(paste0(export, "/target_output_params_nhb.csv")) %>%
  filter(purpose %in% c(12, 13, 14, 15, 16, 18))

build_atl(nts_ntem_df,
          'nhb',
          target_params,
          target_bins)

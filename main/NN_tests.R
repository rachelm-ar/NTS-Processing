# Library --------------------------------------------------------------------
library_list <- c("dplyr",
                  "stringr",
                  "readr",
                  "tidyr",
                  "purrr",
                  "parallel")

library_c(library_list)

# import cb data
cb <- read_csv("D:/NTS/classified builds/cb_tfn_v3.csv")

out <- cb %>%
  group_by(HHoldGOR_B02ID, Settlement2011EW_B01ID, HHoldAreaType2_B01ID) %>%
  summarise(trips = sum(weighted_trips, na.rm = TRUE))
write_csv(out,"D:/NTS/AgBM/nts_settlement.csv")

# check
out <- cb %>%
  group_by(tfn_tt, tfn_at, hh_type, soc, ns, aws, gender) %>%
  summarise(trips = sum(weighted_trips, na.rm = TRUE))
write_csv(out,"D:/NTS/NoTEM/nts_check.csv")

out <- cb %>%
  group_by(HHoldNumAdults, NumCarVan) %>%
  summarise(trips = sum(weighted_trips, na.rm = TRUE))


# TODO list:
# address issue with dest of a preceding leg does not match the orig of the subsequent leg

### NEW METHOD - OUTPUT ACTIVITY DATABASE ###
# step 1 - filter data
cb <- cb %>%
  select(IndividualID, TripID, trip_group, p, trip_direction, 
         HHoldGOR_B02ID, TripOrigGOR_B02ID, TripDestGOR_B02ID,
         HHoldCounty_B01ID, TripOrigCounty_B01ID, TripDestCounty_B01ID,
         HHoldUA_B01ID, TripOrigUA_B01ID, TripDestUA_B01ID,
         tfn_tt, tfn_at, soc, ns, aws, gender, hh_type, occupant,
         main_mode, start_time, end_time, weighted_trips, TripDisIncSW)

# step 2 - create tour_id
nts <- cb %>%
  mutate(purpose = ifelse(trip_direction == "hb_to", 0, 
                          ifelse(trip_direction == 'nhb', p - 10, p))) %>% 
  arrange(IndividualID, trip_group, TripID) %>%
  group_by(IndividualID, trip_group) %>%
  mutate(tour_id = str_c("0_", paste0(purpose, collapse = "_"))) %>%
  ungroup()

nts <- nts %>%
  add_count(IndividualID, trip_group, name = "freq") %>%
  mutate(freq = 1 / freq) # each activity consists of multiple legs
  
# step 3 - aggregate to tfn_tt, tfn_at and GOR
# nts <- nts %>%
#   # merge escort trips to main purposes
#   group_by(tfn_tt, tfn_at, tour_id, HHoldGOR_B02ID, TripOrigGOR_B02ID, TripDestGOR_B02ID, main_mode, 
#            purpose, trip_direction, start_time, TripDisIncSW) %>%
#   summarise(freq = sum(freq * weighted_trips, na.rm = TRUE), 
#             trips = sum(weighted_trips, na.rm = TRUE)) %>%
#   filter(TripOrigGOR_B02ID %in% c(1:11), TripDestGOR_B02ID %in% c(1:11), 
#          start_time %in% c(1:6), !is.na(purpose), !is.na(trip_direction))
#
# step 4 - produce activity database
# act <- nts %>%
#   group_by(HHoldGOR_B02ID, tour_id) %>%
#   summarise(freq = sum(freq, na.rm = TRUE), trips = sum(trips, na.rm = TRUE)) %>%
#   filter(HHoldGOR_B02ID %in% c(1:11), endsWith(tour_id, "_0"))
# 
# # step 5 - produce mode-time split
# mts <- nts %>%
#   mutate(purpose = ifelse(trip_direction == 'hb_to', -p, ifelse(trip_direction == 'nhb', p - 10, p))) %>%
#   group_by(TripOrigGOR_B02ID, TripDestGOR_B02ID, main_mode, purpose, trip_direction, start_time) %>%
#   summarise(freq = sum(freq, na.rm = TRUE), trips = sum(trips, na.rm = TRUE)) %>%
#   filter(main_mode %in% c(1,2,3,5,6,7))
# write_csv(mts,"D:/NTS/AgBM/nts_mode_time_split.csv")
# 
# # step 5a - mode-time split by distance
# mts <- nts %>%
#   mutate(purpose = ifelse(trip_direction == 'hb_to', -p, ifelse(trip_direction == 'nhb', p - 10, p))) %>%
#   group_by(TripOrigGOR_B02ID, TripDestGOR_B02ID, main_mode, purpose, trip_direction, start_time, TripDisIncSW) %>%
#   summarise(freq = sum(freq, na.rm = TRUE), trips = sum(trips, na.rm = TRUE)) %>%
#   filter(main_mode %in% c(1,2,3,5,6,7))
# write_csv(mts,"D:/NTS/AgBM/nts_mode_time_split_distance.csv")

# output NTS data at UA level
# step 3 - aggregate to UA
nts <- nts %>%
  group_by(tfn_tt, tfn_at, tour_id, HHoldUA_B01ID, TripOrigUA_B01ID, TripDestUA_B01ID, main_mode, 
           purpose, p, trip_direction, start_time, TripDisIncSW) %>%
  summarise(freq = sum(freq * weighted_trips, na.rm = TRUE), 
            trips = sum(weighted_trips, na.rm = TRUE)) %>%
  filter(TripOrigUA_B01ID > 0, TripDestUA_B01ID > 0, 
         start_time %in% c(1:6), !is.na(purpose), !is.na(trip_direction))

# step 4 - produce activity database
act <- nts %>%
  group_by(HHoldUA_B01ID, tour_id) %>%
  summarise(freq = sum(freq, na.rm = TRUE), trips = sum(trips, na.rm = TRUE)) %>%
  filter(HHoldUA_B01ID > 0, endsWith(tour_id, "_0"))
write_csv(act,"D:/NTS/AgBM/nts_data/nts_activity.csv")

# step 5 - produce mode-time split
mts <- nts %>%
  mutate(purpose = ifelse(trip_direction == 'hb_to', -p, ifelse(trip_direction == 'nhb', p - 10, p))) %>%
  group_by(TripOrigUA_B01ID, TripDestUA_B01ID, main_mode, purpose, trip_direction, start_time) %>%
  summarise(freq = sum(freq, na.rm = TRUE), trips = sum(trips, na.rm = TRUE)) %>%
  filter(main_mode %in% c(1,2,3,5,6,7))
write_csv(mts,"D:/NTS/AgBM/nts_data/nts_mode_time_split.csv")

# step 5a - mode-time split by distance
mts <- nts %>%
  mutate(purpose = ifelse(trip_direction == 'hb_to', -p, ifelse(trip_direction == 'nhb', p - 10, p))) %>%
  group_by(TripOrigUA_B01ID, TripDestUA_B01ID, main_mode, purpose, trip_direction, start_time, TripDisIncSW) %>%
  summarise(freq = sum(freq, na.rm = TRUE), trips = sum(trips, na.rm = TRUE)) %>%
  filter(main_mode %in% c(1,2,3,5,6,7))
write_csv(mts,"D:/NTS/AgBM/nts_data/nts_mode_time_split_distance.csv")

# step 6 - calculate population
pop <- cb %>%
  group_by(HHoldGOR_B02ID, Settlement2011EW_B01ID, IndividualID) %>%
  summarise(pop = 1) %>%
  group_by(HHoldGOR_B02ID, Settlement2011EW_B01ID) %>%
  summarise(pop = sum(pop)) %>%
  arrange(HHoldGOR_B02ID, Settlement2011EW_B01ID)
write_csv(pop,"D:/NTS/AgBM/nts_population.csv")

# step 7 - extract hb trips
hbt <- cb %>%
  mutate(purpose = ifelse(trip_direction == 'hb_to', -p, ifelse(trip_direction == 'nhb', p - 10, p))) %>%
  filter(trip_direction %in% c("hb_fr")) %>%
  group_by(HHoldGOR_B02ID, Settlement2011EW_B01ID, purpose) %>%
  summarise(trips = sum(weighted_trips, na.rm = TRUE))
write_csv(hbt,"D:/NTS/AgBM/nts_hb_trips.csv")

# checking
out <- cb %>%
# filter(TripOrigGOR_B02ID %in% c(1), TripDestGOR_B02ID %in% c(3, 8), 
#        main_mode %in% c(1))
  filter(IndividualID %in% c("2004000645"))
 write_csv(out,"D:/NTS/AgBM/nts_check.csv")

### PREVIOUS METHOD ###
# temporal distribution
tmp <- cb %>%
  mutate(purpose = ifelse(trip_purpose > 10, trip_purpose - 10, trip_purpose)) %>%
  group_by(IndividualID, TripOrigGOR_B02ID, TripDestGOR_B02ID, purpose, trip_direction, start_time, end_time) %>%
  summarise(freq = 1, trips = sum(weighted_trips, na.rm = TRUE)) %>%
  filter(TripOrigGOR_B02ID %in% c(1:11), TripDestGOR_B02ID %in% c(1:11), 
         start_time %in% c(1:6), end_time %in% c(1:6),
         !is.na(purpose), !is.na(trip_direction))

tmp <- tmp %>%
  group_by(TripOrigGOR_B02ID, TripDestGOR_B02ID, purpose, trip_direction, start_time, end_time) %>%
  summarise(freq = sum(freq, na.rm = TRUE), trips = sum(trips, na.rm = TRUE))
write_csv(tmp,"D:/NTS/AgBM/nts_temporal.csv")

mts <- cb %>%
  mutate(purpose = ifelse(trip_purpose > 10, trip_purpose - 10, trip_purpose)) %>%
  group_by(IndividualID, TripOrigGOR_B02ID, TripDestGOR_B02ID, main_mode, purpose, trip_direction, start_time, end_time) %>%
  summarise(freq = 1, trips = sum(weighted_trips, na.rm = TRUE)) %>%
  filter(TripOrigGOR_B02ID %in% c(1:11), TripDestGOR_B02ID %in% c(1:11), 
         start_time %in% c(1:6), end_time %in% c(1:6),
         !is.na(purpose), !is.na(trip_direction), main_mode %in% c(1,2,3,5,6,7))

mts <- mts %>%
  group_by(TripOrigGOR_B02ID, TripDestGOR_B02ID, main_mode, purpose, trip_direction, start_time, end_time) %>%
  summarise(freq = sum(freq, na.rm = TRUE), trips = sum(trips, na.rm = TRUE))
write_csv(mts,"D:/NTS/AgBM/nts_mode_time_split.csv")

# group database to get activity
nts <- cb %>%
  group_by(IndividualID, HHoldGOR_B02ID, trip_group) %>%
  # escort embedded in main purpose
  mutate(purpose = ifelse(trip_purpose > 10, trip_purpose - 10, trip_purpose)) %>% 
  # mutate(purpose = trip_purpose) %>%
  mutate(purpose = ifelse(trip_direction == "hb_to", 0, purpose)) %>% 
  # summarise(tour = str_c("0_", paste0(rle(as.character(purpose))$values, collapse = "_")), 
  #          freq = 1, trips = sum(weighted_trips, na.rm = TRUE))
  summarise(tour = str_c("0_", paste0(purpose, collapse = "_")), 
                      freq = 1, trips = sum(weighted_trips, na.rm = TRUE))
            
act <- nts %>%
  group_by(HHoldGOR_B02ID, tour) %>%
  summarise(freq = sum(freq, na.rm = TRUE), trips = sum(trips, na.rm = TRUE)) %>%
  filter(HHoldGOR_B02ID %in% c(1:11), endsWith(tour, "_0"))

write_csv(act,"D:/NTS/AgBM/nts_activity.csv")

# get total from home trips by purpose
fr_home <- cb %>%
  mutate(purpose = ifelse(trip_purpose > 10, trip_purpose - 10, trip_purpose)) %>% 
  # mutate(purpose = trip_purpose) %>%
  group_by(TripOrigGOR_B02ID, purpose, trip_direction) %>%
  summarise(trips = sum(weighted_trips, na.rm = TRUE)) %>%
  filter(trip_direction == "hb_fr", TripOrigGOR_B02ID %in% c(1:11)) %>%
  select(!c("trip_direction"))
write_csv(fr_home,"D:/NTS/AgBM/nts_ted_hb_fr.csv")

# get NTS distribution
hb_fr <- cb %>%
  mutate(purpose = ifelse(trip_purpose > 10, trip_purpose - 10, trip_purpose)) %>% 
  # mutate(purpose = trip_purpose) %>%
  group_by(IndividualID, TripOrigGOR_B02ID, TripDestGOR_B02ID, purpose, trip_direction) %>%
  summarise(hb_fr = sum(weighted_trips, na.rm = TRUE)) %>%
  filter(trip_direction == "hb_fr", TripOrigGOR_B02ID %in% c(1:11), TripDestGOR_B02ID %in% c(1:11)) %>%
  select(!c("trip_direction"))

hb_to <- cb %>%
  mutate(purpose = ifelse(trip_purpose > 10, trip_purpose - 10, trip_purpose)) %>% 
  # mutate(pp = trip_purpose) %>%
  group_by(TripOrigGOR_B02ID, TripDestGOR_B02ID, purpose, trip_direction) %>%
  summarise(hb_to = sum(weighted_trips, na.rm = TRUE)) %>%
  filter(trip_direction == "hb_to", TripOrigGOR_B02ID %in% c(1:11), TripDestGOR_B02ID %in% c(1:11)) %>%
  select(!c("trip_direction"))

nhb <- cb %>%
  mutate(purpose = ifelse(trip_purpose > 10, trip_purpose - 10, trip_purpose)) %>% 
  # mutate(purpose = trip_purpose) %>%
  group_by(TripOrigGOR_B02ID, TripDestGOR_B02ID, purpose, trip_direction) %>%
  summarise(nhb = sum(weighted_trips, na.rm = TRUE)) %>%
  filter(trip_direction == "nhb", TripOrigGOR_B02ID %in% c(1:11), TripDestGOR_B02ID %in% c(1:11)) %>%
  select(!c("trip_direction"))

# merge database
out = merge(hb_fr, nhb, by= c("TripOrigGOR_B02ID", "TripDestGOR_B02ID", "purpose"), all = TRUE)
out = merge(out, hb_to, by= c("TripOrigGOR_B02ID", "TripDestGOR_B02ID", "purpose"), all = TRUE)
out[is.na(out)] <- 0
write_csv(out,"D:/NTS/AgBM/nts_mat_all.csv")


### OTHER TESTS ###
cb <- escort_trips(cb, option = 1) # embed escort to main purpose
cb <- data_filter(cb, hb_only = FALSE, remove_van = FALSE, remove_air = TRUE,
                  aggregate_rail = TRUE) #filter data

# get ntem_at
cb <- cb %>%
  mutate(purpose = ifelse(trip_direction == 'hb_to', -p, p))%>%
  mutate(ntem_at = case_when(
    HHoldAreaType2_B01ID == 1 ~ 1,
    HHoldAreaType2_B01ID == 2 ~ 2,
    HHoldAreaType2_B01ID %in% c(3,4,5,6,7,8,9) ~ 3,
    HHoldAreaType2_B01ID == 10 ~ 4,
    HHoldAreaType2_B01ID == 11 ~ 5,
    HHoldAreaType2_B01ID %in% c(12,13) ~ 6,
    HHoldAreaType2_B01ID %in% c(14,15) ~ 7,
    HHoldAreaType2_B01ID == 16 ~ 8))

tld <- cb %>%
  group_by(main_mode, purpose, HHoldGOR_B02ID, HHoldCounty_B01ID, TripOrigCounty_B01ID, TripOrigGOR_B02ID,
           ntem_at, trip_direction, start_time, TripDisIncSW) %>%
  summarise(trips = sum(weighted_trips, na.rm = TRUE),trip_mile = sum(weighted_trips*TripDisIncSW, na.rm = TRUE))
write_csv(tld,"D:/NTS/nts_tld_lincoln.csv")

# calculate tours
hb_fr <- cb %>%
  filter(trip_direction == "hb_fr") %>%
  select(HHoldGOR_B02ID, HHoldCounty_B01ID, ntem_at, IndividualID, TripID, DayID, main_mode, start_time, 
         trip_purpose, purpose, trip_group, W5xHH, W2, JJXSC, weighted_trips) %>%
  arrange(IndividualID, DayID, TripID)

hb_to <- cb %>%
  filter(trip_direction == "hb_to") %>%
  select(HHoldGOR_B02ID, HHoldCounty_B01ID, ntem_at, IndividualID, TripID, DayID, main_mode, start_time, 
         trip_purpose, purpose, trip_group, weighted_trips, W5xHH, W2, JJXSC) %>%
  arrange(IndividualID, DayID, TripID)  

tour <- hb_fr %>%
  left_join(hb_to, by = c("IndividualID","trip_group"), suffix = c("","_return")) %>%
  filter(purpose != 0, !is.na(start_time), !is.na(start_time_return))

# filter hb, remove Van & air and combine light & heavy rail
tour <- data_filter(tour, hb_only = TRUE, remove_van = TRUE, remove_air = TRUE, 
                    aggregate_rail = TRUE)

tour <- tour %>%
  group_by(HHoldGOR_B02ID, HHoldCounty_B01ID, ntem_at, main_mode, purpose, start_time, start_time_return) %>%
  summarise(trips = sum(weighted_trips, na.rm = TRUE)) %>% #, trips_to = sum(weighted_trips_return)) %>%
  rename(purpose = purpose, time_outward = start_time, time_return = start_time_return)
write_csv(tour,"D:/NTS/nts_tour_lincoln.csv")

# output tld w/o tt, soc, sec
 tld <- cb %>%
   group_by(main_mode, p, ntem_at, start_time, TripOrigGOR_B02ID, TripDestGOR_B02ID, 
            TripOrigCounty_B01ID, TripDestCounty_B01ID, #TripOrigUA2009_B01ID, TripDestUA2009_B01ID,
            trip_direction, occupant, TripDisIncSW) %>%
   summarise(trips = sum(weighted_trips)) %>%
   mutate(trip_mile = TripDisIncSW * trips) %>%
   filter(!p %in% c(0,10))
 
 tld <- tld %>% #transpose o/d for _th direction
   mutate(or = ifelse(trip_direction=="hb_to", TripDestGOR_B02ID, TripOrigGOR_B02ID),
          dr = ifelse(trip_direction=="hb_to", TripOrigGOR_B02ID, TripDestGOR_B02ID),
          oc = ifelse(trip_direction=="hb_to", TripDestCounty_B01ID, TripOrigCounty_B01ID),
          dc = ifelse(trip_direction=="hb_to", TripOrigCounty_B01ID, TripDestCounty_B01ID)) %>%
   mutate(TripOrigGOR_B02ID = or, TripDestGOR_B02ID = dr, 
          TripOrigCounty_B01ID = oc, TripDestCounty_B01ID = dc) %>%
   select(!c("or","dr","oc","dc")) %>%
   filter(!is.na(p))
 
 write_csv(tld,"D:/NTS/nts_tld_robR.csv")

 # check series of call
 sc <- cb %>%
   filter(TripOrigGOR_B02ID %in% c(2) | TripDestGOR_B02ID %in% c(2)) %>%   
   group_by(main_mode,start_time, trip_purpose,trip_direction) %>%
   summarise(trip = sum(weighted_trips, na.rm = TRUE))
 
# check area type
at <- cb %>%
  mutate(ntem_at = case_when(
    HHoldAreaType2_B01ID == 1 ~ 1,
    HHoldAreaType2_B01ID == 2 ~ 2,
    HHoldAreaType2_B01ID %in% c(3,4,5,6,7,8,9) ~ 3,
    HHoldAreaType2_B01ID == 10 ~ 4,
    HHoldAreaType2_B01ID == 11 ~ 5,
    HHoldAreaType2_B01ID %in% c(12,13) ~ 6,
    HHoldAreaType2_B01ID %in% c(14,15) ~ 7,
    HHoldAreaType2_B01ID == 16 ~ 8)) %>%
  group_by(ntem_at, HouseholdID) %>%
  summarise(count = 1)

at <- at %>%
  group_by(ntem_at) %>%
  summarise(count=sum(count))

write_csv(at,"D:/NTS/nts_at_tem.csv")

#area type by counties
at <- cb %>%
  mutate(ntem_at = case_when(
    HHoldAreaType2_B01ID == 1 ~ 1,
    HHoldAreaType2_B01ID == 2 ~ 2,
    HHoldAreaType2_B01ID %in% c(3,4,5,6,7,8,9) ~ 3,
    HHoldAreaType2_B01ID == 10 ~ 4,
    HHoldAreaType2_B01ID == 11 ~ 5,
    HHoldAreaType2_B01ID %in% c(12,13) ~ 6,
    HHoldAreaType2_B01ID %in% c(14,15) ~ 7,
    HHoldAreaType2_B01ID == 16 ~ 8)) %>%
  group_by(ntem_at, HHoldGOR_B02ID, HHoldCounty_B01ID, HouseholdID) %>%
  summarise(count = 1)

at <- at %>%
  group_by(ntem_at, HHoldGOR_B02ID, HHoldCounty_B01ID) %>%
  summarise(count=sum(count))

write_csv(at,"D:/NTS/nts_at_tem.csv")

#population density
pd <- cb %>%
  group_by(PSUPopDensity, HHoldGOR_B02ID, HHoldCounty_B01ID, HouseholdID) %>%
  summarise(count=1)

pd <- pd %>%
  group_by(PSUPopDensity, HHoldGOR_B02ID, HHoldCounty_B01ID) %>%
  summarise(count=sum(count))

write_csv(pd,"D:/NTS/nts_pd_tem.csv")

# review light/heavy rail split by LA
rail <- cb %>%
  group_by(HHoldOSLAUA_B01ID, main_mode, p, trip_direction) %>%
  mutate(p = ifelse(p < 0, -p, p)) %>%
  summarise(trips = sum(weighted_trips, na.rm = TRUE)) %>%
  filter(main_mode %in% c(6,7))

write_csv(rail,"D:/NTS/nts_rail_v3.csv")

# review HHold data
test1 <- cb %>%
  group_by(HHoldAreaType1_B01ID, HHoldAreaType2_B01ID, tfn_at, PSUAreaType2_B01ID, PSUPSect, HHoldOSWard_B01ID) %>%
  summarise(trips = sum(weighted_trips))
write_csv(test1,"D:/NTS/HHold_data_analysis.csv")

test2 <- cb %>%
  #filter(trip_purpose==0) %>%
  #filter(is.na(trip_purpose)) %>%
  #filter(is.na(trip_direction)) %>%
  filter(IndividualID==2002000034) %>%
  select(IndividualID, TripID, main_mode, TripPurpFrom_B01ID, TripPurpTo_B01ID, trip_purpose, trip_direction, weighted_trips) %>%
  arrange(IndividualID, TripID)

test <- cb %>%
  count(SurveyYear)

test <- ub %>%
  #filter(trip_purpose==0) %>%
  #filter(is.na(trip_purpose)) %>%
  #filter(is.na(trip_direction)) %>%
  filter(IndividualID==2002000034) %>%
  select(IndividualID, TripID, MainMode_B11ID, TripPurpFrom_B01ID, TripPurpTo_B01ID, W5) %>%
  arrange(IndividualID, TripID)

#write_csv(test,"D:/NTS/nts_tld_summary.csv")

# test
check <- cb %>%
  group_by(main_mode, MainMode_B11ID, occupant) %>%
  summarise(trip = sum(weighted_trips))

write_csv(check,"D:/NTS/nts_occ_summary.csv")

# aggregate to distance band
# 0,1,2,3,4,5,6,7,8,9,10,12.5,15,17.5,20,25,30,35,40,50,75,100,150,200,250,300,400,600,1999
#0,2,5,10,20,50,100,200,500,1999
dist_band = c(0,1,2,3,4,5,6,7,8,9,10,12.5,15,17.5,20,25,30,35,40,50,75,100,150,200,250,300,400,600,1999)
#               1,2,3,4,5,6,7,8,9,10,  11,12,  13,14,15,16,17,18,19,20, 21, 22, 23, 24, 25, 26, 27,  28
tld <- cb %>% 
  mutate(o2 = ifelse(trip_direction=="hb_to", TripDestGOR_B02ID, TripOrigGOR_B02ID)) %>%
  mutate(d2 = ifelse(trip_direction=="hb_to", TripOrigGOR_B02ID, TripDestGOR_B02ID)) %>%
  mutate(TripOrigGOR_B02ID = o2, TripDestGOR_B02ID = d2) %>%
  select(!c("o2","d2"))

tld <- tld %>%
  mutate(bands = cut(TripDisIncSW, breaks = dist_band,
                     right = FALSE, labels = FALSE, ordered_result = TRUE)) %>%
  mutate(bands = factor(bands)) %>%
  group_by(main_mode, p, start_time, TripOrigGOR_B02ID, bands) %>%
  summarise(trips = sum(weighted_trips, na.rm = TRUE),trip_mile = sum(weighted_trips*TripDisIncSW*1.6093, na.rm = TRUE)) %>%
  mutate(trip_mile = trip_mile/trips) %>%
  filter(p %in% c(1:18), start_time %in% c(1:6), main_mode %in% c(3), TripOrigGOR_B02ID %in% c(1:11))

write_csv(tld,"D:/NTS/nts_tld_v3_agg.csv")

# test tld by soc, sec, gender
nts <- cb %>%
  mutate(bands = cut(TripDisIncSW, breaks = dist_band,
                     right = FALSE, labels = FALSE, ordered_result = TRUE)) %>%
  mutate(bands = factor(bands)) %>%  
  group_by(main_mode, p, soc, ns, gender, start_time, trip_direction, occupant, bands) %>%
  summarise(trips = sum(weighted_trips, na.rm = TRUE), trip_mile = sum(weighted_trips*TripDisIncSW, na.rm = TRUE)) %>%
  filter(!p %in% c(0,10), !is.na(soc), !is.na(p), !is.na(start_time), !is.na(trip_direction))

nts <- nts %>%
  complete(occupant = c("driver","passenger"), bands = c(1:28),
           fill = list(trips = 0, trip_mile = 0), explicit = FALSE)

write_csv(nts,"D:/NTS/nts_tld_v3_agg.csv")

# Vehicle Occupancy 
library(tidyverse)

co <- read_csv("C:/Users/Pluto/Documents/NTS_C/nts_tld_all_vars_car_occupancy.csv")

co %>%
  count(trip_purpose)

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
  mutate(bands = cut(TripDisIncSW, c(0,1,2,3,4,5,6,7,8,9,10,12.5,15,17.5,20,25,30,35,40,50,75,100,150,200,250,300,400,600,1999), 
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
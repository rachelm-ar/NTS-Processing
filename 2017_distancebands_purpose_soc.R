### Processing NTS data to obtain trips by purpose, soc, mode and distance


### Load libraries and import data ##############################################
library("tidyverse")
library("data.table")

# Import unclassified build
unclassified_build <- read_csv("Y:/NTS/tfn_unclassified_build.csv")

### Data processing #############################################################

# 2017 only
# North (TfN area) only
# HB and NHB, dropping return legs as no purpose assigned
# distance bands 0-2km	2-5km	5-10km	10-20km	20-50km	50km+
# modes: car, rail, bus, walk, cycle
# remove unclassified trip purpose, mode, SOC and distance


# Classify trip origin to get HB/NHB splits - only needed if disctinction bewteen HB/NHB
hb <- c(23)
nhb <- c(1,2,3,4,5,6,7,8,9,
         10,11,12,13,14,15,
         16,17,18,19,20,21,22)

unclassified_build <- unclassified_build %>%
  mutate(trip_origin = case_when(
    TripPurpFrom_B01ID %in% hb ~ 'hb',
    TripPurpFrom_B01ID %in% nhb ~ 'nhb',
    TRUE ~ as.character(NA)
  ))

# Recode trip purposes as NTEM classification hb_purpose for HB trips
unclassified_build <- unclassified_build %>%
  mutate(trip_purpose = case_when(
    TripPurpTo_B01ID == 1 ~ '1', # Work
    TripPurpTo_B01ID == 2 ~ '2', # In course of work
    TripPurpTo_B01ID == 3 ~ '3', # Education
    TripPurpTo_B01ID == 4 ~ '3', # Food shopping
    TripPurpTo_B01ID == 5 ~ '3', # Non food shopping
    TripPurpTo_B01ID == 6 ~ '3', # Personal business medical
    TripPurpTo_B01ID == 7 ~ '3', # Personal business eat / drink
    TripPurpTo_B01ID == 8 ~ '3', # Personal business other
    TripPurpTo_B01ID == 9 ~ '3', # Eat / drink with friends
    TripPurpTo_B01ID == 10 ~ '3', # Visit friends
    TripPurpTo_B01ID == 11 ~ '3', # Other social
    TripPurpTo_B01ID == 12 ~ '3', # Entertain /  public activity
    TripPurpTo_B01ID == 13 ~ '3', # Sport: participate
    TripPurpTo_B01ID == 14 ~ '3', # Holiday: base
    TripPurpTo_B01ID == 15 ~ '3', # Day trip / just walk
    TripPurpTo_B01ID == 16 ~ '3', # Other non-escort
    TripPurpTo_B01ID == 17 ~ '99', # Escort home
    TripPurpTo_B01ID == 18 ~ '1', # Escort work
    TripPurpTo_B01ID == 19 ~ '2', # Escort in course of work
    TripPurpTo_B01ID == 20 ~ '3', # Escort education
    TripPurpTo_B01ID == 21 ~ '3', # Escort shopping / personal business
    TripPurpTo_B01ID == 22 ~ '3', # Other escort
    TripPurpTo_B01ID == 23 ~ '99', # Home
    TRUE ~ as.character('unclassified')
    # Drop -99 (return legs with no assigned trip purpose) and unclassified (no trip purpose)
  )) %>% filter (trip_purpose != '99' & trip_purpose != 'unclassified')

## Pick a final purpose depending on purpose from
#unclassified_build <- unclassified_build %>%
#  mutate(trip_purpose = case_when(
#    trip_origin == 'hb' ~ hb_purpose,
#    trip_origin == 'nhb' ~ nhb_purpose,
#    TRUE ~ as.character('unclassified')
#  ))

# Convert SOC Types(XSOC2000_B02ID) as soc_stat
unclassified_build <- unclassified_build %>%
  mutate(soc_cat = case_when(
    XSOC2000_B02ID == 1 ~ '1', # 1	Managers and senior officials
    XSOC2000_B02ID == 2 ~ '1', # 2	Professional occupations
    XSOC2000_B02ID == 3 ~ '1', # 3	Associate professional and technical occupations
    XSOC2000_B02ID == 4 ~ '2', # 4	Administrative and secretarial occupations
    XSOC2000_B02ID == 5 ~ '2', # 5	Skilled trades occupations
    XSOC2000_B02ID == 6 ~ '2', # 6	Personal service occupations
    XSOC2000_B02ID == 7 ~ '2', # 7	Sales and customer service occupations
    XSOC2000_B02ID == 8 ~ '3', # 8	Process, plant and machine operatives
    XSOC2000_B02ID == 9 ~ '3', # 9	Elementary occupations
    TRUE ~ '-9'
    # Drop soc_cat -9 (missing category)
  )) %>% filter (soc_cat != '-9')


# Filter for 2017 NTS data only
unclassified_build <- unclassified_build %>%
  filter(SurveyYear == 2017)

# Filter for Households within the North (TfN area)
northern_la <- c("E06000001",	"E06000002",	"E06000003",	"E06000004",	"E06000005",	"E06000006",	"E06000007",	"E06000008",
                 "E06000009",	"E06000010",	"E06000011",	"E06000012",	"E06000013",	"E06000014",	"E06000047",	"E06000048",
                 "E06000049",	"E06000050",	"E07000026",	"E07000027",	"E07000028",	"E07000029",	"E07000030",	"E07000031",
                 "E07000117",	"E07000118",	"E07000119",	"E07000120",	"E07000121",	"E07000122",	"E07000123",	"E07000124",
                 "E07000125",	"E07000126",	"E07000127",	"E07000128",	"E07000163",	"E07000164",	"E07000165",	"E07000166",
                 "E07000167",	"E07000168",	"E07000169",	"E08000001",	"E08000002",	"E08000003",	"E08000004",	"E08000005",
                 "E08000006",	"E08000007",	"E08000008",	"E08000009",	"E08000010",	"E08000011",	"E08000012",	"E08000013",
                 "E08000014",	"E08000015",	"E08000016",	"E08000017",	"E08000018",	"E08000019",	"E08000020",	"E08000021",
                 "E08000022",	"E08000023",	"E08000024",	"E08000032",	"E08000033",	"E08000034",	"E08000035",	"E08000036",
                 "E08000037")

unclassified_build <- unclassified_build %>%
  filter(HHoldOSLAUA_B01ID %in% northern_la)


# Modes: car, rail, bus, walk, cycle
#recode main mode(MainMode_B04ID) as main_mode
unclassified_build <- unclassified_build %>%
  mutate(main_mode = case_when(
    MainMode_B04ID == 1 ~ '1', # Walk
    MainMode_B04ID == 2 ~ '2', # Bicycle
    MainMode_B04ID == 3 ~ '3', # Car/van driver
    MainMode_B04ID == 4 ~ '3', # Car/van passenger
    MainMode_B04ID == 5 ~ '3', # Motorcycle
    MainMode_B04ID == 6 ~ '3', # Other private transport
    MainMode_B04ID == 7 ~ '5', # Bus in London
    MainMode_B04ID == 8 ~ '5', # Other local bus
    MainMode_B04ID == 9 ~ '5', # Non-local bus
    MainMode_B04ID == 10 ~ '99', # London Underground - leave for now
    MainMode_B04ID == 11 ~ '6', # Surface rail
    MainMode_B04ID == 12 ~ '3', # Taxi/minicab ie. a car
    MainMode_B04ID == 13 ~ '5', # Other public transport ie. small bus or light rail.
    TRUE ~ 'unclassified'
    # Drop unclassified mode and 99 (underground), keep underground as train/bus?
  )) %>% filter(main_mode %in% c('1','2','3','5','6'))
   
# distance bands 0-2km	2-5km	5-10km	10-20km	20-50km	50km+
# Includes short walks
# NAs make up ~ 10% of all entries for trip distance
# 1 FALSE                                    4654627
# 2 TRUE                                       47370

# Convert distance in miles into km and classify into distance bands
unclassified_build <- unclassified_build %>%
  mutate(distance = TripDisIncSW/1.609344) %>%
  mutate(distance = case_when(
    distance > 0 & distance <= 2 ~ '0-2km', # 0-2km
    distance > 2 & distance <= 5 ~ '2-5km', # 2-5km
    distance > 5 & distance <= 10 ~ '5-10km', # 5-10km
    distance > 10 & distance <= 20 ~ '10-20km', # 10-20km
    distance > 20 & distance <= 50 ~ '20-50km', #20-50km
    distance >50 ~ '50km+', #50km+
    TRUE ~ 'unclassified'
    # Drop unclassified mode, 99 for underground should be filtered by northern LA
  )) %>% filter(main_mode != 'unclassified')

# Mutate trip_purpose, soc_cat, main_mode and distance into factors
facts <- c('trip_purpose', 'main_mode', 'soc_cat', 'distance')
unclassified_build <- unclassified_build %>% mutate_at(facts, funs(factor))
unclassified_build$distance <- factor(unclassified_build$distance, levels = c('0-2km','2-5km','5-10km', '10-20km', '20-50km', '50km+'))
unclassified_build$main_mode <- factor(unclassified_build$main_mode, labels = c('walking', 'cycling', 'car', 'bus', 'rail'))
unclassified_build$trip_purpose <- factor(unclassified_build$trip_purpose, labels = c('commuting', 'business', 'other'))

# Re-weight short-walk trips by a factor of 7
walk_trips <- unclassified_build %>%
  select(IndividualID, trip_purpose, W5xHh, main_mode, TripDisIncSW) %>%
  filter(TripDisIncSW < 1.609344, main_mode == "walking" & trip_purpose != 8) %>%
  mutate(W5xHh = 7)

# Replace updated short walk trips with new trip weights
setDT(unclassified_build)             
setDT(walk_trips) 

unclassified_build[walk_trips, on = c("IndividualID", "trip_purpose", "main_mode", "TripDisIncSW"), W5xHh := i.W5xHh]
unclassified_build <- as_tibble(unclassified_build)

#### Produce tables by purpose/mode/distance and soc/mode/distance##############################

##### HUMZA

# We multiply the weights, aggregate for each individual, divide by their individual weight and then aggregate further for all individuals

# Purpose/mode/distance
pmd_df <- unclassified_build %>%
  mutate(weighted_trips = W1 * W2 * W5xHh) %>%
  select(IndividualID, trip_purpose, main_mode, distance, weighted_trips, W2) %>%
  group_by(IndividualID, trip_purpose, main_mode, distance, W2) %>%
  summarise(weighted_trips = sum(weighted_trips, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(trip_rate = weighted_trips/W2) %>%
  group_by(trip_purpose, main_mode, distance, .drop = FALSE) %>%
  summarise(trip_rate = sum(trip_rate, na.rm = TRUE)) %>%
  write_csv("Y:/NTS/2017 trips by purpose soc mode distance/2017_purpose_mode_dist.csv")

# SOC/mode/distance
smd_df <- unclassified_build %>%
  mutate(weighted_trips = W1 * W2 * W5xHh) %>%
  select(IndividualID, soc_cat, main_mode, distance, weighted_trips, W2) %>%
  group_by(IndividualID, soc_cat, main_mode, distance, W2) %>%
  summarise(weighted_trips = sum(weighted_trips, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(trip_rate = weighted_trips/W2) %>%
  group_by(soc_cat, main_mode, distance, .drop = FALSE) %>%
  summarise(trip_rate = sum(trip_rate, na.rm = TRUE)) %>%
  write_csv("Y:/NTS/2017 trips by purpose soc mode distance/2017_soc_mode_dist.csv")

# Mode/purpose/soc/distance
mpsd_df <- unclassified_build %>%
  mutate(weighted_trips = W1 * W2 * W5xHh) %>%
  select(IndividualID, soc_cat, main_mode, trip_purpose, distance, weighted_trips, W2) %>%
  group_by(IndividualID, main_mode, trip_purpose, soc_cat, distance, W2) %>%
  summarise(weighted_trips = sum(weighted_trips, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(trip_rate = weighted_trips/W2) %>%
  group_by(main_mode, trip_purpose, soc_cat, distance, .drop = FALSE) %>%
  summarise(trip_rate = sum(trip_rate, na.rm = TRUE)) %>%
  write_csv("Y:/NTS/2017 trips by purpose soc mode distance/2017_mode_purpose_soc_dist.csv")


#### MARTIN ####

## Frequency table of purpose, mode, distance
#pmd_df <- unclassified_build %>% 
#  mutate(weighted_trips = W1 * W2 *W5xHh) %>%
#  select(trip_purpose, main_mode, distance, weighted_trips) %>%
#  group_by(trip_purpose, main_mode, distance, .drop = FALSE) %>%
#  summarise(trips = sum(weighted_trips,na.rm = TRUE))
#  write.csv("2017_purpose_mode_distance_weighted.csv")
#  
## Frequency table of purpose, mode, distance
#smd_df <- unclassified_build %>% 
#  mutate(weighted_trips = W1 * W2 *W5xHh) %>%
#  select(soc_cat, main_mode, distance, weighted_trips) %>%
#  group_by(soc_cat, main_mode, distance, .drop = FALSE) %>%
#  summarise(trips = sum(weighted_trips, na.rm = TRUE)) %>%
#  write.csv("2017_soc_mode_distance_weighted.csv")


#######################

# caveat: uses main_mode, i.e. discards any secondary mode(s) in multimodal journeys
# this probably reduces walking/cycling trips?

# includes both HB and NHB at the moment, if only HB: replace trip_purpose by hb_purpose in group_bys


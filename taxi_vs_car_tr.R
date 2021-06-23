
require(tidyverse)

# Get unclassified dataset
uc <- read_csv('C:/Users/genie/Documents/NTS_C/unclassified builds/ub_tfn.csv')

# Filter down to year, skill, LA, WFH%, person weight,
car_taxi <- uc %>%
  select("SurveyYear", "HHoldAreaType1_B01ID", "IndividualID", "TripID", "TravelWeekDay_B01ID",  "MainMode_B11ID", "TripOrigUA2009_B01ID", "W5") %>%
  filter(TravelWeekDay_B01ID %in% c(1,2,3,4,5)) %>%
  select(-"TravelWeekDay_B01ID") %>%
  distinct()

car_taxi_trips <- car_taxi %>%
  group_by(MainMode_B11ID, HHoldAreaType1_B01ID) %>%
  summarise(trips = sum(W5, na.rm=TRUE)) %>%
  ungroup()

car_taxi_people <- car_taxi %>%
  select(IndividualID, HHoldAreaType1_B01ID) %>%
  group_by(HHoldAreaType1_B01ID) %>%
  distinct() %>%
  summarise(people = n())

car_taxi_trips <- car_taxi_trips %>%
  left_join(car_taxi_people)

car_taxi_trips %>%
  write_csv('C:/Users/genie/Documents/NTS_C/mode_rates_at.csv')

# Count & apply weights

# Get base mix by year
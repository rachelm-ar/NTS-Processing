
require(tidyverse)

# Get unclassified dataset
uc <- read_csv('C:/Users/genie/Documents/NTS_C/unclassified builds/ub_tfn.csv')

# Filter down to year, skill, LA, WFH%, person weight,
wfh_uc <- uc %>%
  select("SurveyYear", "TravelWeekDay_B01ID", "IndividualID", "XSOC2000_B02ID","TripOrigUA2009_B01ID", "OftHome_B01ID", "W3") %>%
  filter(TravelWeekDay_B01ID %in% c(1,2,3,4,5)) %>%
  select(-"TravelWeekDay_B01ID") %>%
  distinct()

wfh_uc %>%
  write_csv('C:/Users/genie/Documents/NTS_C/trips.csv')

# Count & apply weights

# Get base mix by year
# Initialisation ----------------------------------------------------------

user <- Sys.info()[[6]]
repo_dir <- paste0("C:/Users/", user, "/Documents/GitHub/NTS-Processing/")

# Load custom functions
source(paste0(repo_dir, "utils.R"))
source(paste0(repo_dir, "lookups.R"))

cb <- read_csv("C:/Users/Pluto/Documents/NTS_C/classified builds/cb_pr_freight.csv")

# Preprocess --------------------------------------------------------------

# Lookups

mm_df <- cb %>%
  lu_stage_mode() %>%
  lu_is_north()

# Remove short walk stages
mm_df <- mm_df %>%
  filter(StageShortWalk_B01ID != 1)

# Filter for HB and NHB trip purposes
mm_df <- mm_df %>%
  filter(trip_purpose %in% 1:18) %>%
  mutate(trip_purpose = case_when(
    trip_purpose == 1 ~ "commute",
    trip_purpose %in% c(2, 12) ~ "business",
    trip_purpose %in% c(3:8, 13:16, 18) ~ "other"
  ))

# Zonal (north/non-north)
mm_df <- mm_df %>%
  mutate(area = ifelse(is_north == "yes", "north", "non-north")) %>%
  select(-HHoldOSLAUA_B01ID)

# modes
mm_df <- mm_df %>%
  filter(main_mode != 7, stage_mode != 7) %>%
  mutate(main_mode = case_when(
    main_mode == 1 ~ "walk",
    main_mode == 2 ~ "cycle",
    main_mode == 3 ~ "car",
    main_mode == 4 ~ "light_rail",
    main_mode == 5 ~ "bus",
    main_mode == 6 ~ "rail"
  )) %>%
  mutate(stage_mode = case_when(
    stage_mode == 1 ~ "walk",
    stage_mode == 2 ~ "cycle",
    stage_mode == 3 ~ "car",
    stage_mode == 4 ~ "light_rail",
    stage_mode == 5 ~ "bus",
    stage_mode == 6 ~ "rail"
  ))

# gender
mm_df <- mm_df %>%
  mutate(gender = case_when(
    gender == 1 ~ "child",
    gender == 2 ~ "male",
    gender == 3 ~ "female"
  ))

# Methodology -------------------------------------------------------------

# Aggregate distances of stages with 2 or more stages of same mode
mm_df <- mm_df %>%
  select(HouseholdID, IndividualID, TripID, StageID, NumStages, main_mode, stage_mode, StageDistance,
         TripDisIncSW, StageShortWalk_B01ID, soc, gender, trip_purpose, area) %>%
  group_by(HouseholdID, IndividualID, TripID, main_mode, stage_mode, soc, gender, trip_purpose, area) %>%
  summarise(StageDistance = sum(StageDistance)) %>%
  ungroup()

# Recount number of stages per trip and calculate new multi stage trips
mm_df <- mm_df %>%
  group_by(TripID) %>%
  mutate(stage_count = n()) %>%
  ungroup()

# Filter for multi stage trips
mm_df <- mm_df %>%
  filter(stage_count > 1)
  
# Find main and second mode based on distance  
mm_df <- mm_df %>%
  filter(main_mode != stage_mode) %>%
  group_by(TripID) %>%
  slice_max(StageDistance) %>%
  slice(1) %>%
  ungroup()

# Output ------------------------------------------------------------------

mm_df %>%
  select(main_mode, stage_mode, area, trip_purpose, gender, soc) %>%
  rename(secondary_mode = stage_mode) %>%
  group_by_all() %>%
  count() %>%
  rename(count = n) %>%
  write_csv("Y:/NTS/outputs/multi-modal output.csv")
  
  

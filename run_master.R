user <- Sys.info()[[6]]
repo_dir <- paste0("C:/Users/", user, "/Documents/GitHub/NTS-Processing/")

# Load custom functions
source(paste0(repo_dir, "utils.R"))
source(paste0(repo_dir, "lookups.R"))
source(paste0(repo_dir, "extraction_script_trip_rates.R"))
source(paste0(repo_dir, "unclassified_build_processing.R"))
source(paste0(repo_dir, "build_nhb_trip_rates.R"))
source(paste0(repo_dir, "hb_time_split.R"))
source(paste0(repo_dir, "hb_mode_split.R"))
source(paste0(repo_dir, "hb_mode_time_split.R"))

# Extraction Script -------------------------------------------------------
"
Only C drive support - read/write to C drive

Have a folder called NTS_C in documents with specialise licence inside

"

create_ub(extract_version = "ntem", 
          drive = "C")

# Classified build --------------------------------------------------------

"
ub_name: CSV name of unclassified build

cb_columns_name: CSV name of vars to select in ub

build_type: hb_trip_rates OR ca

drive: Which drive?

out_cb_name: name of classified_build out

save_processed: TRUE - save classified build

"
build_types <- c('hb_trip_rates', 'car_ownership')

ub_version = "tfn"
cb_version = "tfn"
lookups_version = "tfn"
drive = "C"
save_processed = TRUE

create_cb(user = user,
          ub_version = "tfn",
          cb_version = "ntem",
          build_type = "hb_trip_rates",
          drive = "C",
          save_processed = TRUE)

# HB Time Split -----------------------------------------------------------

extract_hb_ts(cb_version = "tfn",
              drive = "C",
              user = user,
              weekday = TRUE,
              week = TRUE)

# HB Mode Split -----------------------------------------------------------

extract_hb_ms(cb_name = "classified_build",
              drive = "C",
              user = user,
              weekday = TRUE,
              week = TRUE)

# HB Time Mode Split ------------------------------------------------------

extract_hb_mts(cb_name = "classified_build",
               drive = "C", 
               user = user)

# NHB ---------------------------------------------------------------------

# trip_rate = TRUE/FALSE - do you want to calculate new trip rates?
# time_split = TRUE/FALSE - do you want to calculate new time splits?

extract_nhb(drive = "Y",
            user = user,
            trip_rate = TRUE,
            time_split = TRUE)

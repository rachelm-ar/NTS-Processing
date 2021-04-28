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

# import_dir: special license directory
# export_dir: directory to export 
# extract_version: what is this export for? name must be consistent with extraction_cols_'name'.csv
# extract_name: name of output csv
# tsy: filter for travel survey year, remove if no filter

extract_raw_nts(import_dir = "C:/Users/Pluto/Documents/NTS_C/UKDA-7553-tab/tab/",
                export_dir = "C:/Users/Pluto/Documents/NTS_C/unclassified builds/",
                extract_version = "tfn",
                extract_name = "unclassified_build_tfn",
                tsy = 2015:2019)

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

classify_nts(user = user,
             ub_name = "unclassified_build_tfn",
             cb_columns_name = "classified_build_vars_tfn",
             build_type = "hb_trip_rates",
             drive = "C",
             out_cb_name = "classified_build_tfn",
             save_processed = FALSE)

# HB Time Split -----------------------------------------------------------

extract_hb_ts(cb_name = "classified_build",
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

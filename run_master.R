user <- Sys.info()[[6]]
repo_dir <- paste0("C:/Users/", user, "/Documents/GitHub/NTS-Processing/")

# Load custom functions
source(paste0(repo_dir, "utils.R"))
source(paste0(repo_dir, "extraction_script_trip_rates.R"))
source(paste0(repo_dir, "unclassified_build_processing.R"))
source(paste0(repo_dir, "lookups.R"))
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

extract_raw_nts(import_dir = "C:/Users/HumzaAli/Documents/NTS/UKDA-7553-tab/tab/",
                export_dir = "C:/Users/HumzaAli/Documents/NTS/",
                extract_version = "tfn",
                extract_name = "unclassified_build_tfn",
                tsy = 2015:2019)

# Classified build --------------------------------------------------------

# If you want to save time with reading:
#   custom_import: directory in c drive to a folder which has
#                   1. unclassified_build.csv
#                   2. classified_build_vars.csv
# If you want to save time with writing:
#   custom_export: directory in c drive to save builds

classify_nts(unclassified_build_v = "unclassified_build_tfn.csv",
             build_type = "hb_trip_rates",
             save_processed = TRUE,
             custom_import = "C:/Users/HumzaAli/Documents/NTS/",
             custom_export = "C:/Users/HumzaAli/Documents/NTS/")

# HB Time Split -----------------------------------------------------------

extract_hb_ts(drive = "Y",
              user = user,
              weekday = TRUE,
              week = TRUE)


# HB Mode Split -----------------------------------------------------------

extract_hb_ms(drive = "Y",
              user = user,
              weekday = TRUE,
              week = TRUE)


# HB Time Mode Split ------------------------------------------------------

extract_hb_mts(drive = "Y", 
               user = user)

# NHB ---------------------------------------------------------------------

# trip_rate = TRUE/FALSE - do you want to calculate new trip rates?
# time_split = TRUE/FALSE - do you want to calculate new time splits?

extract_nhb(drive = "Y",
            user = user,
            trip_rate = TRUE,
            time_split = TRUE)

user <- Sys.info()[[6]]
repo_dir <- paste0("C:/Users/", user, "/Documents/GitHub/NTS-Processing/")

# Load custom functions
source(paste0(repo_dir, "utils/utils.R"))
source(paste0(repo_dir, "utils/lookups.R"))
source(paste0(repo_dir, "main/build_ub.R"))
source(paste0(repo_dir, "main/build_cb.R"))
source(paste0(repo_dir, "main/build_hb_trip_rates.R"))
source(paste0(repo_dir, "main/build_hb_mts.R"))
source(paste0(repo_dir, "main/build_hb_productions.R"))

# Extraction Script -------------------------------------------------------
"
Only C drive support - read/write to C drive

Have a folder called NTS_C in documents with specialise licence inside

"

build_ub(extract_version = "tfn", 
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

build_cb(user = user,
         drive = "C",
         version_in = "tfn",
         version_out = "tfn",
         build_type = "hb_trip_rates",
         save_processed = TRUE)


# HB Trip Rates -----------------------------------------------------------

drive = "C"
tfn_or_ntem = "tfn"

build_hb_trip_rates(user = user,
                    drive = "C",
                    tfn_or_ntem = "tfn")

build_hb_trip_rates(user = user,
                    drive = "C",
                    tfn_or_ntem = "ntem")

# HB Time Mode Split ------------------------------------------------------

build_hb_mts(user = user,
             drive = "C",
             tfn_or_ntem = "tfn")
build_hb_mts(user = user,
             drive = "C",
             tfn_or_ntem = "ntem")

# HB Productions ---------------------------------------------------------------------

build_hb_productions(user = user,
                     drive = "C")


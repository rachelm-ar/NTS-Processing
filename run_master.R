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

# Unclassified build ------------------------------------------------------

build_ub(ub_input_csv_dir = "I:/NTS/import/ub_input.csv")

# Classified build --------------------------------------------------------

build_cb(cb_input_csv_dir = "I:/NTS/import/cb_input.csv",
         tfn_or_ntem = "tfn",
         save_processed = TRUE,
         extract_hb_trip_rates_inputs = TRUE)

# HB Trip Rates -----------------------------------------------------------

build_hb_trip_rates(hb_tr_input_csv_dir = "I:/NTS/import/hb_tr_input.csv",
                    tfn_or_ntem = "tfn")

# HB Time Mode Split ------------------------------------------------------

build_hb_mts(user = user,
             drive = "C",
             tfn_or_ntem = "tfn",
             seg_max = 300)

# HB Productions ---------------------------------------------------------------------

build_hb_productions(user = user,
                     drive = "C")

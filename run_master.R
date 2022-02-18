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
source(paste0(repo_dir, "main/build_nhb_outputs.R"))

# Unclassified build ------------------------------------------------------

build_ub(input_csv = "I:/NTS/import/ub_input.csv")

# Classified build --------------------------------------------------------

build_cb(input_csv = "I:/NTS/import/cb_input.csv")

# HB Trip Rates -----------------------------------------------------------

build_hb_trip_rates(input_csv = "I:/NTS/import/hb_tr_input.csv")

# HB Time Mode Split ------------------------------------------------------

build_hb_mts(input_csv = "I:/NTS/import/hb_mts_input.csv",
             seg_max = 300)

# NHB Trip Rates and Time Splits ------------------------------------------

build_nhb_outputs(input_csv = "I:\\NTS\\import\\nhb_input.csv",
                  trip_rate = TRUE,
                  time_split = TRUE)

# HB Productions ---------------------------------------------------------------------

# build_hb_productions(user = user,
#                     drive = "C")
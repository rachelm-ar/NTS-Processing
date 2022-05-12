user <- Sys.info()[[6]]
repo_dir <- paste0("C:/Users/", user, "/Documents/GitHub/NTS-Processing/")
setwd(repo_dir)

# Load custom functions
source(paste0(repo_dir, "utils/utils.R"))
source(paste0(repo_dir, "utils/lookups.R"))
source(paste0(repo_dir, "main/build_cb.R"))
source(paste0(repo_dir, "main/build_hb_trip_rates.R"))
source(paste0(repo_dir, "main/build_hb_mts.R"))
source(paste0(repo_dir, "main/build_hb_productions.R"))
source(paste0(repo_dir, "main/build_nhb_outputs.R"))
source(paste0(repo_dir, "main/build_tld_data.R"))

# Classified build -------------------------------------------------------------
build_cb(input_csv = "D:/NTS/import/cb_input.csv")

# HB Trip Rates ----------------------------------------------------------------
build_hb_trip_rates(input_csv = "D:/NTS/import/hb_tr_input.csv")

# HB Mode Time Split & Phis ----------------------------------------------------
build_hb_mts(input_csv = "D:/NTS/import/hb_mts_input.csv",
             seg_max = 300)

# NHB Trip Rates and Time Splits -----------------------------------------------
build_nhb_outputs(input_csv = "D:/NTS/import/nhb_input.csv",
                  trip_rate = TRUE, time_split = TRUE, seg_max = 300)


# Build TLD and Occupancy ----------------------------------------------------
build_tld_data(input_csv = "D:/NTS/import/tld_input.csv")


# Version controls -------------------------------------------------------------
# cb_build_v3:        retain full raw NTS data, added in tfn specs
# hb_trip_rate_v3.0:  add soc4, embed escort trips to main purpose
# mts_v3.0:           4 level of aggregation, embed escort trips to main purpose
# phi_v3.0:           exclude escort trips
# nhb_trip_rate_v3.0: include escort trips
# nhb_ts_split_v3.0:  include escort trips

#.rs.restartR()
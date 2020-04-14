# Initialise parameters
wd <- "C:/Users/Pluto/Documents/GitHub/NTS-Processing/" # Working directory
csv_input <- "Y:/NTS/nhb_weekly_trip_rates.csv" # CSV of raw data
output_dir <- "Y:/NTS/TfN_Trip_Rates/" # Output directory
ntem_tr_input <- "" # CSV of NTEM data
type <- "nhb" # 'hb' or 'nhb'

# Read in R functions
source(paste0(wd,"trip_rate_model_hb.r"))
source(paste0(wd,"trip_rate_model_nhb.r"))

if(type == "hb"){
  
  hb_trip_rates(csv_input, output_dir, ntem_tr_input)
  
} else if (type == "nhb") {
  
  nhb_trip_rates(csv_input, output_dir, ntem_tr_input)
  
}


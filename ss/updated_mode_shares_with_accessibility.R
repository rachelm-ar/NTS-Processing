### Calculating new mode shares by area type/trip purpose/car availability/MSOA ###


#### Load libraries and import data ####
require("tidyverse")
require("data.table")

# Import new rail share by area type/trip purpose/car availability and MSOA
# Negative rail shares have been replaced by 0 and rail shares >1 have been set to 1
new_railshare_msoa <- read_csv("Y:/NTS/mode_time_splits/rail share regression/new_railshare_input.csv")
mode_shares <- new_railshare_msoa %>% pivot_longer(3:130,names_to = 'id', values_to = 'rail_share')

#Import 'old' mode split by area type/trip purpose/car availability
initial_mode_split <- read_csv("Y:/NTS/mode_time_splits/rail share regression/cars_areatype_purpose_mode_split_transposed.csv")
colnames(initial_mode_split) <- c("id","cars", "area_type", "hb_purpose", "initial_m1", "initial_m2", "initial_m3","initial_m5", "initial_m6")


# Add inital mode shares
mode_shares <- mode_shares %>% left_join(initial_mode_split)


# Calculate updated mode shares
mode_shares$m1 =  ((1 - mode_shares$rail_share) * mode_shares$initial_m1) / (mode_shares$initial_m1 + mode_shares$initial_m2 + mode_shares$initial_m3 + mode_shares$initial_m5) 
mode_shares$m2 =  ((1 - mode_shares$rail_share) * mode_shares$initial_m2) / (mode_shares$initial_m1 + mode_shares$initial_m2 + mode_shares$initial_m3 + mode_shares$initial_m5) 
mode_shares$m3 =  ((1 - mode_shares$rail_share) * mode_shares$initial_m3) / (mode_shares$initial_m1 + mode_shares$initial_m2 + mode_shares$initial_m3 + mode_shares$initial_m5) 
mode_shares$m5 =  ((1 - mode_shares$rail_share) * mode_shares$initial_m5) / (mode_shares$initial_m1 + mode_shares$initial_m2 + mode_shares$initial_m3 + mode_shares$initial_m5) 

mode_shares$m6 = mode_shares$rail_share

# Export full version for comparison, drop old mode shares and export final version
mode_shares = mode_shares[, -4]
mode_shares %>% fwrite(file = "Y:/NTS/mode_time_splits/rail share regression/updated_mode_split_comparison.csv")

mode_shares = mode_shares[, c(1,2,3,4,5,6,12,13,14,15,16) ]
mode_shares %>% fwrite(file = "Y:/NTS/mode_time_splits/rail share regression/updated_mode_split.csv")


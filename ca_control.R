library(tidyverse)

nts_dir <- "Y:/NTS/"
lookup_dir <- str_c(nts_dir, "lookups/")

# Read in classified output
classified_build <- read_csv('Y:/NTS/import/classified_nts_pre-weighting.csv')

# Lookup functions
source(str_c(lookup_dir,"lookups.r"))

classified_build <- classified_build %>%
  mutate(trip_weights = W1 * W5xHh * W2) %>%
  lu_ca()


north_la <- c('E06000001', 'E06000002', 'E06000003', 'E06000004', 'E06000005', 'E06000006',
              'E06000007', 'E06000008', 'E06000009', 'E06000010', 'E06000011', 'E06000012',
              'E06000013', 'E06000014', 'E06000021', 'E06000047', 'E06000049', 'E06000050',
              'E06000057', 'E07000026', 'E07000027', 'E07000028', 'E07000029', 'E07000030',
              'E07000031', 'E07000033', 'E07000034', 'E07000035', 'E07000037', 'E07000038',
              'E07000117', 'E07000118', 'E07000119', 'E07000120', 'E07000121', 'E07000122',
              'E07000123', 'E07000124', 'E07000125', 'E07000126', 'E07000127', 'E07000128',
              'E07000137', 'E07000142', 'E07000163', 'E07000164', 'E07000165', 'E07000166',
              'E07000167', 'E07000168', 'E07000169', 'E07000170', 'E07000171', 'E07000174',
              'E07000175', 'E07000198', 'E08000001', 'E08000002', 'E08000003', 'E08000004',
              'E08000005', 'E08000006', 'E08000007', 'E08000008', 'E08000009', 'E08000010',
              'E08000011', 'E08000012', 'E08000013', 'E08000014', 'E08000015', 'E08000016',
              'E08000017', 'E08000018', 'E08000019', 'E08000021', 'E08000022', 'E08000023',
              'E08000024', 'E08000032', 'E08000033', 'E08000034', 'E08000035', 'E08000036',
              'E08000037', 'W06000001', 'W06000002', 'W06000003', 'W06000004', 'W06000005',
              'W06000006')

# Build ca subset
ca_sub <- classified_build %>%
  select(SurveyYear, HHoldOSLAUA_B01ID, main_mode, trip_origin, trip_weights, ca) %>%
  filter(HHoldOSLAUA_B01ID %in% north_la)

rep <- ca_sub %>%
  group_by(SurveyYear, main_mode, trip_origin, ca) %>%
  summarise(trips = sum(trip_weights,na.rm=TRUE)) %>%
  filter(main_mode == 6, trip_origin=='hb') %>%
  ungroup()

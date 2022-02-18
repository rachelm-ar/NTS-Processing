build_ub <- function(input_csv){
  
  # Load libraries
  library_list <- c("stringr",
                    "readr",
                    "dplyr",
                    "purrr",
                    "tibble",
                    "tidyr")
  
  library_c(library_list)
  
# Directories -----------------------------------------------------
  
  input_csv <- read_csv(input_csv)
  input_csv <- transpose_input_csv(input_csv)
  
  # Read ub columns
  ub_columns <- read_csv(input_csv$ub_columns_csv_dir)
  
  # save directory
  ub_output_dir <- str_c(input_csv$ub_save_dir,
                         "\\",
                         input_csv$ub_name,
                         ".csv")
  
# Read, Select and Join -------------------------------------------
  
  # Table paths
  variable_list <- colnames(ub_columns)
  variable_path <- str_replace(variable_list, "cols", "special_2002-2019_protect.tab")
  variable_path <- str_c(input_csv$nts_raw_dir, variable_path, sep = "\\")

  # Convert ub columns df to list
  extraction_variables <- map(variable_list, pull, .data = ub_columns)
  extraction_variables <- map(extraction_variables, na.omit)
  
  # Read tables
  ub_tables <- map(variable_path, read_delim, delim = "\t")
  
  # Select only variables for each table specified in ub_columns.csv
  ub_tables <- map2(ub_tables, extraction_variables, ~ .x %>% select(all_of(.y)))
  
  ub <- reduce(ub_tables, left_join)
  
  # Write output
  write_csv(ub, ub_output_dir)
  
}
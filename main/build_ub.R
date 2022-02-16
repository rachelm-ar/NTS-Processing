build_ub <- function(ub_input_csv_dir){
  
  # Load libraries
  library_list <- c("stringr",
                    "readr",
                    "dplyr",
                    "purrr",
                    "tibble",
                    "tidyr")
  
  library_c(library_list)
  
# Paths -------------------------------------------------------------------

  input_csv <- read_csv(input_csv_dir)

# Read, Select and Join -------------------------------------------
  
  # Read ub columns
  ub_columns <- read_csv(input_csv$ub_columns_input_csv)
  
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
  write_csv(ub, str_c(input_csv$output_dir, input_csv$output_name, sep = "\\"))
  
}
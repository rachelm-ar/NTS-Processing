build_ub <- function(username = user,
                     extract_version,
                     drive){
  
  # Load libraries
  library_list <- c("stringr",
                    "readr",
                    "dplyr",
                    "purrr",
                    "tibble",
                    "tidyr")
  
  library_c(library_list)
  
# Paths -------------------------------------------------------------------

  nts_c_dir <- str_c("C:/Users/", username, "/Documents/NTS_C/")
  
  import_dir <- str_c(nts_c_dir, "UKDA-7553-tab/tab/")
  
  nts_dir <- ifelse(drive == "C", nts_c_dir, "Y:/NTS/")
  
  ub_columns_dir <- str_c(nts_c_dir, "import/ub_columns/extraction_cols_", extract_version , ".csv")

  # Export dir
  export_dir <- str_c(nts_c_dir, "unclassified builds/ub_", extract_version, ".csv")

# Read, Select and Join -------------------------------------------
  
  # Read ub columns
  ub_columns <- read_csv(ub_columns_dir)
  
  # Table paths
  variable_list <- colnames(ub_columns)
  variable_path <- str_replace(variable_list, "cols", "special_2002-2019_protect.tab")
  variable_path <- str_c(import_dir, variable_path)

  # Convert ub columns df to list
  extraction_variables <- map(variable_list, pull, .data = ub_columns)
  extraction_variables <- map(extraction_variables, na.omit)
  
  # Read tables
  ub_tables <- map(variable_path, read_delim, delim = "\t")
  
  # Select only variables for each table specified in ub_columns.csv
  ub_tables <- map2(ub_tables, extraction_variables, ~ .x %>% select(all_of(.y)))
  
  ub <- reduce(ub_tables, left_join)
  
  # Write output
  write_csv(ub, export_dir)
  
}
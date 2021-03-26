# Install (if needed) and load libraries --------

library_c <- function(packages_list) {
  "
  Description
  ----------
  - Install packages if not previously installed
  - Load in packages
  
  Parameters
  ----------
  packages_list:
    A vector of packages required to run the main function
  
  Return
  ----------
  A boolean list of TRUE indicating all packages are loaded
  
  "
  
  # Packages not installed
  packages_new <- packages_list[!(packages_list %in% installed.packages()[,"Package"])]
  
  # Install packages
  if(length(packages_new)) install.packages(packages_new)
  
  # Load packages
  lapply(packages_list, require, character.only = TRUE)
  
}

# Source lookup -----------------------------------------------------------

source_lookups <- function(username){
  
  lookups_f_dir <- str_c("C:/Users/", username, 
                         "/Documents/GitHub/NTS-Processing/lookups.R")
  
  if(file.exists(lookups_f_dir)){
    
    source(lookups_f_dir)
    
  } else {
    
    stop("Github Folder not in documents - add custom path 'lookups_dir' to lookups.R in NTS Processing")
    
  }
  
}

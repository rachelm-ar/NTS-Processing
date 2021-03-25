# 1) load packages
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

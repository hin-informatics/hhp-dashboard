# SYSTEM VARIABLES ----
# Get user profile
USERPROFILE <- Sys.getenv("USERPROFILE")
LOCATION <- "\\King's College London\\HIN - Informatics - Informatics sensitive data\\Healthy Hearts\\"

# LIBRARIES ----
if (!require("pacman")){
  install.packages("pacman")}
pacman::p_load(
  'janitor'
  , 'tidyverse'
  , 'data.table'
  ,'skimr' 
  ,'tictoc'
)
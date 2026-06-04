# SYSTEM VARIABLES ----
# Sys.setenv()

# LIBRARIES ----
if (!require("pacman")){
  install.packages("pacman")}
pacman::p_load(
  'renv'
  ,'janitor'
  , 'tidyverse'
  , 'data.table'
  ,'skimr' 
  ,'tictoc'
)


# HELPER FUNCTIONS ----
# Get user profile
USERPROFILE <- Sys.getenv("USERPROFILE")
LOCATION <- "\\OneDrive - King's College London\\Insights - HIN - Insights\\Informatics\\1. Projects and Workstreams\\HHP\\R\\data\\"

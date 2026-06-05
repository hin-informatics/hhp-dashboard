# SYSTEM VARIABLES ----
# Get user profile
USERPROFILE <- Sys.getenv("USERPROFILE")
LOCATION <- "\\OneDrive - King's College London\\Insights - HIN - Insights\\Informatics\\1. Projects and Workstreams\\HHP\\R\\data\\"

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


# HELPER FUNCTIONS ----


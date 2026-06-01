# SYSTEM VARIABLES ----
# Sys.setenv()

# LIBRARIES ----
if (!require("pacman")){
  install.packages("pacman")}
pacman::p_load(
  'renv'
  ,'janitor'
  , 'dplyr'
  ,'skimr' 
  ,'tictoc'
)


# HELPER FUNCTIONS ----
# SYSTEM VARIABLES ----
# Sys.setenv()

# LIBRARIES ----
if (!require("pacman")){
  install.packages("pacman")}
pacman::p_load(
  'renv'
  ,'janitor'
  ,'data.table'
  ,'tidyverse'
  ,'plotly'
  ,'skimr' 
  ,'tictoc'
)


# HELPER FUNCTIONS ----
###################
## DO NOT MODIFY ##
###################


# SYSTEM VARIABLES ----

# LIBRARIES ----
if (!require("pacman")){
  install.packages("pacman")}
pacman::p_load(
  'janitor'
  ,'tidyverse'
  ,'data.table'
  ,'skimr' 
  ,'tictoc'
  ,'Microsoft365R'
  ,'ggvenn'
  ,'readxl'
)

# FUNCTIONS
source('scripts/func_demographics.R')
source('scripts/func_optimisation.R')
source('scripts/func_appointments.R')
source('scripts/func_helpers.R')
source('scripts/func_practice_loader.R')



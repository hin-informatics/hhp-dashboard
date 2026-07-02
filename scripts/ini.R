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
  ,'gplots'
  ,'ggvenn'
)

# FUNCTIONS
source('scripts/func_ar.R')
source('scripts/func_ps.R')
source('scripts/func_ea.R')


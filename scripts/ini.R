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
# Transformation Functions:

remove_empty_names <- function(x) {
  
  # For Patient and Appointment data header names
  # Removes rows with empty values
  
  x <- x[x != '']
  return(x)
}

handle_date_num_vars <- function(data){
  
  # Regularises columns with 'date' as part of their names
  
  data <- data %>% mutate(across(
    contains("date"), 
    ~ suppressWarnings(parse_date_time(.x, orders = c("dmy", "ymd", "mdy", "my", "y"))) %>% as_date()
  )) %>%
  mutate(across(contains("value"), as.numeric))
  
  return(data)
}






source('scripts/func_ar.R')
source('scripts/func_ps.R')


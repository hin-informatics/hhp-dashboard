# HHP TRANSFORMATION FUNCTIONS
# efejiro.ashano@nhs.net

# Helper function 1 ----
remove_empty_names <- function(x) {
  
  # For Patient and Appointment data header names
  # Removes rows with empty values
  
  x <- x[x != '']
  return(x)
}

# Helper function 2 ----
handle_date_num_vars <- function(data){
  
  # Regularises columns with 'date' as part of their names
  
  data <- data %>% mutate(across(
    contains("date"), 
    ~ suppressWarnings(parse_date_time(.x, orders = c("dmy", "ymd", "mdy", "my", "y"))) %>% as_date()
  )) %>%
    mutate(across(contains("value"), as.numeric))
  
  return(data)
}

# Helper function 3 ----
appt_cols <- function(data){
  
  setDT(data)
  
  d <- d1 %>% mutate(emis_number = as.character(emis_number)) %>% setDT()
  
  statins <- d[!is.na(statins_first_issue_name_dosage_and_quantity), c(  
    'emis_number',
    'statins_first_issue_name_dosage_and_quantity',
    'statins_first_issue_date_of_issue',
    'statins_first_issue_medication_courses_most_recent_issue_date_in_course',
    'statins_first_issue_medication_courses_course_status_current_past_etc'
    )]
  
  data <- merge(data, statins, all.x = T, by = 'emis_number')
  
  return(data)
}





## HELPER FUNCTIONS ##

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
    mutate(across(contains("value"), as.numeric)) %>%
    mutate(emis_number = as.numeric(emis_number, na.omit = T))
  
  return(data)
}

# Helper function 3 ----
# This implementation handles multiple instances of prescription activity in all appointments across five years.
appt_prescriptions <- function(data){
  
  setDT(data)
  setDT(d1)
  
  d <- d1
  
  d <- setorder(d, emis_number, gp_appointments_5_years_appointment_date)
  
  statins <- d[, .(
    statins_first_issue_name_dosage_and_quantity = last(na.omit(statins_first_issue_name_dosage_and_quantity)),
    statins_first_issue_date_of_issue = first(na.omit(statins_first_issue_date_of_issue)),
    statins_first_issue_medication_courses_most_recent_issue_date_in_course = last(na.omit(statins_first_issue_medication_courses_most_recent_issue_date_in_course)),
    statins_first_issue_medication_courses_course_status_current_past_etc = last(na.omit(statins_first_issue_medication_courses_course_status_current_past_etc))
  ), by = 'emis_number']
  
  ## >>> Add other Prescriptions here <<< ##
  
  data <- merge(data, statins, all.x = T, by = 'emis_number')
  
  ## >>> Merge other prescriptions here <<< ##
  
  return(data)
}
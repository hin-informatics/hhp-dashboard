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

assign_cvrm_cohort <- function(data) {
  data %>%
    mutate(cvrm_cohort = case_when(
      ckd_exist == 1 & diabetes_exist == 0 & hypertension_exist == 0 ~ "CKD only",
      ckd_exist == 1 & diabetes_exist == 0 & hypertension_exist == 1 ~ "HTN & CKD",
      ckd_exist == 0 & diabetes_exist == 0 & hypertension_exist == 1 ~ "HTN only",
      ckd_exist == 0 & diabetes_exist == 1 & hypertension_exist == 1 ~ "HTN & T2D",
      ckd_exist == 0 & diabetes_exist == 1 & hypertension_exist == 0 ~ "T2D only",
      ckd_exist == 1 & diabetes_exist == 1 & hypertension_exist == 0 ~ "T2D & CKD",
      ckd_exist == 1 & diabetes_exist == 1 & hypertension_exist == 1 ~ "All three",
      TRUE ~ "None"
    ))
}
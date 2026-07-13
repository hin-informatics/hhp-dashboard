# HHP TRANSFORMATION FUNCTIONS
# alan.rajan@nhs.net

# Instructions:
# 1. Git Pull
# 2. Make adjustments between start and end code blocks
# 3. Test it works
# 4. Add, Commit and push back to Git.


## DEMOGRAPHICS FUNCTIONS ##

# Age band ----

# Creates 10-year age band column with data and variable input.
create_age_bands <- function(data, age_var = age){
  message('Creating 10-year age band column: ')
  
  d = data
  
  ## Start code here ##
  d <- d %>%
    mutate(grouped_age_bands = case_when(
      #list of cases below
      {{ age_var }} < 20 ~ "18-19",
      {{ age_var }} < 30 ~ "20-29",
      {{ age_var }} < 40 ~ "30-39",
      {{ age_var }} < 50 ~ "40-49",
      {{ age_var }} < 60 ~ "50-59",
      {{ age_var }} < 70 ~ "60-69",
      {{ age_var }} < 80 ~ "70-79",
      {{ age_var }} < 90 ~ "80-89",
      {{ age_var }} >= 90 ~ "90+",
      TRUE ~ "Unknown"
  ))
  ## End code here ##
  
  return(d)
}

# Ethnic Grouping ----
# Creates major ethnic categories column with data and variable input.

create_ethnic_grps <- function(data, ethnic_var = ethnic_origin){
  message('Creating major ethnic groups: ')
  
  d = data
  
  ## Start code here ##
  
  ### Asian or Asian British Groups ###
  d <- d %>% 
    mutate(grouped_ethnic_origin = case_when(
      {{ ethnic_var }} %in% c(
        "Asian and Chinese - ethnic category 2001 census",
        "Asian or Asian British: any other Asian background - England and Wales ethnic category 2011 census",
        "Bangladeshi",
        "Bangladeshi or British Bangladeshi - ethnic category 2001 census",
        "Filipino - ethnic category 2001 census",
        "Indian or British Indian - ethnic category 2001 census",
        "Other Asian background - ethnic category 2001 census",
        "Other Asian ethnic group",
        "Other Asian or Asian unspecified - ethnic category 2001 census",
        "Pakistani",
        "Pakistani or British Pakistani - ethnic category 2001 census",
        "Vietnamese") ~ "Asian or Asian British",
      
      ### Black, African, Caribbean or Black British Groups ###
      {{ ethnic_var }} %in% c(
        "African - ethnic category 2001 census",
        "African: African, African Scottish or African British - Scotland ethnic category 2011 census",
        "Black African",
        "Black British - ethnic category 2001 census",
        "Black Caribbean",
        "Black Caribbean/W.I./Guyana",
        "Black or African or Caribbean or Black British: African - England and Wales ethnic category 2011 census",
        "Black or African or Caribbean or Black British: Caribbean - England and Wales ethnic category 2011 census",
        "Black or African or Caribbean or Black British: other Black or African or Caribbean background - England and Wales ethnic category 2011 census",
        "Caribbean - ethnic category 2001 census",
        "Nigerian - ethnic category 2001 census",
        "North African - ethnic category 2001 census",
        "Other Black background - ethnic category 2001 census",
        "Somali - ethnic category 2001 census") ~ "Black, African, Caribbean or Black British",
      
      ### Mixed / Multiple Ethnic Groups ###
      {{ ethnic_var }} %in% c(
        "Black Caribbean and White",
        "Black and Asian - ethnic category 2001 census",
        "Black and White - ethnic category 2001 census",
        "Mixed Asian - ethnic category 2001 census",
        "Mixed multiple ethnic groups: any other Mixed or multiple ethnic background - England and Wales ethnic category 2011 census",
        "Other Mixed background - ethnic category 2001 census",
        "Other Mixed or Mixed unspecified - ethnic category 2001 census",
        "Other ethnic, Asian/White orig",
        "Other ethnic, mixed origin",
        "White and Asian - ethnic category 2001 census",
        "White and Black African - ethnic category 2001 census",
        "White and Black Caribbean - ethnic category 2001 census") ~ "Mixed / Multiple Ethnicity",
      
      ### Not Stated Groups ###
      {{ ethnic_var }} %in% c(
        "Ethnic category not stated - 2001 census",
        "Ethnicity and other related nationality data",
        "Patient ethnicity unknown",
        "Refusal by patient to provide information about ethnic group") ~ "Not Stated",
      
      ### Other Ethnic Groups ###
      {{ ethnic_var }} %in% c(
        "Any other group - ethnic category 2001 census",
        "Arab - ethnic category 2001 census",
        "Asian or Asian British: Chinese - England and Wales ethnic category 2011 census",
        "Chinese - ethnic category 2001 census",
        "Iranian - ethnic category 2001 census",
        "Latin American - ethnic category 2001 census",
        "Other - ethnic category 2001 census",
        "South and Central American - ethnic category 2001 census") ~ "Other Ethnic Group",
      
      ### White Groups ###
      {{ ethnic_var }} %in% c(
        "Albanian - ethnic category 2001 census",
        "British or mixed British - ethnic category 2001 census",
        "Bulgarian",
        "Greek - ethnic category 2001 census",
        "Gypsy/Romany - ethnic category 2001 census",
        "Irish - ethnic category 2001 census",
        "Other White European or European unspecified or Mixed European - ethnic category 2001 census",
        "Other White background - ethnic category 2001 census",
        "Other White or White unspecified - ethnic category 2001 census",
        "Other mixed White - ethnic category 2001 census",
        "Portuguese",
        "Scottish - ethnic category 2001 census",
        "White",
        "White - ethnic group",
        "White British",
        "White British - ethnic category 2001 census",
        "White Irish - ethnic category 2001 census",
        "White: English or Welsh or Scottish or Northern Irish or British - England and Wales ethnic category 2011 census",
        "White: any other White background - England and Wales ethnic category 2011 census") ~ "White",
      TRUE ~ "Not Coded"
    ))
  
  ## End code here ##
  
  return(d)
}


# Disease Cohorts
# Hypertension
# Diabetes
# CKD

# Palliative Care
# Frailty

# Geography Assignment  ----

# Creates useful geography and IMD columns with data and variable input.
create_geo_grps <- function(data){
  message('Creating geographies and IMD: ')
  
  d = data
  
  ## Start code here ##
  imd <- read.csv("data/IMD_2010.csv")
  
  # left_join merges those two columns LSOA fields to data 
   d <- d %>% left_join(
      imd %>% select(lsoa_code, la_name),
      by = c("lower_layer_area_2011" = "lsoa_code")
    )
  ## End code here ##
  
  return(d)
}


## OPTIMISATION FUNCTIONS ##

# Hypertension Optimisation ----

# Patient optimised if diagnosed with Hypertension AND:
# ACR < 70 & Systolic < 140 & diastolic < 90
# ACR >= 70 & Systolic < 130 & diastolic < 80
# ACR value & Frail & Systolic < 150 & diastolic < 90

optimised_htn <- function(data){
  message('Creating optimisation flag: Hypertension ')
  
  d = data
  
  ## Start code here ##
  d <- d %>%
    mutate(
      hypertension_exist = ifelse(is.na(hypertension_diagnosis_code_term), 0, 1) # Checking hypertension exists or not (Yes = 1 and No = 0)
    ) %>%
    mutate(
      hypertension_optimised = case_when(
        hypertension_exist == 1 &
          (acr_value < 70 | is.na(acr_value)) &
          clinic_bp_value < 140 & clinic_bp_secondary_value < 90 ~ 1,
        hypertension_exist == 1 &
          acr_value >= 70 &
          clinic_bp_value < 130 & clinic_bp_secondary_value < 80 ~ 1,
        hypertension_exist == 1 &
          !is.na(acr_value) & !is.na(moderate_or_severe_frailty_code_term) &
          clinic_bp_value < 150 & clinic_bp_secondary_value < 90 ~ 1,
        hypertension_exist == 0 ~ NA_real_, # If hypertension doesn't exist in a pt, we assign 'NA_real_' meaning Not applicable
        TRUE ~ 0)
    )
  ## End code here ##
  
  return(d)
}

# Chronic Kidney Disease Optimisation ----

# Patient optimised if diagnosed with CKD AND:
# YES Diabetes AND ACR >= 3 & taking ACEi OR ARB + SGLT2i + statin 
# NO Diabetes AND ACR >= 22.6 & & taking ACEi OR ARB + SGLT2i + statin

optimised_ckd <- function(data){
  message('Creating optimisation flag: CKD ')
  
  d = data
  
  ## Start code here ##
  d <- d %>% 
    mutate(
      ckd_exist = ifelse(is.na(ckd_diagnosis_code_term), 0, 1) # Checking Chronic Kidney Disease exists or not (Yes = 1 and No = 0)
    ) %>%
    mutate(
      ckd_optimised = case_when(
        ckd_exist == 1 &
          !is.na(type_2_diabetes_diagnosis_code_term) & acr_value >= 3 &
          (ac_ei_course_status == "Current" | arb_course_status == "Current") & sglt2i_course_status == "Current" & statins_first_issue_medication_courses_course_status_current_past_etc == "Current" ~ 1,
        ckd_exist == 1 &
          is.na(type_2_diabetes_diagnosis_code_term) & acr_value >= 22.6 &
          (ac_ei_course_status == "Current" | arb_course_status == "Current") & sglt2i_course_status == "Current" & statins_first_issue_medication_courses_course_status_current_past_etc == "Current" ~ 1,
        ckd_exist == 0 ~ NA_real_, # If Chronic Kidney Disease doesn't exist in a pt, we assign 'NA_real_' meaning Not applicable
        TRUE ~ 0)
    )
  ## End code here ##
  
  return(d)
}

# Diabetes Optimisation ----

# Patient optimised if diagnosed with Type 2 Diabetes AND:
# NO frailty (blank) & taking Metformin + SGLT2i & HbA1c <= 53
# YES frailty & taking Metformin & HbA1c <= 75

optimised_t2d <- function(data){
  message('Creating optimisation flag: Type 2 diabetes ')
  
  d = data

  ## Start code here ##
  d <- d %>%
    # Checking Diabetes exists or not (Yes = 1 and No = 0)
    mutate(
      diabetes_exist = ifelse(is.na(type_2_diabetes_diagnosis_code_term), 0, 1)
    ) %>%
    mutate(
      diabetes_optimised = case_when(
        diabetes_exist == 1 &
          is.na(moderate_or_severe_frailty_code_term) &
          !is.na(metformin_rx_first_issue_name_dose) & !is.na(sglt2i_first_issue_name_dosage) & hb_a1c_value <= 53 ~ 1,
        diabetes_exist == 1 &
          !is.na(moderate_or_severe_frailty_code_term) &
          !is.na(metformin_rx_first_issue_name_dose) & hb_a1c_value <= 75 ~ 1,
        diabetes_exist == 0 ~ NA_real_, # If Diabetes doesn't exist in a pt, we assign 'NA_real_' meaning Not applicable
        TRUE ~ 0)
    )
  ## End code here ##
  
  return(d)
}

# All patients Optimisation ----

# Patient optimised if no HYP, CKD or T2D AND:
# YES CVD (i.e. CHD, PAD, PVD, Stroke OR TIA = TRUE) & Non-HDL <= 2.6
# NO CVD AND QRISK >= 10 & on Statin

# Patients are NOT optimised if missing ACR, BP, HbA1Ac records.


optimised_all <- function(data){
  message('Creating optimisation flag: All patients ')
  
  d = data
  
  ## Start code here ##
  d <- d %>%
    mutate(
      all_patients = ifelse(hypertension_exist == 1 | ckd_exist == 1 | diabetes_exist == 1, 0, 1)
      ) %>%
    mutate(
      all_optimised = case_when(
        all_patients == 1 & (!is.na(chd_code_term) | !is.na(pad_code_term) | !is.na(pvd_code_term) | !is.na(non_hemorrhagic_stroke_code_term) | !is.na(tia_code_term)) & non_hdl_value <= 2.6 & statins_first_issue_medication_courses_course_status_current_past_etc == "Current" ~ 1,
        all_patients == 1 & (is.na(chd_code_term) & is.na(pad_code_term) & is.na(pvd_code_term) & is.na(non_hemorrhagic_stroke_code_term) & is.na(tia_code_term)) & qrisk_value >= 10 & statins_first_issue_medication_courses_course_status_current_past_etc == "Current" ~ 1,
        all_patients == 0 ~ NA_real_,
        TRUE ~ 0)
  )
  ## End code here ##
  
  return(d)
}  

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
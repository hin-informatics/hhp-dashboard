# HHP TRANSFORMATION FUNCTIONS
# alan.rajan@nhs.net

# Instructions:
# 1. Git Pull
# 2. Make adjustments between start and end code blocks
# 3. Test it works
# 4. Add, Commit and push back to Git.


# Age band Function ----
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

# Geography Assignment Function ----
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
      # Column 1 hypertension_optimised
      hypertension_optimised = case_when(
        !is.na(hypertension_diagnosis_code_term) & acr_value < 70 & clinic_bp_value < 140 & clinic_bp_secondary_value < 90 |
          !is.na(hypertension_diagnosis_code_term) & acr_value >= 70 & clinic_bp_value < 130 & clinic_bp_secondary_value < 80 |
          !is.na(hypertension_diagnosis_code_term) & !is.na(acr_value) & !is.na(moderate_or_severe_frailty_code_term) & clinic_bp_value < 150 & clinic_bp_secondary_value < 90
        ~ '1'
      ),
      # # Column 2 hypertension_exist
      hypertension_exist = ifelse(is.na(hypertension_diagnosis_code_term), 'no','yes')
    )
  
  ## End code here ##
  
  return(d)
}

# Chronic Kidney Disease Optimisation ----

optimised_ckd <- function(data){
  message('Creating optimisation flag: CKD ')
  
  d = data
  
  ## Start code here ##
  d <- d %>% mutate(
    ckd_optimised = case_when(),
    ckd_exists = ifelse(is.na(ckd_diagnosis_code_term), 'no', 'yes')
  )
  
  # Need to workout statin and second clause about exclusion
  ## End code here ##
  
  return(d)
}

# Diabetes ----

# Patient optimised if diagnosed with Diabetes AND:
# LOGIC 1
# LOGIC 2
# LOGIC 3

optimised_t2d<- function(data){
  message('Creating optimisation flag: Type 2 diabetes ')
  
  d = data
  
  ## Start code here ##
  
  ## End code here ##
  
  return(d)
}

# All patients  ----

# Patient optimised if:
# LOGIC 1
# LOGIC 2
# LOGIC 3

optimised_all <- function(data){
  message('Creating optimisation flag: All patients ')
  
  d = data
  
  ## Start code here ##
  
  ## End code here ##
  
  return(d)
}  

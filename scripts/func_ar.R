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
      hypertension_exist = ifelse(is.na(hypertension_diagnosis_code_term), 0,1)
    ) %>%
    mutate(
      hypertension_optimised = case_when(
        hypertension_exist == 1 & (acr_value < 70 | is.na(acr_value)) & clinic_bp_value < 140 & clinic_bp_secondary_value < 90 ~ 1,
        hypertension_exist == 1 & acr_value >= 70 & clinic_bp_value < 130 & clinic_bp_secondary_value < 80 ~ 1,
        hypertension_exist == 1 & !is.na(acr_value) & !is.na(moderate_or_severe_frailty_code_term) & clinic_bp_value < 150 & clinic_bp_secondary_value < 90 ~ 1,
        TRUE ~ 0)
    )
  ## End code here ##
  
  return(d)
}

# Chronic Kidney Disease Optimisation ----

# Patient optimised if diagnosed with CKD AND:
  # Has got (Diabetes AND acr >=3 OR has got no diabetes AND acr >=22.6) AND is taking any dose of (acei OR arb AND sglt2i AND Statin, the medication should be CURRENT) -- Where is intolerance?
  # Any AND Statin


optimised_ckd <- function(data){
  message('Creating optimisation flag: CKD ')
  
  d = data
  
  ## Start code here ##
  d <- d %>% 
    mutate(
      ckd_exists = ifelse(is.na(ckd_diagnosis_code_term), 0,1)
    ) %>%
    mutate(
      ckd_optimised = case_when(
        ckd_exists == 1 & !is.na(type_2_diabetes_diagnosis_code_term) & acr_value >= 3 & (ac_ei_course_status == "Current" | arb_course_status == "Current") & (sglt2i_course_status == "Current")
        ~ 1,
        ckd_exists == 1 & is.na(type_2_diabetes_diagnosis_code_term) & acr_value >= 22.6 & (ac_ei_course_status == "Current" | arb_course_status == "Current") & (sglt2i_course_status == "Current")
        ~ 1,
        TRUE ~ 0)
    )
  # End code here ##
  
  return(d)
}

# Diabetes ----

# Patient optimised if diagnosed with Diabetes AND:
# no frailty value AND metformin AND sglt2i AND hba1c <= 53
# frailty has value which is Moderate or Severe AND metformin AND hba1c <= 75

optimised_t2d <- function(data){
  message('Creating optimisation flag: Type 2 diabetes ')
  
  d = data

  ## Start code here ##
  d <- d %>%
    mutate(
      diabetes_exist = ifelse(is.na(type_2_diabetes_diagnosis_code_term), 0,1)
    ) %>%
    mutate(
      diabetes_optimised = case_when(
        diabetes_exist == 1 & is.na(moderate_or_severe_frailty_code_term) & !is.na(metformin_rx_first_issue_name_dose) & !is.na(sglt2i_first_issue_name_dosage) & hb_a1c_value <= 53 ~ 1,
        diabetes_exist == 1 & !is.na(moderate_or_severe_frailty_code_term) & !is.na(metformin_rx_first_issue_name_dose) & hb_a1c_value <= 75 ~ 1,
        TRUE ~ 0)
    )
  ## End code here ##
  
  return(d)
}

# All patients  ----

# Patient optimised if:
# CVD: Has value in (chd AND pad AND pvd AND (non_hemorrhagic_stroke OR tia)) AND (ldl <= 2 OR non_hdl <=2.6)
# No CVD: Has value in (chd AND pad AND pvd AND (non_hemorrhagic_stroke OR tia)) AND (ldl <= 2 OR non_hdl <=2.6) AND (qrisk >=10) AND (antilipid medication AND ldl <= 2)
# ALL: Has value in acr AND bp AND hba1c

optimised_all <- function(data){
  message('Creating optimisation flag: All patients ')
  
  d = data
  
  ## Start code here ##

  ## End code here ##
  
  return(d)
}  
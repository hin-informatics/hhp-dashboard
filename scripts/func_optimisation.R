## OPTIMISATION FUNCTIONS ##

# Hypertension Optimisation ----

# Patient optimised if diagnosed with Hypertension AND:
# ACR < 70 & Systolic < 140 & diastolic < 90
# ACR >= 70 & Systolic < 130 & diastolic < 80
# ACR value & Frail & Systolic < 150 & diastolic < 90

optimised_htn <- function(ptts_dt){
  d = ptts_dt
  
  d <- d %>%
    mutate(
      hypertension_exist = ifelse(is.na(hypertension_diagnosis_code_term), 0, 1),
      
      # Determine whether we use Home BP (preferred) or Clinic BP
      has_home_bp = !is.na(home_systolic_bp_value) & !is.na(home_diastolic_bp_value),
      systolic_val = if_else(has_home_bp, home_systolic_bp_value, clinic_bp_value),
      diastolic_val = if_else(has_home_bp, home_diastolic_bp_value, clinic_bp_secondary_value),
      
      # Set targets (Home targets are 5 mmHg lower than Clinic targets)
      target_systolic_frail = if_else(has_home_bp, 145, 150),
      target_diastolic_frail = if_else(has_home_bp, 85, 90),
      
      target_systolic_high_acr = if_else(has_home_bp, 125, 130),
      target_diastolic_high_acr = if_else(has_home_bp, 75, 80),
      
      target_systolic_standard = if_else(has_home_bp, 135, 140),
      target_diastolic_standard = if_else(has_home_bp, 85, 90)
    ) %>%
    mutate(
      hypertension_optimised = case_when(
        hypertension_exist == 0 ~ NA_real_,
        
        # 1. Frail Patients (Relaxed target regardless of ACR status)
        hypertension_exist == 1 & !is.na(moderate_or_severe_frailty_code_term) &
          systolic_val < target_systolic_frail & diastolic_val < target_diastolic_frail ~ 1,
        
        # 2. High ACR Patients (ACR >= 70)
        hypertension_exist == 1 & acr_value >= 70 &
          systolic_val < target_systolic_high_acr & diastolic_val < target_diastolic_high_acr ~ 1,
        
        # 3. Standard Patients (ACR < 70 or missing)
        hypertension_exist == 1 & (acr_value < 70 | is.na(acr_value)) &
          systolic_val < target_systolic_standard & diastolic_val < target_diastolic_standard ~ 1,
        
        TRUE ~ 0
      )
    )
  
  message('Optimisation flag created for Hypertension (incorporating Home BP targets).')
  return(d)
}

# Chronic Kidney Disease Optimisation ----

# Patient optimised if diagnosed with CKD AND:
# YES Diabetes AND ACR >= 3 & taking ACEi OR ARB + SGLT2i + statin 
# NO Diabetes AND ACR >= 22.6 & & taking ACEi OR ARB + SGLT2i + statin

optimised_ckd <- function(ptts_dt){
  d = ptts_dt
  d <- d %>%
    mutate(
      ckd_exist = ifelse(is.na(ckd_diagnosis_code_term), 0, 1),
      
      # Determine if clinical drug requirements are met or medically excluded
      acei_arb_ok = (ac_ei_course_status == "Current" | arb_course_status == "Current" |
                       acei_excluded == 1 | arb_excluded == 1),
      sglt2i_ok = (sglt2i_course_status == "Current" | sglt2i_excluded == 1),
      statin_ok = (statins_first_issue_medication_courses_course_status_current_past_etc == "Current" | statin_excluded == 1)
    ) %>%
    mutate(
      ckd_optimised = case_when(
        ckd_exist == 1 & !is.na(type_2_diabetes_diagnosis_code_term) & acr_value >= 3 &
          acei_arb_ok & sglt2i_ok & statin_ok ~ 1,
        ckd_exist == 1 & is.na(type_2_diabetes_diagnosis_code_term) & acr_value >= 22.6 &
          acei_arb_ok & sglt2i_ok & statin_ok ~ 1,
        ckd_exist == 0 ~ NA_real_,
        TRUE ~ 0)
    )
  message('Optimisation flag created for CKD (incorporating exceptions).')
  return(d)
}

# Diabetes Optimisation ----

# Patient optimised if diagnosed with Type 2 Diabetes AND:
# NO frailty (blank) & taking Metformin + SGLT2i & HbA1c <= 53
# YES frailty & taking Metformin & HbA1c <= 75

optimised_t2d <- function(ptts_dt){
  
  d = ptts_dt
  
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
  message('Optimisation flag created for diabetes.')
  return(d)
}

# All patients Optimisation ----

# Patient optimised if no HYP, CKD or T2D AND:
# YES CVD (i.e. CHD, PAD, PVD, Stroke OR TIA = TRUE) & Non-HDL <= 2.6
# NO CVD AND QRISK >= 10 & on Statin

# Patients are NOT optimised if missing ACR, BP, HbA1Ac records.

optimised_all <- function(ptts_dt){
  d = ptts_dt
  d <- d %>%
    mutate(
      all_patients = ifelse(hypertension_exist == 1 | ckd_exist == 1 | diabetes_exist == 1, 0, 1),
      statin_ok = (statins_first_issue_medication_courses_course_status_current_past_etc == "Current" | statin_excluded == 1)
    ) %>%
    mutate(
      all_optimised = case_when(
        # CVD Patients: Statin and non-HDL <= 2.6
        all_patients == 1 & (!is.na(chd_code_term) | !is.na(pad_code_term) | !is.na(pvd_code_term) |
                               !is.na(non_hemorrhagic_stroke_code_term) | !is.na(tia_code_term)) &
          non_hdl_value <= 2.6 & statin_ok ~ 1,
        
        # Non-CVD Patients: QRISK >= 10 and Statin
        all_patients == 1 & (is.na(chd_code_term) & is.na(pad_code_term) & is.na(pvd_code_term) &
                               is.na(non_hemorrhagic_stroke_code_term) & is.na(tia_code_term)) &
          qrisk_value >= 10 & statin_ok ~ 1,
        
        all_patients == 0 ~ NA_real_,
        TRUE ~ 0)
    )
  message('Optimisation flag created for other patients.')
  return(d)
}

# Time to Optimisation ----
#
#
#
optimisation_date <- function(ptts_dt){
  
  d = ptts_dt
  
  d <- d %>%
    mutate(
      dx_date_htn = if_else(hypertension_exist == 1, hypertension_diagnosis_earliest_date, as.Date(NA)),
      dx_date_t2d = if_else(diabetes_exist == 1, type_2_diabetes_diagnosis_earliest_date, as.Date(NA)),
      dx_date_ckd = if_else(ckd_exist == 1, ckd_diagnosis_date, as.Date(NA))
    )
  
  # Efficient element-wise maximum date across active conditions
  d$most_recent_dx_date <- do.call(
    pmax,
    c(d[, c("dx_date_htn", "dx_date_t2d", "dx_date_ckd")], na.rm = TRUE)
  )
  
  
  d <- d %>%
    mutate(
      # Hypertension Optimisation Date: Prefer Home BP date, fallback to Clinic BP
      opt_date_htn = if_else(
        hypertension_exist == 1 & hypertension_optimised == 1,
        if_else(!is.na(home_systolic_bp_latest_date), home_systolic_bp_latest_date, clinic_bp_latest_date),
        as.Date(NA)
      ),
      
      # CKD Optimisation Date: Maximum of the first issue dates for required meds
      opt_date_ckd = if_else(
        ckd_exist == 1 & ckd_optimised == 1,
        pmax(
          pmin(ac_ei_first_issue_date, arb_first_issue_date, na.rm = TRUE),
          sglt2i_first_issue_date,
          statins_first_issue_date_of_issue,
          na.rm = TRUE
        ),
        as.Date(NA)
      ),
      
      # Diabetes Optimisation Date: Max of metformin, sglt2i (if non-frail), and latest HbA1c date
      opt_date_t2d = if_else(
        diabetes_exist == 1 & diabetes_optimised == 1,
        if_else(
          !is.na(moderate_or_severe_frailty_code_term),
          pmax(metformin_rx_first_issue_date, hb_a1c_latest_date, na.rm = TRUE),
          pmax(metformin_rx_first_issue_date, sglt2i_first_issue_date, hb_a1c_latest_date, na.rm = TRUE)
        ),
        as.Date(NA)
      )
    )
  
  d$overall_optimisation_date <- do.call(pmax, c(d[, c("opt_date_htn", "opt_date_ckd", "opt_date_t2d")], na.rm = TRUE))
  
  d <- d %>%
    mutate(
      fully_optimised = if_else(
        (hypertension_exist == 0 | hypertension_optimised == 1) &
          (ckd_exist == 0 | ckd_optimised == 1) &
          (diabetes_exist == 0 | diabetes_optimised == 1),
        1, 0
      ),
      time_to_optimisation_days = if_else(
        fully_optimised == 1,
        as.numeric(overall_optimisation_date - most_recent_dx_date),
        as.numeric(NA)
      )
    )
  message('Created optimisation dates.')
  return(d)
  
}




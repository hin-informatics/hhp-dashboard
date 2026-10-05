## OPTIMISATION FUNCTIONS ##

# Hypertension Optimisation ----

# Patient optimised if diagnosed with Hypertension AND:
# ACR < 70 & Systolic < 140 & diastolic < 90
# ACR >= 70 & Systolic < 130 & diastolic < 80
# ACR value & Frail & Systolic < 150 & diastolic < 90

optimised_htn <- function(ptts_dt, base_dt){
  if (missing(base_dt) || is.null(base_dt) || nrow(base_dt) == 0) {
    stop("Error in optimised_htn: 'base_dt' (Report 3 longitudinal baseline data) is required and cannot be empty.")
  }
  
  d <- copy(ptts_dt)
  setDT(d)
  
  d[, hypertension_exist := fifelse(is.na(hypertension_diagnosis_code_term), 0, 1)]
  
  message('Optimising Hypertension using longitudinal Baseline Report 3 BP readings...')
  
  b <- copy(base_dt)
  setDT(b)
  
  # 1. Forward-fill patient identifiers in b
  b <- b %>% tidyr::fill(emis_number, organisation_name, age, ethnic_origin, gender, .direction = "down")
  setDT(b)
  
  # 2. Extract clinical target drivers and earliest diagnosis date from d
  pt_targets <- d[, .(
    emis_number,
    is_frail = !is.na(moderate_or_severe_frailty_code_term),
    is_high_acr = !is.na(acr_value) & acr_value >= 70,
    hypertension_exist,
    dx_date_htn = hypertension_diagnosis_earliest_date
  )]
  
  b <- merge(b, pt_targets, by = "emis_number", all.x = TRUE)
  
  # 3. Determine Home vs Clinic BP reading per row
  b[, `:=`(
    has_home = !is.na(home_systolic_bp_latest_value) & !is.na(home_diastolic_bp_latest_value),
    has_clinic = !is.na(clinic_bp_latest_value) & !is.na(clinic_bp_latest_secondary_value)
  )]
  
  b[, bp_date := fifelse(has_home, home_systolic_bp_latest_date, clinic_bp_latest_date)]
  b[, sys_val := fifelse(has_home, home_systolic_bp_latest_value, clinic_bp_latest_value)]
  b[, dia_val := fifelse(has_home, home_diastolic_bp_latest_value, clinic_bp_latest_secondary_value)]
  b[, bp_type := fifelse(has_home, "Home", "Clinic")]
  
  # Keep only rows with valid BP measurements and dates
  b <- b[!is.na(sys_val) & !is.na(dia_val) & !is.na(bp_date)]
  
  # ----------------------------------------------------------------------------
  # CLINICAL UPDATE: Restrict BP readings to on or after earliest diagnosis date
  # ----------------------------------------------------------------------------
  # In primary care records, patients often have normotensive/in-range readings
  # from routine health checks, well-person checks, or acute visits months or years
  # BEFORE their hypertension diagnosis date was formally entered on the problem list.
  # Restricting evaluation to bp_date >= dx_date_htn ensures:
  # 1) Clinical optimisation reflects genuine post-diagnosis management & therapeutic control.
  # 2) Pre-diagnostic normal readings do not trigger premature false-positive optimisation.
  # 3) Turnaround interval (time_to_optimisation_days) is logically non-negative (>= 0).
  # 4) Appointment counts in turnaround window accurately capture post-diagnosis care.
  b <- b[is.na(dx_date_htn) | bp_date >= dx_date_htn]
  
  # 4. Set target thresholds per reading event
  b[, `:=`(
    sys_target = fcase(
      is_frail, fifelse(has_home, 145, 150),
      is_high_acr, fifelse(has_home, 125, 130),
      default = fifelse(has_home, 135, 140)
    ),
    dia_target = fcase(
      is_frail, fifelse(has_home, 85, 90),
      is_high_acr, fifelse(has_home, 75, 80),
      default = fifelse(has_home, 85, 90)
    )
  )]
  
  # 5. Flag in-range status
  b[, in_range := (sys_val < sys_target & dia_val < dia_target)]
  
  # Sort chronologically by patient and date
  setorder(b, emis_number, bp_date)
  
  # 6. Calculate longitudinal metrics per patient
  pt_bp_summary <- b[, {
    tot_readings <- .N
    latest_row <- .SD[.N]
    in_range_dates <- bp_date[in_range == TRUE]
    first_in_range_date <- if (length(in_range_dates) > 0) min(in_range_dates) else as.Date(NA)
    
    if (is.na(first_in_range_date)) {
      list(
        bp_first_in_range_date = as.Date(NA),
        bp_post_target_total_readings = 0L,
        bp_post_target_in_range_readings = 0L,
        bp_in_range_ratio = 0.0,
        bp_latest_date = latest_row$bp_date,
        bp_latest_systolic = latest_row$sys_val,
        bp_latest_diastolic = latest_row$dia_val
      )
    } else {
      post_dt <- .SD[bp_date >= first_in_range_date]
      p_tot <- nrow(post_dt)
      p_in <- sum(post_dt$in_range)
      ratio <- round(p_in / p_tot, 3)
      list(
        bp_first_in_range_date = first_in_range_date,
        bp_post_target_total_readings = as.integer(p_tot),
        bp_post_target_in_range_readings = as.integer(p_in),
        bp_in_range_ratio = ratio,
        bp_latest_date = latest_row$bp_date,
        bp_latest_systolic = latest_row$sys_val,
        bp_latest_diastolic = latest_row$dia_val
      )
    }
  }, by = emis_number]
  
  # 7. Merge onto patient dataset
  d <- merge(d, pt_bp_summary, by = "emis_number", all.x = TRUE)
  
  # Assign default 0 for patients without BP records
  d[is.na(bp_in_range_ratio), bp_in_range_ratio := 0.0]
  d[is.na(bp_post_target_total_readings), bp_post_target_total_readings := 0L]
  d[is.na(bp_post_target_in_range_readings), bp_post_target_in_range_readings := 0L]
  
  # 8. Define new optimisation rule: Optimised IF bp_in_range_ratio >= 0.5
  d[, hypertension_optimised := fcase(
    hypertension_exist == 0, NA_real_,
    hypertension_exist == 1 & bp_in_range_ratio >= 0.5, 1,
    default = 0
  )]
  
  # 9. Save event-level post-target readings table for Power BI drilldown
  post_readings <- b[emis_number %in% pt_bp_summary[!is.na(bp_first_in_range_date)]$emis_number]
  post_readings <- merge(post_readings, pt_bp_summary[, .(emis_number, bp_first_in_range_date)], by = "emis_number", all.x = TRUE)
  post_target_events <- post_readings[bp_date >= bp_first_in_range_date, .(
    emis_number,
    organisation_name,
    bp_date,
    bp_type,
    systolic_value = sys_val,
    diastolic_value = dia_val,
    systolic_target = sys_target,
    diastolic_target = dia_target,
    is_in_range = in_range
  )]
  write.csv(post_target_events, "data/patient_bp_readings.csv", row.names = FALSE, na = "")
  message(sprintf('Exported %d post-target BP readings to data/patient_bp_readings.csv.', nrow(post_target_events)))
  
  message('Optimisation flag created for Hypertension (incorporating longitudinal BP targets).')
  return(d)
}

# Chronic Kidney Disease Optimisation ----

# Patient optimised if diagnosed with CKD AND:
# - Elevated ACR (T2D & ACR >= 3, or No T2D & ACR >= 22.6):
#   Taking ACEi OR ARB + SGLT2i + Statin (or documented clinical exceptions)
# - Normal / Mild ACR (T2D & ACR < 3, or No T2D & ACR < 22.6):
#   Per NICE NG203 (1.6.1) & CG182, ACEi/ARB and SGLT2i are not indicated for
#   renal protection in low ACR; patient is optimised if taking Statin (or statin exception).

optimised_ckd <- function(ptts_dt){
  d = ptts_dt
  d <- d %>%
    mutate(
      ckd_exist = ifelse(is.na(ckd_diagnosis_code_term), 0, 1),
      
      # Determine if clinical drug requirements are met or medically excluded
      acei_arb_ok = (ac_ei_course_status == "Current" | arb_course_status == "Current" |
                       acei_excluded == 1 | arb_excluded == 1),
      sglt2i_ok = (sglt2i_course_status == "Current" | sglt2i_excluded == 1),
      statin_ok = (statins_first_issue_medication_courses_course_status_current_past_etc == "Current" | statin_excluded == 1),
      
      # ACR categorization per NICE guidelines
      has_t2d_ckd = !is.na(type_2_diabetes_diagnosis_code_term),
      ckd_high_acr = fifelse(
        ckd_exist == 1 & !is.na(acr_value) & (
          (has_t2d_ckd & acr_value >= 3) | (!has_t2d_ckd & acr_value >= 22.6)
        ), 1, 0
      ),
      ckd_low_acr = fifelse(
        ckd_exist == 1 & !is.na(acr_value) & (
          (has_t2d_ckd & acr_value < 3) | (!has_t2d_ckd & acr_value < 22.6)
        ), 1, 0
      )
    ) %>%
    mutate(
      ckd_optimised = case_when(
        # High ACR: Requires ACEi/ARB + SGLT2i + Statin
        ckd_exist == 1 & ckd_high_acr == 1 & acei_arb_ok & sglt2i_ok & statin_ok ~ 1,
        
        # Normal / Mild ACR: Does NOT require ACEi/ARB or SGLT2i; requires Statin (NICE NG203 1.6.1)
        ckd_exist == 1 & ckd_low_acr == 1 & statin_ok ~ 1,
        
        ckd_exist == 0 ~ NA_real_,
        TRUE ~ 0
      )
    )
  message('Optimisation flag created for CKD (incorporating exceptions and normal/mild ACR).')
  return(d)
}

# Diabetes Optimisation ----

# Patient optimised if diagnosed with Type 2 Diabetes AND:
# NO frailty (blank) & taking Metformin + SGLT2i & HbA1c <= 58 (incorporating exceptions)
# YES frailty & taking Metformin & HbA1c <= 75 (incorporating exceptions)

optimised_t2d <- function(ptts_dt){
  
  d = ptts_dt
  
  ## Start code here ##
  d <- d %>%
    # Checking Diabetes exists and medication status/exceptions
    mutate(
      diabetes_exist = ifelse(is.na(type_2_diabetes_diagnosis_code_term), 0, 1),
      metformin_ok = (metformin_rx_first_issue_course_status == "Current" | diabetic_med_excluded == 1),
      sglt2i_ok = (sglt2i_course_status == "Current" | sglt2i_excluded == 1)
    ) %>%
    mutate(
      diabetes_optimised = case_when(
        # Non-frail: Requires Current Metformin + SGLT2i (or valid exceptions) and HbA1c <= 58
        diabetes_exist == 1 &
          is.na(moderate_or_severe_frailty_code_term) &
          metformin_ok & sglt2i_ok & hb_a1c_value <= 58 ~ 1,
        
        # Frail: Relaxed target, requires Current Metformin (or valid exception) and HbA1c <= 75
        diabetes_exist == 1 &
          !is.na(moderate_or_severe_frailty_code_term) &
          metformin_ok & hb_a1c_value <= 75 ~ 1,
        
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
      # Hypertension Optimisation Date: Use first in-range date if available, fallback to latest BP date
      opt_date_htn = if_else(
        hypertension_exist == 1 & hypertension_optimised == 1,
        if_else(!is.na(bp_first_in_range_date), bp_first_in_range_date,
                if_else(!is.na(home_systolic_bp_latest_date), home_systolic_bp_latest_date, clinic_bp_latest_date)),
        as.Date(NA)
      ),
      
      # CKD Optimisation Date:
      # - High ACR: Max issue date across required meds (ACEi/ARB, SGLT2i, Statin)
      # - Low/Mild ACR: Max of statin initiation date and ACR lab test date
      # Capped at dx_date_ckd so pre-diagnostic treatments do not cause negative turnaround days.
      opt_date_ckd_raw = case_when(
        ckd_exist == 1 & ckd_optimised == 1 & ckd_high_acr == 1 ~ pmax(
          pmin(ac_ei_first_issue_date, arb_first_issue_date, na.rm = TRUE),
          sglt2i_first_issue_date,
          statins_first_issue_date_of_issue,
          na.rm = TRUE
        ),
        ckd_exist == 1 & ckd_optimised == 1 & ckd_low_acr == 1 ~ pmax(
          statins_first_issue_date_of_issue,
          acr_latest_date,
          na.rm = TRUE
        ),
        TRUE ~ as.Date(NA)
      ),
      
      opt_date_ckd = if_else(
        ckd_exist == 1 & ckd_optimised == 1,
        pmax(dx_date_ckd, opt_date_ckd_raw, na.rm = TRUE),
        as.Date(NA)
      ),
      
      # Diabetes Optimisation Date: Max of metformin, sglt2i (if non-frail), and latest HbA1c date
      opt_date_t2d_raw = if_else(
        diabetes_exist == 1 & diabetes_optimised == 1,
        if_else(
          !is.na(moderate_or_severe_frailty_code_term),
          pmax(metformin_rx_first_issue_date, hb_a1c_latest_date, na.rm = TRUE),
          pmax(metformin_rx_first_issue_date, sglt2i_first_issue_date, hb_a1c_latest_date, na.rm = TRUE)
        ),
        as.Date(NA)
      ),
      opt_date_t2d = if_else(
        diabetes_exist == 1 & diabetes_optimised == 1,
        pmax(dx_date_t2d, opt_date_t2d_raw, na.rm = TRUE),
        as.Date(NA)
      )
    )
  
  d$overall_optimisation_date <- do.call(pmax, c(d[, c("opt_date_htn", "opt_date_ckd", "opt_date_t2d")], na.rm = TRUE))
  
  d <- d %>%
    mutate(
      # Count active conditions and count optimized conditions
      num_conditions = coalesce(hypertension_exist, 0) + 
                       coalesce(ckd_exist, 0) + 
                       coalesce(diabetes_exist, 0),
      
      num_optimised = coalesce(hypertension_optimised, 0) + 
                      coalesce(ckd_optimised, 0) + 
                      coalesce(diabetes_optimised, 0),
      
      # Binary flags
      fully_optimised = if_else(num_conditions > 0 & num_optimised == num_conditions, 1, 0),
      partially_optimised = if_else(num_conditions > 0 & num_optimised > 0 & num_optimised < num_conditions, 1, 0),
      not_optimised = if_else(num_conditions > 0 & num_optimised == 0, 1, 0),
      
      # Categorical Status
      optimisation_status = case_when(
        num_conditions == 0 ~ NA_character_,
        num_optimised == num_conditions ~ "Fully Optimised",
        num_optimised > 0 & num_optimised < num_conditions ~ "Partially Optimised",
        num_optimised == 0 ~ "Not Optimised"
      ),
      
      time_to_optimisation_days = if_else(
        fully_optimised == 1,
        pmax(0, as.numeric(overall_optimisation_date - most_recent_dx_date)),
        as.numeric(NA)
      )
    )
  message('Created optimisation dates and category flags.')
  return(d)
}




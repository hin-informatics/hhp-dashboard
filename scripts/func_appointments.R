# Appointment Prescriptions ----
# Standardizes and merges historical prescription and drug exception data
appt_prescriptions <- function(appt_dt, ptts_dt){
  
  setDT(appt_dt)
  setDT(ptts_dt)
  
  # Forward-fill patient identifiers down repeating appointment/medication event rows
  appt_dt <- appt_dt %>% tidyr::fill(emis_number, .direction = "down")
  setDT(appt_dt)
  
  appt_dt <- setorder(appt_dt, emis_number, gp_appointments_5_years_appointment_date)
  
  # 1. Extract Statin prescriptions
  statins <- appt_dt[, .(
    statins_first_issue_name_dosage_and_quantity = last(na.omit(statins_first_issue_name_dosage_and_quantity)),
    statins_first_issue_date_of_issue = first(na.omit(statins_first_issue_date_of_issue)),
    statins_first_issue_medication_courses_most_recent_issue_date_in_course = last(na.omit(statins_first_issue_medication_courses_most_recent_issue_date_in_course)),
    statins_first_issue_medication_courses_course_status_current_past_etc = last(na.omit(statins_first_issue_medication_courses_course_status_current_past_etc))
  ), by = 'emis_number']
  
  ptts_dt <- merge(ptts_dt, statins, all.x = T, by = 'emis_number')
  
  # 2. Extract drug intolerance/exclusion exceptions if the columns exist
  exception_cols <- c("ace_exceptions_all_code_term", "arb_exceptions_all_code_term",
                      "sglt_2_exceptions_all_code_term", "sglt2_exceptions_all_code_term",
                      "lipid_lowering_therapy_exceptions_all_code_term",
                      "diabetic_medication_exceptions_all_code_term")
  
  existing_exceptions <- intersect(exception_cols, names(appt_dt))
  
  if (length(existing_exceptions) > 0) {
    # Check if a patient has any recorded exception in the appointment database
    exceptions <- appt_dt[, .(
      acei_excluded = ifelse("ace_exceptions_all_code_term" %in% names(appt_dt) && any(!is.na(ace_exceptions_all_code_term)), 1, 0),
      arb_excluded = ifelse("arb_exceptions_all_code_term" %in% names(appt_dt) && any(!is.na(arb_exceptions_all_code_term)), 1, 0),
      sglt2i_excluded = ifelse(
        ("sglt_2_exceptions_all_code_term" %in% names(appt_dt) && any(!is.na(sglt_2_exceptions_all_code_term))) ||
          ("sglt2_exceptions_all_code_term" %in% names(appt_dt) && any(!is.na(sglt2_exceptions_all_code_term))), 1, 0
      ),
      statin_excluded = ifelse("lipid_lowering_therapy_exceptions_all_code_term" %in% names(appt_dt) && any(!is.na(lipid_lowering_therapy_exceptions_all_code_term)), 1, 0),
      diabetic_med_excluded = ifelse("diabetic_medication_exceptions_all_code_term" %in% names(appt_dt) && any(!is.na(diabetic_medication_exceptions_all_code_term)), 1, 0)
    ), by = 'emis_number']
    
    ptts_dt <- merge(ptts_dt, exceptions, all.x = T, by = 'emis_number')
    
    # Fill NAs in exceptions to 0
    exc_cols <- c("acei_excluded", "arb_excluded", "sglt2i_excluded", "statin_excluded", "diabetic_med_excluded")
    for (col in exc_cols) {
      if (col %in% names(ptts_dt)) {
        ptts_dt[is.na(get(col)), (col) := 0]
      }
    }
  } else {
    # Fallback to 0 if columns aren't found in raw data
    ptts_dt[, c("acei_excluded", "arb_excluded", "sglt2i_excluded", "statin_excluded", "diabetic_med_excluded") := 0]
  }
  
  message('Appointment prescription & exception fields added.')
  return(ptts_dt)
}

# Appointment Calculated Fields ----
# Counts patient appointments in the turnaround window and adds them as columns to ptts_dt
appt_calculations <- function(appt_dt, ptts_dt){
  
  setDT(appt_dt)
  setDT(ptts_dt)
  
  # Forward-fill patient identifiers down repeating appointment event rows
  appt_dt <- appt_dt %>% tidyr::fill(emis_number, .direction = "down")
  setDT(appt_dt)
  
  # 1. Melt and standardise raw appointments from the appointment-level dataset
  # Exclude non-attended appointments (DNA, Cancelled)
  gp <- appt_dt[
    !is.na(gp_appointments_5_years_appointment_date) &
      !(trimws(gp_appointments_5_years_current_slot_status) %in% c("DNA", "Cancelled")),
    .(emis_number, appt_date = gp_appointments_5_years_appointment_date, clinician_type = "GP")
  ]
  
  nurse <- appt_dt[
    !is.na(nurse_appointments_5_years_appointment_date) &
      !(trimws(nurse_appointments_5_years_current_slot_status) %in% c("DNA", "Cancelled")),
    .(emis_number, appt_date = nurse_appointments_5_years_appointment_date, clinician_type = "Nurse")
  ]
  
  hca <- appt_dt[
    !is.na(hca_appointments_5_years_appointment_date) &
      !(trimws(hca_appointments_5_years_current_slot_status) %in% c("DNA", "Cancelled")),
    .(emis_number, appt_date = hca_appointments_5_years_appointment_date, clinician_type = "HCA")
  ]
  
  pharm <- appt_dt[
    !is.na(pharmacist_appointments_5_years_appointment_date) &
      !(trimws(pharmacist_appointments_5_years_current_slot_status) %in% c("DNA", "Cancelled")),
    .(emis_number, appt_date = pharmacist_appointments_5_years_appointment_date, clinician_type = "Pharmacist")
  ]
  
  # Union all clinician types into one long table
  all_appts <- rbindlist(list(gp, nurse, hca, pharm), use.names = TRUE, fill = TRUE)
  
  # 2. Merge diagnostic and optimization dates onto the appointment table
  merged_appts <- merge(
    all_appts,
    ptts_dt[, .(emis_number, fully_optimised, most_recent_dx_date, overall_optimisation_date)],
    by = "emis_number",
    all.x = TRUE
  )
  
  # 3. Filter appointments that fall strictly inside the individual turnaround window
  appts_in_window <- merged_appts[
    fully_optimised == 1 &
      !is.na(most_recent_dx_date) & !is.na(overall_optimisation_date) &
      appt_date >= most_recent_dx_date & appt_date <= overall_optimisation_date
  ]
  
  # 4. Count appointments per patient by clinician type
  counts_gp <- appts_in_window[clinician_type == "GP", .(appts_gp_in_window = .N), by = emis_number]
  counts_nurse <- appts_in_window[clinician_type == "Nurse", .(appts_nurse_in_window = .N), by = emis_number]
  counts_hca <- appts_in_window[clinician_type == "HCA", .(appts_hca_in_window = .N), by = emis_number]
  counts_pharm <- appts_in_window[clinician_type == "Pharmacist", .(appts_pharm_in_window = .N), by = emis_number]
  
  # 5. Merge counts back to the patient list
  ptts_dt <- merge(ptts_dt, counts_gp, by = "emis_number", all.x = TRUE)
  ptts_dt <- merge(ptts_dt, counts_nurse, by = "emis_number", all.x = TRUE)
  ptts_dt <- merge(ptts_dt, counts_hca, by = "emis_number", all.x = TRUE)
  ptts_dt <- merge(ptts_dt, counts_pharm, by = "emis_number", all.x = TRUE)
  
  # Replace NA counts with 0 for fully optimised patients
  appt_cols <- c("appts_gp_in_window", "appts_nurse_in_window", "appts_hca_in_window", "appts_pharm_in_window")
  for (col in appt_cols) {
    set(ptts_dt, i = which(ptts_dt$fully_optimised == 1 & is.na(ptts_dt[[col]])), j = col, value = 0)
  }
  
  # Calculate total appointments in window for fully optimised patients (keep NA for non-optimised)
  ptts_dt[fully_optimised == 1, appts_total_in_window := rowSums(.SD, na.rm = TRUE), .SDcols = appt_cols]
  ptts_dt[fully_optimised == 0, appts_total_in_window := as.numeric(NA)]
  
  message('Appointment calculated fields.')
  return(ptts_dt)
}


###################
## DO NOT MODIFY ##
###################
options(warn = 2)

rm(list = ls())

source('scripts/ini.R')

# Pipeline Settings
TestMode <- F
SourceMode <- "practices" # Options: "practices" (local multi-practice extracts) or "sharepoint"

tic('ETL Process complete')

# EXTRACTION ----
if (SourceMode == "practices") {
  message('Extracting, validating, and appending data from practice folders.')
  practice_data <- extract_and_append_practices(practices_dir = "practices")
  
  ptts_dt <- practice_data$ptts_dt
  appt_dt <- practice_data$appt_dt
  base_dt <- practice_data$base_dt
  
} else if (SourceMode == "sharepoint") {
  # Step 1: Extract data from shared location
  message('Getting and cleaning data from sharepoint.')
  
  # 1. Access SharePoint
  site <- get_sharepoint_site(
    site_url = "https://emckclac.sharepoint.com/sites/LSMhini",
    tenant = "emckclac.onmicrosoft.com"
  )
  
  drv <- site$get_drive("Informatics sensitive data") 
  
  # 2. Grab the file items from SharePoint
  file_item1 <- drv$get_item("Healthy Hearts/Healthy Hearts Evaluation Report 1 - patient level data.csv")
  file_item2 <- drv$get_item("Healthy Hearts/Healthy Hearts Evaluation Report 2 - appt data.csv")
  
  # 3. Create secure local temporary file paths
  temp_patient <- tempfile(fileext = ".csv")
  temp_appoint <- tempfile(fileext = ".csv")
  
  # 4. Stream the raw files down to your local machine memory
  file_item1$download(dest = temp_patient)
  file_item2$download(dest = temp_appoint)
  
  # 5. FAST LOADING & CLEANING WITH data.table
  # 'skip = "emis_number"' automatically drops rows 1-8 and reads row 9 as the true header.
  # 'data.table = TRUE' converts them to data.tables instantly.
  
  ptts_dt <- fread(temp_patient, skip = "EMIS Number", data.table = TRUE, select = 1:85, na.strings = c("", "NA"))
  appt_dt <- fread(temp_appoint, skip = "EMIS Number", data.table = TRUE, select = 1:80, na.strings = c("", "NA"))
  
  # 6. Securely delete the temporary files from your disk memory
  unlink(temp_patient)
  unlink(temp_appoint)
  
  message('Data successfully loaded and cleaned via reference skipping.')
  
  headers <- read.csv('data/headers.csv')
  
  patient_names <- remove_empty_names(headers$patient_data)
  appt_names <- remove_empty_names(headers$appt_data)
  
  names(ptts_dt) <- patient_names
  names(appt_dt) <- appt_names
} else {
  stop("Invalid SourceMode specified. Choose 'practices' or 'sharepoint'.")
}

ptts_dt <- clean_names(ptts_dt)
appt_dt <- clean_names(appt_dt)
base_dt <- clean_names(base_dt)

# Numeric and date columns

ptts_dt <- handle_date_num_vars(ptts_dt)
appt_dt <- handle_date_num_vars(appt_dt)
base_dt <- handle_date_num_vars(base_dt)


# TRANSFORMATION PIPELINE ----
# 1. Cohort filters
ptts_dt <- ptts_dt %>% filter(
  age >= 18 # Patients 18 and over only
)

# 2. Function transformation
ptts_dt <- appt_prescriptions(appt_dt, ptts_dt) # Step 1

ptts_dt <- create_age_bands(ptts_dt) # Step 2
ptts_dt <- create_ethnic_grps(ptts_dt) # Step 3
ptts_dt <- create_geo_grps(ptts_dt) # Step 4

ptts_dt <- optimised_htn(ptts_dt, base_dt) # Step 5
ptts_dt <- optimised_ckd(ptts_dt) # Step 6
ptts_dt <- optimised_t2d(ptts_dt) # Step 7
ptts_dt <- optimised_all(ptts_dt) # Step 8

ptts_dt <- assign_cvrm_cohort(ptts_dt) # Step 9

ptts_dt <- optimisation_date(ptts_dt) # Step 10

ptts_dt <- appt_calculations(appt_dt, ptts_dt) # Step 11

# LOAD ----
ptts_dt[, emis_number := as.character(emis_number)]

if(TestMode){
  message("TestMode is set to 'T': No changes made to the output data.")
}else{
  
  use_cols <- c(
    "emis_number"
    ,"organisation_name"
    ,"cvrm_cohort"
    ,"gender"
    ,'func_age_bands'
    ,'func_ethnic_group'
    ,'la_name'
    ,'lower_layer_area_2011'
    ,"imd_quintile_label"
    ,"diabetes_exist"
    ,"hypertension_exist"
    ,"ckd_exist"
    ,"diabetes_optimised"
    ,"hypertension_optimised"
    ,"ckd_optimised"
    ,"fully_optimised"
    ,"partially_optimised"
    ,"not_optimised"
    ,"optimisation_status"
    ,"time_to_optimisation_days"
    ,"appts_gp_in_window"
    ,"appts_nurse_in_window"
    ,"appts_hca_in_window"
    ,"appts_pharm_in_window"
    ,"appts_total_in_window"
    ,"bp_first_in_range_date"
    ,"bp_post_target_total_readings"
    ,"bp_post_target_in_range_readings"
    ,"bp_in_range_ratio"
    ,"bp_latest_date"
    ,"bp_latest_systolic"
    ,"bp_latest_diastolic"
  )
  
  payload <- ptts_dt[, ..use_cols]
  
  skim(payload)
  
  write.csv(payload, 'data/facts.csv', row.names = FALSE, na = "")
  
  if (SourceMode == "sharepoint") {
    drv$upload_file(
      src = 'data/facts.csv',
      dest = "Healthy Hearts/facts.csv"
    )
    message('Data loaded in SharePoint (', ncol(payload), ' columns)')
  } else {
    message('Data saved locally to data/facts.csv (', ncol(payload), ' columns)')
  }
}



toc()

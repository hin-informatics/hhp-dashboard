###################
## DO NOT MODIFY ##
###################
options(warn = 2)

rm(list = ls())

source('scripts/ini.R')

# Pipeline Settings
TestMode <- F

tic('ETL Process complete')

# EXTRACTION ----
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

d0 <- fread(temp_patient, skip = "EMIS Number", data.table = TRUE, select = 1:85, na.strings = c("", "NA"))
d1 <- fread(temp_appoint, skip = "EMIS Number", data.table = TRUE, select = 1:80, na.strings = c("", "NA"))

# 6. Securely delete the temporary files from your disk memory
unlink(temp_patient)
unlink(temp_appoint)

message('Data successfully loaded and cleaned via reference skipping.')

headers <- read.csv('data/headers.csv')

patient_names <- remove_empty_names(headers$patient_data)
appt_names <- remove_empty_names(headers$appt_data)

names(d0) <- patient_names
names(d1) <- appt_names

d0 <- clean_names(d0)
d1 <- clean_names(d1)

# Numeric and date columns

d0 <- handle_date_num_vars(d0)
d1 <- handle_date_num_vars(d1)

dt <- d0

# TRANSFORMATION PIPELINE ----
# 1. Cohort filters
dt <- dt %>% filter(
  age >= 18 # Patients 18 and over only
)

# 2. Appointment data enrichments
dt <- appt_prescriptions(dt)

# 3. Other transformation

dt <- create_age_bands(dt)
dt <- create_ethnic_grps(dt)
dt <- create_geo_grps(dt)

dt <- optimised_htn(dt)
dt <- optimised_ckd(dt)
dt <- optimised_t2d(dt)
dt <- optimised_all(dt)


# LOAD ----

dt[, emis_number := as.character(emis_number)]

if(TestMode){
  message("TestMode is set to 'T': No changes made to the output data.")
}else{
  
  use_cols <- c(
    "emis_number"
    ,"organisation_name"
    ,"diabetes_exist"
    ,"hypertension_exist"
    ,"ckd_exist"
    
  )
  
  payload <- dt[, ..use_cols]
  
  write.csv(payload, 'data/facts.csv', row.names = F)
  
  drv$upload_file(
    src = 'data/facts.csv',
    dest = "Healthy Hearts/facts.csv"
    )
  message('Data loaded in SharePoint')
}



toc()

###################
## DO NOT MODIFY ##
###################

source('scripts/ini.R')

# Pipeline Settings
TestMode <- F

tic('ETL Process complete')

# EXTRACTION ----
# Step 1: Extract data from shared location
message('Getting and cleaning data from sharepoint.')
# Access SharePoint
site <- get_sharepoint_site(
  site_url = "https://emckclac.sharepoint.com/sites/LSMhini",
  tenant = "emckclac.onmicrosoft.com"
)

drv <- site$get_drive("Informatics sensitive data") # Get drive
drv$list_files(path = "Healthy Hearts") # list files in Healthy Heart folder
file_item <- drv$get_item("Healthy Hearts/Healthy Hearts Evaluation Report 1 - patient level data.csv")

extract <- file_item$load_dataframe()

# CLEANING
# Rename Headers
d <- extract[-c(1:9), ]
headers <- read.csv('data/headers.csv')
names(d) <- headers$patient_data

d <- clean_names(d)

# Numeric and date columns
d <- d %>% mutate(across(contains("value"), as.numeric))

d <- d %>% mutate(across(
  contains("date"), 
  ~ suppressWarnings(parse_date_time(.x, orders = c("dmy", "ymd", "mdy", "my", "y"))) %>% as_date()
))

dt <- d

# TRANSFORMATION PIPELINE ----
# 1. Cohort filters

dt <- dt %>% filter(
  age >= 18 # Patients 18 and over only
)

# 2. Column transformation

dt <- create_age_bands(dt)
dt <- create_ethnic_grps(dt)
dt <- create_geo_grps(dt)

dt <- optimised_htn(dt)
dt <- optimised_ckd(dt)
dt <- optimised_t2d(dt)
dt <- optimised_all(dt)

# LOAD ----
if(TestMode){
  message("TestMode is set to 'T': No changes made to the output data.")
}else{
  
  write.csv(dt, 'data/patients_data.csv', row.names = F)
  
  drv$upload_file(
    src = 'data/patients_data.csv',
    dest = "Healthy Hearts/clean_data_patients.csv"
    )
  message('Data loaded in SharePoint')
}



toc()

###################
## DO NOT MODIFY ##
###################

source('scripts/ini.R')

# Pipeline Settings
TestMode <- F
DataRefresh <- T

tic('ETL Process complete')

# EXTRACTION ----
# Step 1: Extract data from shared location
if(DataRefresh == T){
  message('DataRefresh set to "T": Fresh data pull.')
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
  
  saveRDS(d, paste0('data/wd_',format(Sys.Date(), "%Y%m%d"))) # Save Working data set
}else{
  message('DataRefresh set to "F": Using latest archived data source.')
  file_list <- list.files('data/', pattern = "^wd_", full.names = T)
  file_metadata <- file.info(file_list)
  latest_data <- file_list[which.max(file_metadata$mtime)]
  d <- readRDS(latest_data)
}

dt <- d

# TRANSFORMATION PIPELINE ----
# Cohort filters

dt <- dt %>% filter(
  age >= 18 # Patients 18 and over only
)

# 2. Column transformation

dt <- create_age_bands(dt)
dt <- create_ethnic_grps(dt)
dt <- create_geo_grps(dt)

# LOAD ----
if(TestMode){
  message("TestMode is set to 'T': No changes made to the output data.")
}
write.csv(dt, 'data/clean.csv')

toc()
